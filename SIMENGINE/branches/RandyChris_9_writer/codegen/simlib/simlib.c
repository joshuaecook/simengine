#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<dlfcn.h>
#include <assert.h>
#include <zlib.h>

#ifdef DARWIN
#include<mach-o/loader.h>
#include<mach-o/nlist.h>
#else
#include<elf.h>
#endif

#define NUM_SYMBOLS 2 // FNAME_beg and FNAME_end
#define BLOCK_SIZE 16384

const void* SELF = NULL;
static void *binary_handle = NULL;

const char beg_sfx[] = "_beg";
const char end_sfx[] = "_end";
const int sfx_len = 4;
const char padding[] = "\0\0\0\0\0\0\0\0";

int open_binary(const char *binary){
  binary_handle = dlopen(binary, RTLD_LAZY);
  return binary_handle != NULL;
}

void close_binary(){
  if(binary_handle){
    dlclose(binary_handle);
    binary_handle = NULL;
  }
}

inline int round_to_word(int offset){
  if(sizeof(void*) == 4)
    return (offset & 0x3) ? offset - (offset & 0x3) + 0x4 : offset;
  else
    return (offset & 0x7) ? offset - (offset & 0x7) + 0x8 : offset;
}

inline int pad_len(int offset){
  if(sizeof(void*) == 4)
    return (0x4 - (offset & 0x3)) & 0x3;
  else
    return (0x8 - (offset & 0x7)) & 0x7;
}

void fix_fname(char *fname){
  int i;
  int len = strlen(fname);

  for(i=0;i<len;i++){
    if((fname[i] < 'a' || fname[i] > 'z') &&
       (fname[i] < 'A' || fname[i] > 'Z') &&
       (fname[i] < '0' || fname[i] > '9')){
      fname[i] = '_';
    }
  }
}
/* Compresses the data passed in via source to a buffer created as dest */
int compress_data(unsigned char *source, int ssize, unsigned char ** dest, int *dsize){
  int ret;
  z_stream strm;

  int bsize = ssize;
  *dsize = 0;

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;

  ret = deflateInit(&strm, Z_DEFAULT_COMPRESSION);
  if(ret != Z_OK)
    return ret;

  *dest = (unsigned char *)malloc(bsize);

  strm.avail_in = ssize;
  strm.next_in = source;
  strm.avail_out = bsize;
  strm.next_out = *dest;

  /* Make sure all input data gets compressed */
  while(strm.avail_in){
    /* make sure all compressed data gets stored */
    do{
      strm.avail_out = bsize - *dsize;
      ret = deflate(&strm, Z_FINISH);
      assert(ret != Z_STREAM_ERROR);
      *dsize = bsize - strm.avail_out;
      /* Reallocate if output buffer is full */
      if(strm.avail_out == 0){
	bsize += 1024;
	*dest = (unsigned char *)realloc(*dest, bsize);
	strm.next_out = &((*dest)[*dsize]);
      }
    }while(strm.avail_out == 0);
  }

  /* Clean up and return */
  deflateEnd(&strm);
  return Z_OK;
}

int decompress_data(unsigned char *source, int ssize, unsigned char **dest, int *dsize)
{
    int ret;
    z_stream strm;
    int bsize = ((ssize/1024)+1) * 1024 * 4;
    *dsize = 0;

    /* allocate inflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;

    ret = inflateInit(&strm);
    if (ret != Z_OK)
        return ret;

    *dest = (unsigned char *)malloc(bsize);

    strm.avail_in = ssize;
    strm.next_in = source;
    strm.avail_out = bsize;
    strm.next_out = *dest;

    /* decompress until deflate stream ends or end of file */
    while(strm.avail_in){
        /* run inflate() on input until output buffer not full */
        do {
	  strm.avail_out = bsize - *dsize;
            ret = inflate(&strm, Z_NO_FLUSH);
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            switch (ret) {
            case Z_NEED_DICT:
                ret = Z_DATA_ERROR;     /* and fall through */
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                (void)inflateEnd(&strm);
                return ret;
            }
            *dsize = bsize - strm.avail_out;
	    if(strm.avail_out == 0){
	      bsize += 1024;
	      *dest = (unsigned char*)realloc(*dest,bsize);
	      strm.next_out = &((*dest)[*dsize]);
	    }
        } while (strm.avail_out == 0);
    }

    /* clean up and return */
    inflateEnd(&strm);

    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

/* report a zlib or i/o error */
void zerr(int ret)
{
    fputs("simlib: ", stderr);
    switch (ret) {
    case Z_ERRNO:
        if (ferror(stdin))
            fputs("error reading stdin\n", stderr);
        if (ferror(stdout))
            fputs("error writing stdout\n", stderr);
        break;
    case Z_STREAM_ERROR:
        fputs("invalid compression level\n", stderr);
        break;
    case Z_DATA_ERROR:
        fputs("invalid or incomplete deflate data\n", stderr);
        break;
    case Z_MEM_ERROR:
        fputs("out of memory\n", stderr);
        break;
    case Z_VERSION_ERROR:
        fputs("zlib version mismatch!\n", stderr);
    }
}

// Returns the length of the data stream and sets data to point to the head
unsigned long extract_binary_file(const char *fname, unsigned char **data){
  char *fname_fixed;
  unsigned char *sym_beg;
  unsigned char *sym_end;
  unsigned long dsize;
  int len;

  // Dynamic library must already be open
  if(!binary_handle){
    *data = NULL;
    return 0;
  }

  len = strlen(fname);
  // Allocate strings
  fname_fixed = (char*)malloc(len+1);
  sym_beg = (unsigned char*)malloc(len+sfx_len+1); // '_' will be prepended by dlsym() in Mach-O
  sym_end = (unsigned char*)malloc(len+sfx_len+1); // '_' will be prepended by dlsym() in Mach-O

  // Manipulate filename for symbols
  strcpy(fname_fixed, fname);
  fix_fname(fname_fixed);
  strcpy(sym_beg, fname_fixed);
  strcat(sym_beg, beg_sfx);
  strcpy(sym_end, fname_fixed);
  strcat(sym_end, end_sfx);

  // Retrieve data from dynamic library
  *data = (unsigned char*)dlsym(binary_handle, sym_beg);
  dsize = (unsigned long)(dlsym(binary_handle, sym_end) - (void*)*data);

  free(fname_fixed);
  free(sym_beg);
  free(sym_end);

  return dsize;
}

#ifdef DARWIN
char *write_mach_o(char *fname, int dsize, char *data){
  // Mach-O structures
  struct mach_header mh;
  struct segment_command segc;
  //struct segment_command_64 segc64;
  struct section sec;
  //struct section_64 sec64;
  struct symtab_command symc;
  struct dysymtab_command dsymc;
  struct nlist nl[NUM_SYMBOLS];
  // Other vars
  FILE *objfile;
  char *fname_fixed;
  char *ofname;
  char *sym_beg;
  char *sym_end;
  int len = strlen(fname);

  // Mach-o Header
  mh.magic = MH_MAGIC;
  mh.cputype = CPU_TYPE_I386;
  //mh.cputype = CPU_TYPE_X86_64;
  mh.cpusubtype = CPU_SUBTYPE_I386_ALL;
  mh.filetype = MH_OBJECT;
  mh.ncmds = 3;
  mh.sizeofcmds = 228;
  mh.flags = 0;

  // Data Segment/Section 32bit
  // Segment
  segc.cmd = LC_SEGMENT;
  segc.cmdsize = sizeof(segc) + sizeof(sec);
  segc.segname[0] = 0; // Intermediate objects have only one segment with no name
  segc.vmaddr = 0;
  segc.vmsize = round_to_word(dsize); // GENERATED
  segc.fileoff = 256; // Beginning of contents after header and load commands
  segc.filesize = segc.vmsize; // GENERATED
  segc.maxprot = 1; // Read-only
  segc.initprot = 1; // Read-only
  segc.nsects = 1;
  segc.flags = 0;
  // Section
  strcpy(sec.sectname, "__const");
  strcpy(sec.segname, "__DATA");
  sec.addr = 0;
  sec.size = dsize; //GENERATED
  sec.offset = 256;
  sec.align = 0;
  sec.reloff = segc.vmsize + sec.offset; //GENERATED
  sec.nreloc = 0;
  sec.flags = 0;
  sec.reserved1 = 0;
  sec.reserved2 = 0;

  // Symbol Table
  symc.cmd = LC_SYMTAB;
  symc.cmdsize = sizeof(symc);
  symc.symoff = sec.reloff; // GENERATED
  symc.nsyms = NUM_SYMBOLS;
  symc.stroff = symc.symoff + symc.nsyms * sizeof(struct nlist); // GENERATED
  symc.strsize = NUM_SYMBOLS*(len + sfx_len + 1); // GENERATED

  // Dynamic Symbol Table
  dsymc.cmd = LC_DYSYMTAB;
  dsymc.cmdsize = sizeof(dsymc);
  dsymc.ilocalsym = 0;
  dsymc.nlocalsym = 0;
  dsymc.iextdefsym = 0;
  dsymc.nextdefsym = NUM_SYMBOLS;
  dsymc.iundefsym = NUM_SYMBOLS;
  dsymc.nundefsym = 0;
  dsymc.tocoff = 0;
  dsymc.ntoc = 0;
  dsymc.modtaboff = 0;
  dsymc.nmodtab = 0;
  dsymc.extrefsymoff = 0;
  dsymc.nextrefsyms = 0;
  dsymc.indirectsymoff = 0;
  dsymc.nindirectsyms = 0;
  dsymc.extreloff = 0;
  dsymc.nextrel = 0;
  dsymc.locreloff = 0;
  dsymc.nlocrel = 0;

  // Symbol Table entries
  nl[0].n_un.n_strx = 0; // Character offset into the string table
  nl[0].n_type = N_EXT | N_SECT;
  nl[0].n_sect = 1;
  nl[0].n_desc = REFERENCE_FLAG_DEFINED | REFERENCED_DYNAMICALLY | N_NO_DEAD_STRIP;
  nl[0].n_value = 0; // Beginning addr symbol (beginning of section, will be relocated on link/load)

  nl[1].n_un.n_strx = len+sfx_len+2; // Character offset into the string table
  nl[1].n_type = N_EXT | N_SECT;
  nl[1].n_sect = 1;
  nl[1].n_desc = REFERENCE_FLAG_DEFINED | REFERENCED_DYNAMICALLY | N_NO_DEAD_STRIP;
  nl[1].n_value = dsize; // Ending addr symbol (end of section, will be relocated on link/load)

  // Allocate strings
  fname_fixed = (char*)malloc(len+1);
  ofname = (char*)malloc(len+3);
  sym_beg = (char*)malloc(len+sfx_len+2);
  sym_end = (char*)malloc(len+sfx_len+2);

  // Manipulate filename for output filename and symbols
  strcpy(fname_fixed, fname);
  fix_fname(fname_fixed);
  strcpy(ofname, fname_fixed);
  strcat(ofname, ".o");
  strcpy(sym_beg, "_"); // Mach-O dlsym() adds an '_' when performing lookup
  strcat(sym_beg, fname_fixed);
  strcat(sym_beg, beg_sfx);
  strcpy(sym_end, "_"); // Mach-O dlsym() adds an '_' when performing lookup
  strcat(sym_end, fname_fixed);
  strcat(sym_end, end_sfx);

  // Write the Mach-O object
  objfile = fopen(ofname, "w");
  fwrite(&mh, sizeof(mh), 1, objfile);
  fwrite(&segc, sizeof(segc), 1, objfile);
  fwrite(&sec, sizeof(sec), 1, objfile);
  fwrite(&symc, sizeof(symc), 1, objfile);
  fwrite(&dsymc, sizeof(dsymc), 1, objfile);
  fwrite(data, dsize, 1, objfile);
  fwrite(padding, pad_len(dsize), 1, objfile); // Pad data to word alignment
  fwrite(nl, NUM_SYMBOLS*sizeof(struct nlist), 1, objfile);
  fwrite(sym_beg, strlen(sym_beg) + 1, 1, objfile);
  fwrite(sym_end, strlen(sym_end) + 1, 1, objfile);
  fclose(objfile);

  // Free string allocations
  free(fname_fixed);
  free(sym_beg);
  free(sym_end);

  return ofname; // Must be freed externally
}

#else // #ifdef DARWIN

char *write_elf(char *fname, int dsize, char *data){
  Elf32_Ehdr eh;
  //Elf64_Ehdr eh64;
  Elf32_Shdr sh[5];
  //const char shstrtab[] = "\0.symtab\0.strtab\0.shstrtab\0.data\0";
  const char shstrtab[] = "\0.data\0.shstrtab\0.symtab\0.strtab\0\0\0\0";
  Elf32_Sym sym[NUM_SYMBOLS + 2]; // empty and SECTION symbols
  // Other vars
  FILE *objfile;
  char *fname_fixed;
  char *ofname;
  char *sym_beg;
  char *sym_end;
  int len = strlen(fname);

  // Allocate strings
  fname_fixed = (char*)malloc(len+1);
  ofname = (char*)malloc(len+3);
  sym_beg = (char*)malloc(len+sfx_len+1);
  sym_end = (char*)malloc(len+sfx_len+1);

  // Manipulate filename for output filename and symbols
  strcpy(fname_fixed, fname);
  fix_fname(fname_fixed);
  strcpy(ofname, fname_fixed);
  strcat(ofname, ".o");
  strcpy(sym_beg, fname_fixed);
  strcat(sym_beg, beg_sfx);
  strcpy(sym_end, fname_fixed);
  strcat(sym_end, end_sfx);

  bzero(&eh, sizeof(eh));
  eh.e_ident[EI_MAG0] = ELFMAG0;
  eh.e_ident[EI_MAG1] = ELFMAG1;
  eh.e_ident[EI_MAG2] = ELFMAG2;
  eh.e_ident[EI_MAG3] = ELFMAG3;
  eh.e_ident[EI_CLASS] = ELFCLASS32;
  //eh.e_ident[EI_CLASS] = ELFCLASS64;
  eh.e_ident[EI_DATA] = ELFDATA2LSB;
  eh.e_ident[EI_VERSION] = EV_CURRENT;
  
  eh.e_type = ET_REL;
  eh.e_machine = EM_386;
  //eh.e_machine = EM_X86_64;
  eh.e_version = EV_CURRENT;
  //eh.e_shoff = 0; // GENERATED below
  eh.e_ehsize = sizeof(eh);
  eh.e_shentsize = sizeof(Elf32_Shdr);
  eh.e_shnum = 5;
  eh.e_shstrndx = 2;

  bzero(sh, 5*sizeof(Elf32_Shdr));
  sh[0].sh_type = SHT_NULL;

  sh[1].sh_name = strlen(&shstrtab[sh[0].sh_name])+sh[0].sh_name+1;
  sh[1].sh_type = SHT_PROGBITS;
  sh[1].sh_flags = SHF_ALLOC; // Memory mapped, but not writeable
  sh[1].sh_offset = eh.e_ehsize;
  sh[1].sh_size = round_to_word(dsize); // GENERATED
  sh[1].sh_addralign = 4;

  sh[2].sh_name = strlen(&shstrtab[sh[1].sh_name])+sh[1].sh_name+1;
  sh[2].sh_type = SHT_STRTAB;
  sh[2].sh_offset = sh[1].sh_offset + sh[1].sh_size; // GENERATED
  sh[2].sh_size = 36;
  sh[2].sh_addralign = 4;

  eh.e_shoff = round_to_word(sh[2].sh_offset + sh[2].sh_size); // GENERATED

  sh[3].sh_name = strlen(&shstrtab[sh[2].sh_name])+sh[2].sh_name+1;
  sh[3].sh_type = SHT_SYMTAB;
  sh[3].sh_offset = eh.e_shoff + eh.e_shentsize * eh.e_shnum;
  sh[3].sh_size = (NUM_SYMBOLS + 2) * sizeof(Elf32_Sym);
  sh[3].sh_link = 4;
  sh[3].sh_info = 2;
  sh[3].sh_addralign = 4;
  sh[3].sh_entsize = sizeof(Elf32_Sym);

  sh[4].sh_name = strlen(&shstrtab[sh[3].sh_name])+sh[3].sh_name+1;
  sh[4].sh_type = SHT_STRTAB;
  sh[4].sh_offset = sh[3].sh_offset + sh[3].sh_size;
  sh[4].sh_size = strlen(sym_beg) + strlen(sym_end) + 3; // GENERATED
  sh[4].sh_addralign = 1;

  bzero(sym, (NUM_SYMBOLS + 2)*sizeof(Elf32_Sym));
  sym[1].st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
  sym[1].st_shndx = 1;

  sym[2].st_name = 1;
  sym[2].st_info = ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE);
  sym[2].st_shndx = 1;

  sym[3].st_name = strlen(sym_beg)+2;
  sym[3].st_value = dsize;
  sym[3].st_info = ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE);
  sym[3].st_shndx = 1;

  // Write the ELF object
  objfile = fopen(ofname, "w");
  fwrite(&eh, sizeof(eh), 1, objfile);
  fwrite(data, sh[1].sh_size, 1, objfile);
  //fwrite(padding, pad_len(sh[1].sh_size), 1, objfile); // Pad data to word alignment
  fwrite(shstrtab, sh[2].sh_size, 1, objfile);
  //fwrite(padding, pad_len(sh[2].sh_size), 1, objfile); // Pad string table to word alignment
  fwrite(sh, eh.e_shnum*eh.e_shentsize, 1, objfile);
  fwrite(sym, sh[3].sh_size, 1, objfile);
  fwrite(padding, 1, 1, objfile); // Empty string at beginning of strtab
  fwrite(sym_beg, strlen(sym_beg)+1, 1, objfile);
  fwrite(sym_end, strlen(sym_end)+1, 1, objfile);
  fclose(objfile);

  // Free string allocations
  free(fname_fixed);
  free(sym_beg);
  free(sym_end);

  return ofname; // Must be freed externally
}

#endif // #ifdef DARWIN

unsigned long read_binary_file(const char *fname, unsigned char **data){
  FILE *bfile;
  int alloc_size = BLOCK_SIZE;
  unsigned long dsize = 0;
  unsigned char *tmp;

  *data = NULL;

  bfile = fopen(fname, "r");
  if(!bfile) return 0;

  // Allocate space for data
  *data = (unsigned char*)malloc(alloc_size);

  while(1){
    dsize += fread(&((*data)[dsize]), 1, alloc_size - dsize, bfile);
    if(dsize != alloc_size){
      // Finished reading
      break;
    }
    else{
      // Resize buffer to read next block
      alloc_size += BLOCK_SIZE;
      tmp = (unsigned char*)realloc(*data, alloc_size);
      if(!tmp){
	// Resize failed
	free(*data);
	*data = NULL;
	return 0;
      }
      else
	*data = tmp;
    }
  }

  fclose(bfile);

  return dsize;
}

void write_obj_files(int numfiles, char **fnames){
  int i;
  unsigned char *data, *cdata;
  int dsize, cdsize;
  char *ofname;

  for(i=0; i<numfiles; i++){
    dsize = read_binary_file(fnames[i], &data);
    if(dsize){
      compress_data(data,dsize,&cdata,&cdsize);
#ifdef DARWIN
      ofname = write_mach_o(fnames[i], cdsize, cdata);
#else
      ofname = write_elf(fnames[i], cdsize, cdata);
#endif
      printf("%s\n", ofname);
      free(ofname);
      free(cdata);
    }
    if(data)
      free(data);
  }
}

void sim_extract_files(char *library, int numfiles, char **fnames){
  int i;
  unsigned char *cdata, *data;
  int cdsize, dsize;

  if(!open_binary(library))
    return;

  // Extract all symbols
  for(i=0;i<numfiles;i++){
    cdsize = extract_binary_file(fnames[i], &cdata);
    if(cdsize){
      decompress_data(cdata, cdsize, &data, &dsize);
      fwrite(data, 1, dsize, stdout);
      free(data);
    }
  }
  // cdata need not be freed as it points to constant memory in the library
  close_binary();

}

// This program takes the following parameters on the command line
// usage: simlib SiMagic put <file1> [file2 ... fileN]
//        simlib SiMagic get <shared object> <file1> [file2 ... fileN]
int main(int argc, char** argv){
  unsigned long size;
  void *data;
  int i;

  // Silently ignore attempts to execute with the wrong parameters
  if(argc < 4) // too few parameters
    return 0;
  if(strcmp(argv[1],"SiMagic")) // first parameter must be Magical
    return 0;
  if(0==strcmp(argv[2],"put")){
    write_obj_files(argc - 3, &argv[3]);
  }
  if(0==strcmp(argv[2],"get")){
    if(argc <5) // too few parameters
      return 0;
    sim_extract_files(argv[3], argc - 4, &argv[4]);
  }

  return 0;
}
