#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<dlfcn.h>
#include<assert.h>
#include<zlib.h>

// Architecture specific includes
#ifdef __APPLE__
#include<mach-o/loader.h>
#include<mach-o/nlist.h>
#else
#include<elf.h>
#endif

// Architecture specific routine macro
#ifdef __APPLE__
//#ifdef __X86_64__
//#define WRITE_OBJ write_mach_o64
//#else
#define WRITE_OBJ write_mach_o32
//#endif
#else
#ifdef __X86_64__
#define WRITE_OBJ write_elf64
#else
#define WRITE_OBJ write_elf32
#endif
#endif

enum{ SUCCESS, ERR_OUT_OF_MEMORY, ERR_FOPEN, ERR_DLOPEN, ERR_NO_OBJECT, 
      ERR_COMPRESS, ERR_DECOMPRESS};

// simlib API functions
int make_object_from_contents(// Inputs
			      const char *object_name,
			      const int length,
			      char *contents,
			      // Outputs
			      char **object_file_name);
int make_object_from_file(// Inputs
			  const char *object_name,
			  const char *file_name,
			  // Outputs
			  char **object_file_name);
int get_contents_from_archive(// Inputs
			      const char *archive_name,
			      const char *object_name,
			      // Outputs
			      int *length,
			      char **contents);
int get_file_from_archive(// Inputs
			  const char *archive_name,
			  const char *object_name,
			  const char *file_name);


#define NUM_SYMBOLS 2 // FNAME_beg and FNAME_end
#define BLOCK_SIZE 16384

static const void* SELF = NULL;
static void *binary_handle = NULL;

static const char beg_sfx[] = "_beg";
static const char end_sfx[] = "_end";
static const int sfx_len = 4;
static const char padding[] = "\0\0\0\0\0\0\0\0";

static int open_binary(const char *binary){
  binary_handle = dlopen(binary, RTLD_LAZY);
  if(NULL == binary_handle)
    return ERR_DLOPEN;
  else
    return SUCCESS;
}

static void close_binary(){
  if(binary_handle){
    dlclose(binary_handle);
    binary_handle = NULL;
  }
}

static inline int round_to_word(const int offset){
  //if(4 == sizeof(void*))
  return (offset & 0x3) ? offset - (offset & 0x3) + 0x4 : offset;
  //else
  //return (offset & 0x7) ? offset - (offset & 0x7) + 0x8 : offset;
}

static inline int pad_len(const int offset){
  //if(sizeof(void*) == 4)
  return (0x4 - (offset & 0x3)) & 0x3;
  //else
  //return (0x8 - (offset & 0x7)) & 0x7;
}

/* Compresses the data passed in via source to a buffer created as dest */
static int compress_data(char *source, const int ssize, char ** dest, int *dsize){
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

  *dest = (char *)malloc(bsize);

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
	*dest = (char *)realloc(*dest, bsize);
	strm.next_out = &((*dest)[*dsize]);
      }
    }while(strm.avail_out == 0);
  }

  /* Clean up and return */
  deflateEnd(&strm);
  return Z_OK;
}

static int decompress_data(char *source, const int ssize, char **dest, int *dsize)
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

    *dest = (char *)malloc(bsize);

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
	      *dest = (char*)realloc(*dest,bsize);
	      strm.next_out = &((*dest)[*dsize]);
	    }
        } while (strm.avail_out == 0);
    }

    /* clean up and return */
    inflateEnd(&strm);

    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

/* report a zlib or i/o error */
static void zerr(const int ret)
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

#ifdef __APPLE__
static int write_mach_o32(FILE *objfile, const char *sym_beg, const char *sym_end, const int dsize, const char *data){
  // Mach-O structures
  struct mach_header mh;
  struct segment_command segc;
  //struct segment_command_64 segc64;
  struct section sec;
  //struct section_64 sec64;
  struct symtab_command symc;
  struct dysymtab_command dsymc;
  struct nlist nl[NUM_SYMBOLS];

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
  symc.strsize = NUM_SYMBOLS + strlen(sym_beg) + strlen(sym_end); // GENERATED

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

  nl[1].n_un.n_strx = strlen(sym_beg)+1; // Character offset into the string table
  nl[1].n_type = N_EXT | N_SECT;
  nl[1].n_sect = 1;
  nl[1].n_desc = REFERENCE_FLAG_DEFINED | REFERENCED_DYNAMICALLY | N_NO_DEAD_STRIP;
  nl[1].n_value = dsize; // Ending addr symbol (end of section, will be relocated on link/load)

  // Write the Mach-O object
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

  return SUCCESS; // Must be freed externally
}

static int write_mach_o64(FILE *objfile, const char *sym_beg, const char *sym_end, const int dsize, const char *data){
  // Mach-O structures
  struct mach_header_64 mh;
  struct segment_command_64 segc;
  struct section_64 sec;
  struct symtab_command symc;
  struct dysymtab_command dsymc;
  struct nlist_64 nl[NUM_SYMBOLS];

  // Mach-o Header
  mh.magic = MH_MAGIC_64;
  mh.cputype = CPU_TYPE_X86_64;
  mh.cpusubtype = CPU_SUBTYPE_X86_64_ALL;
  mh.filetype = MH_OBJECT;
  mh.ncmds = 3;
  mh.sizeofcmds = 256;
  mh.flags = 0;

  // Data Segment/Section 64bit
  // Segment
  segc.cmd = LC_SEGMENT_64;
  segc.cmdsize = sizeof(segc) + sizeof(sec);
  segc.segname[0] = 0; // Intermediate objects have only one segment with no name
  segc.vmaddr = 0;
  segc.vmsize = round_to_word(dsize); // GENERATED
  segc.fileoff = 288; // Beginning of contents after header and load commands
  segc.filesize = segc.vmsize; // GENERATED
  segc.maxprot = 1; // Read-only
  segc.initprot = 1; // Read-only
  segc.nsects = 1;
  segc.flags = 0;
  // Section
  strcpy(sec.sectname, "__const");
  strcpy(sec.segname, "__DATA");
  sec.addr = 0;
  sec.size = round_to_word(dsize); //GENERATED
  sec.offset = 288;
  sec.align = 2;
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
  symc.stroff = symc.symoff + symc.nsyms * sizeof(struct nlist_64); // GENERATED
  symc.strsize = NUM_SYMBOLS + strlen(sym_beg) + strlen(sym_end); // GENERATED

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

  nl[1].n_un.n_strx = strlen(sym_beg)+2; // Character offset into the string table
  nl[1].n_type = N_EXT | N_SECT;
  nl[1].n_sect = 1;
  nl[1].n_desc = REFERENCE_FLAG_DEFINED | REFERENCED_DYNAMICALLY | N_NO_DEAD_STRIP;
  nl[1].n_value = dsize; // Ending addr symbol (end of section, will be relocated on link/load)

  // Write the Mach-O object
  fwrite(&mh, sizeof(mh), 1, objfile);
  fwrite(&segc, sizeof(segc), 1, objfile);
  fwrite(&sec, sizeof(sec), 1, objfile);
  fwrite(&symc, sizeof(symc), 1, objfile);
  fwrite(&dsymc, sizeof(dsymc), 1, objfile);
  fwrite(data, dsize, 1, objfile);
  fwrite(padding, pad_len(dsize), 1, objfile); // Pad data to word alignment
  fwrite(nl, NUM_SYMBOLS*sizeof(struct nlist_64), 1, objfile);
  fwrite(sym_beg, strlen(sym_beg) + 1, 1, objfile);
  fwrite(sym_end, strlen(sym_end) + 1, 1, objfile);

  return SUCCESS; // Must be freed externally
}

#else // #ifdef __APPLE__

static int write_elf32(FILE *objfile, const char *sym_beg, const char *sym_end, const int dsize, const char *data){
  Elf32_Ehdr eh;
  Elf32_Shdr sh[5];
  const char shstrtab[] = "\0.data\0.shstrtab\0.symtab\0.strtab\0\0\0\0";
  Elf32_Sym sym[NUM_SYMBOLS + 2]; // empty and SECTION symbols

  bzero(&eh, sizeof(eh));
  eh.e_ident[EI_MAG0] = ELFMAG0;
  eh.e_ident[EI_MAG1] = ELFMAG1;
  eh.e_ident[EI_MAG2] = ELFMAG2;
  eh.e_ident[EI_MAG3] = ELFMAG3;
  eh.e_ident[EI_CLASS] = ELFCLASS32;
  eh.e_ident[EI_DATA] = ELFDATA2LSB;
  eh.e_ident[EI_VERSION] = EV_CURRENT;
  
  eh.e_type = ET_REL;
  eh.e_machine = EM_386;
  eh.e_version = EV_CURRENT;
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
  fwrite(&eh, sizeof(eh), 1, objfile);
  fwrite(data, sh[1].sh_size, 1, objfile);
  fwrite(shstrtab, sh[2].sh_size, 1, objfile);
  fwrite(sh, eh.e_shnum*eh.e_shentsize, 1, objfile);
  fwrite(sym, sh[3].sh_size, 1, objfile);
  fwrite(padding, 1, 1, objfile); // Empty string at beginning of strtab
  fwrite(sym_beg, strlen(sym_beg)+1, 1, objfile);
  fwrite(sym_end, strlen(sym_end)+1, 1, objfile);

  return SUCCESS;
}

static int write_elf64(FILE *objfile, const char *sym_beg, const char *sym_end, const int dsize, const char *data){
  Elf64_Ehdr eh;
  Elf64_Shdr sh[5];
  const char shstrtab[] = "\0.data\0.shstrtab\0.symtab\0.strtab\0\0\0\0";
  Elf64_Sym sym[NUM_SYMBOLS + 2]; // empty and SECTION symbols

  bzero(&eh, sizeof(eh));
  eh.e_ident[EI_MAG0] = ELFMAG0;
  eh.e_ident[EI_MAG1] = ELFMAG1;
  eh.e_ident[EI_MAG2] = ELFMAG2;
  eh.e_ident[EI_MAG3] = ELFMAG3;
  eh.e_ident[EI_CLASS] = ELFCLASS64;
  eh.e_ident[EI_DATA] = ELFDATA2LSB;
  eh.e_ident[EI_VERSION] = EV_CURRENT;
  
  eh.e_type = ET_REL;
  eh.e_machine = EM_X86_64;
  eh.e_version = EV_CURRENT;
  eh.e_ehsize = sizeof(eh);
  eh.e_shentsize = sizeof(Elf64_Shdr);
  eh.e_shnum = 5;
  eh.e_shstrndx = 2;

  bzero(sh, 5*sizeof(Elf64_Shdr));
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
  sh[3].sh_size = (NUM_SYMBOLS + 2) * sizeof(Elf64_Sym);
  sh[3].sh_link = 4;
  sh[3].sh_info = 2;
  sh[3].sh_addralign = 4;
  sh[3].sh_entsize = sizeof(Elf64_Sym);

  sh[4].sh_name = strlen(&shstrtab[sh[3].sh_name])+sh[3].sh_name+1;
  sh[4].sh_type = SHT_STRTAB;
  sh[4].sh_offset = sh[3].sh_offset + sh[3].sh_size;
  sh[4].sh_size = strlen(sym_beg) + strlen(sym_end) + 3; // GENERATED
  sh[4].sh_addralign = 1;

  bzero(sym, (NUM_SYMBOLS + 2)*sizeof(Elf64_Sym));
  sym[1].st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
  sym[1].st_shndx = 1;

  sym[2].st_name = 1;
  sym[2].st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
  sym[2].st_shndx = 1;

  sym[3].st_name = strlen(sym_beg)+2;
  sym[3].st_value = dsize;
  sym[3].st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
  sym[3].st_shndx = 1;

  // Write the ELF object
  fwrite(&eh, sizeof(eh), 1, objfile);
  fwrite(data, sh[1].sh_size, 1, objfile);
  fwrite(shstrtab, sh[2].sh_size, 1, objfile);
  fwrite(sh, eh.e_shnum*eh.e_shentsize, 1, objfile);
  fwrite(sym, sh[3].sh_size, 1, objfile);
  fwrite(padding, 1, 1, objfile); // Empty string at beginning of strtab
  fwrite(sym_beg, strlen(sym_beg)+1, 1, objfile);
  fwrite(sym_end, strlen(sym_end)+1, 1, objfile);

  return SUCCESS;
}
#endif // #ifdef __APPLE__

static int read_binary_file(FILE *bfile, int *length, char **data){
  int alloc_size = BLOCK_SIZE;
  char *tmp;
  *length = 0;

  *data = NULL;

  // Allocate space for data
  *data = (char*)malloc(alloc_size);
  if(NULL == *data){
    return ERR_OUT_OF_MEMORY;
  }

  while(1){
    *length += fread(&((*data)[*length]), 1, alloc_size - *length, bfile);
    if(*length != alloc_size){
      // Finished reading
      break;
    }
    else{
      // Resize buffer to read next block
      alloc_size += BLOCK_SIZE;
      tmp = (char*)realloc(*data, alloc_size);
      if(!tmp){
	// Resize failed
	free(*data);
	*data = NULL;
	return ERR_OUT_OF_MEMORY;
      }
      else
	*data = tmp;
    }
  }

  return SUCCESS;
}

static void fix_fname(char *fname){
  int i;
  int len = strlen(fname);
  FILE *bfile, *ofile;

  for(i=0;i<len;i++){
    if((fname[i] < 'a' || fname[i] > 'z') &&
       (fname[i] < 'A' || fname[i] > 'Z') &&
       (fname[i] < '0' || fname[i] > '9')){
      fname[i] = '_';
    }
  }
}

static int write_object(const char *object_name, const int cdsize, const char *cdata, char **object_file_name){
  int status;
  FILE *objfile;

  // Allocate strings
  int len = strlen(object_name);
  char *oname_fixed = (char*)malloc(len+1);
  char *sym_beg = (char*)malloc(len+sfx_len+2);
  char *sym_end = (char*)malloc(len+sfx_len+2);
  *object_file_name = (char*)malloc(len+3);

  if(NULL == oname_fixed ||
     NULL == *object_file_name ||
     NULL == sym_beg ||
     NULL == sym_end){
    return ERR_OUT_OF_MEMORY;
  }

  // Manipulate object name for output filename
  strcpy(oname_fixed, object_name);
  fix_fname(oname_fixed);
  strcpy(*object_file_name, oname_fixed);
  strcat(*object_file_name, ".o");

  objfile = fopen(*object_file_name, "w");
  if(NULL == objfile){
    return ERR_FOPEN;
  }

  sym_beg[0] = 0;
  sym_end[0] = 0;

#ifdef __APPLE__
  strcpy(sym_beg, "_"); // Mach-O dlsym() adds an '_' when performing lookup
  strcpy(sym_end, "_"); // Mach-O dlsym() adds an '_' when performing lookup
#endif

  // Manipulate object name for symbol names
  strcat(sym_beg, oname_fixed);
  strcat(sym_beg, beg_sfx);
  strcat(sym_end, oname_fixed);
  strcat(sym_end, end_sfx);   

  status = WRITE_OBJ(objfile, sym_beg, sym_end, cdsize, cdata);

  // Free string allocations
  free(oname_fixed);
  free(sym_beg);
  free(sym_end);

  fclose(objfile);

  return status;
}

// Returns the length of the data stream and sets data to point to the head
static int extract_binary_file(const char *fname, int *length, char **cdata){
  int status;
  char *fname_fixed;
  char *sym_beg;
  char *sym_end;
  int len;

  // Dynamic library must already be open
  if(NULL == binary_handle){
    *cdata = NULL;
    *length = 0;
    return ERR_DLOPEN;
  }

  len = strlen(fname);
  // Allocate strings
  fname_fixed = (char*)malloc(len+1);
  sym_beg = (char*)malloc(len+sfx_len+1); // '_' will be prepended by dlsym() in Mach-O
  sym_end = (char*)malloc(len+sfx_len+1); // '_' will be prepended by dlsym() in Mach-O

  if(NULL == fname_fixed ||
     NULL == sym_beg ||
     NULL == sym_end){
    return ERR_OUT_OF_MEMORY;
  }

  // Manipulate filename for symbols
  strcpy(fname_fixed, fname);
  fix_fname(fname_fixed);
  strcpy(sym_beg, fname_fixed);
  strcat(sym_beg, beg_sfx);
  strcpy(sym_end, fname_fixed);
  strcat(sym_end, end_sfx);

  // Retrieve data from dynamic library
  *cdata = (char*)dlsym(binary_handle, sym_beg);
  *length = (int)(dlsym(binary_handle, sym_end) - (void*)*cdata);

  free(fname_fixed);
  free(sym_beg);
  free(sym_end);

  return SUCCESS;
}

// simlib API functions
// *****************************************************************************
int make_object_from_contents(// Inputs
			      const char *object_name,
			      const int length,
			      char *contents,
			      // Outputs
			      char **object_file_name){
  int status;
  char *cdata;
  int cdsize;

  // Compress object contents
  status = compress_data(contents, length, &cdata, &cdsize);
  if(Z_OK != status){
    return ERR_COMPRESS;
  }

  // Write object contents to object file
  write_object(object_name, cdsize, cdata, object_file_name);

  // Free compressed contents
  free(cdata);

  return status;
}

int make_object_from_file_ptr(// Inputs
			      const char *object_name,
			      FILE *file_ptr,
			      // Outputs
			      char ** object_file_name){
  int status;
  int length;
  char *contents;

  status = read_binary_file(file_ptr, &length, &contents);
  if(SUCCESS != status){
    return status;
  }

  status = make_object_from_contents(object_name, length, contents, object_file_name);

  free(contents);

  return status;
}

int make_object_from_file(// Inputs
			  const char *object_name,
			  const char *file_name,
			  // Outputs
			  char **object_file_name){
  int status;
  FILE *file_ptr;

  file_ptr = fopen(file_name, "r");
  if(NULL == file_ptr){
    return ERR_FOPEN;
  }
  
  status = make_object_from_file_ptr(object_name, file_ptr, object_file_name);

  fclose(file_ptr);

  return status;
}

int get_contents_from_archive(// Inputs
			      const char *archive_name,
			      const char *object_name,
			      // Outputs
			      int *length,
			      char **contents){
  int status;
  // cdata need never be freed as it points to constant memory in the archive
  char *cdata;
  int cdsize;

  // Open the archive
  status = open_binary(archive_name);
  if(SUCCESS != status)
    return status;

  // Retrieve the compressed data from the archive
  status = extract_binary_file(object_name, &cdsize, &cdata);
  if(SUCCESS != status){
    close_binary();
    return status;
  }
  if(0 >= cdsize){
    close_binary();
    return ERR_NO_OBJECT;
  }

  // Decompress the data
  status = decompress_data(cdata, cdsize, contents, length);
  if(Z_OK != status){
    close_binary();
    return ERR_DECOMPRESS;
  }

  // Close the archive
  close_binary();

  return SUCCESS;
}

int get_file_from_archive(// Inputs
			  const char *archive_name,
			  const char *object_name,
			  const char *file_name){
  int status;
  FILE *outfile;
  int length;
  char *contents;

  // Open the output file
  outfile = fopen(file_name, "w");
  if(NULL == outfile)
    return ERR_FOPEN;

  // Get the file contents from the archive
  status = get_contents_from_archive(archive_name, object_name, &length, &contents);
  if(SUCCESS != status){
    return status;
  }
  // Write the contents to the file
  fwrite(contents, length, 1, outfile);
  // Close the file
  fclose(outfile);
  // Free the contents in memory
  free(contents);

  return SUCCESS;
}

//********************************************************************************

#ifdef SIMLIB_MAIN

void print_usage(const char *binary_name){
  fprintf(stderr, "usage:\t%s put <objectname> [filename]\n"
	  "\t%s get <archivename> <objectname> [filename]\n\n"
	  "\t objectname - name of the object in the archive\n"
	  "\t filename - optional filename to read/write (default stdin/stdout)\n\n",
	  binary_name, binary_name);
}

int main(int argc, char** argv){
  int status = SUCCESS;
  int size;
  void *data;
  int i;

  // Show usage if incorrect parameters are supplied
  if(argc < 3 || argc > 5){
    print_usage(argv[0]);
    return status;
  }

  // Create an object for an archive
  if(0==strcmp(argv[1],"put")){
    char *object_file_name;

    if(argc == 5){
      print_usage(argv[0]);
      return status;
    }
    // Read contents from stdin
    else if(argc == 3){
      status = make_object_from_file_ptr(argv[2], stdin, &object_file_name);
    }
    // Read contents from file
    else{
      status = make_object_from_file(argv[2], argv[3], &object_file_name);
    }

    if(SUCCESS == status){
      printf("%s\n", object_file_name);
      //free(object_file_name);
    }
  }

  // Retrieve an object from an archive
  else if(0==strcmp(argv[1],"get")){
    if(argc == 3){
      print_usage(argv[0]);
      return status;
    }
    // Write object to stdout
    else if(argc == 4){
      int length;
      char *contents;
      status = get_contents_from_archive(argv[2], argv[3], &length, &contents);
      if(SUCCESS == status){
	fwrite(contents, length, 1, stdout);
	free(contents);
      }
      else
	return status;
    }
    // Write object to file
    else{
      status = get_file_from_archive(argv[2], argv[3], argv[4]);
    }
  }

  return status;
}
#endif // #ifdef SIMLIB_MAIN
