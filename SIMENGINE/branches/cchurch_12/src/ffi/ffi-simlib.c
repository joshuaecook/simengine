/* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. */

#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* The header should be generated by MLton.
 * It shall declare MLton's own data types
 * as well as the signatures of all
 * exported functions (e.g. those use to
 * allocate memory for arrays.) */
#include "ffi-exports.h"

Int32_t simlib_MakeObjectFromContents (String8_t objectName, Int32_t length, String8_t contents);
Int32_t simlib_MakeObjectFromFile (String8_t objectName, String8_t filename);
Int32_t simlib_GetContentsFromArchive (String8_t archiveName, String8_t objectName);
Int32_t simlib_GetFileFromArchive (String8_t archiveName, String8_t objectName, String8_t filename);

/* A vector of 8-bit words is appropriate for MLton's string type. */
static Vector(Word8_t) gcallocVector8 (size_t length, Word8_t *defaults)
    {
    int i;
    Word8_t *words = (Word8_t *)heap_alloc_word8(length, 0);

    memcpy(words, defaults, length);
    /*** Why does this fault when the memcpy() above works fine? ***/
    /* for (i = 0; i < length; i++) */
    /* 	heap_update_word8(words, i, defaults[i]); */
    return words;
    }

Int32_t simlib_errno (void)
    {
    return errno;
    }

void simlib_strerror (Int32_t errnum)
    {
    char *message = strerror(errnum);
    make_the_string(strlen(message), message);
    //return gcallocVector8(strlen(message), message);
    }

Int32_t simlib_MakeObjectFromContents (String8_t objectName, Int32_t length, String8_t contents)
    {
    int status;
    char *ofn = NULL;
    size_t ofn_len;
    
    status = make_object_from_contents(objectName, length, contents, &ofn);
    if (0 == status && NULL != ofn)
	{
	make_the_string(strlen(ofn), ofn);
	//ofn_len = strlen(ofn);
	//*((char **)objectFilename) = gcallocVector8(ofn_len, ofn);
	}
    if (ofn) free(ofn);

    return status;
    }

Int32_t simlib_MakeObjectFromFile (String8_t objectName, String8_t filename)
    {
    int status;
    size_t length;
    char *ofn = NULL;

    status = make_object_from_file(objectName, filename, &ofn);
    if (0 == status && NULL != ofn)
	{
	make_the_string(strlen(ofn), ofn);
	//length = strlen(ofn);
	//*((char **)objectFilename) = gcallocVector8(length, ofn);
	}
    if (ofn) free(ofn);

    return status;
    }

Int32_t simlib_GetContentsFromArchive (String8_t archiveName, String8_t objectName)
    {
    int status;
    char *data = NULL;
    int length = 0;
    status = get_contents_from_archive(archiveName, objectName, &length, &data);
    if (0 == status && NULL != data)
	{
	make_the_string(length, data);
	//*((char **)contents) = gcallocVector8(length, data);
	}
    if (data) free(data);
    
    return status;
    }

Int32_t simlib_GetFileFromArchive (String8_t archiveName, String8_t objectName, String8_t filename)
    {
    return get_file_from_archive(archiveName, objectName, filename);
    }
