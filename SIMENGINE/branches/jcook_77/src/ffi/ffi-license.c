/* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. */

#define KEYLEN 256
#define B64LEN 512

int encode(int plen, unsigned char *ptext, int *clen, unsigned char *ctext);
int decode(int clen, unsigned char *ctext, int *plen, unsigned char *ptext);

// SML ffi interface to license encoding/decoding
#if !defined(TEST_ENCODE) && !defined(TEST_DECODE)
#include "ffi-exports.h"

Int32_t license_Decode (Int32_t clen, String8_t ctext);

Int32_t license_Decode (Int32_t clen, String8_t ctext) {
  int status;
  char plaintext[KEYLEN];
  int length = 0;

  status = decode(clen, ctext, &length, plaintext);
  if (0 == status) {
    make_the_string(length, plaintext);
  }
  return status;
}

Int32_t license_Encode (Int32_t plen, String8_t ptext) {
  int status;
  char ciphertext[B64LEN];
  int length = 0;

  status = encode(plen, ptext, &length, ciphertext);
  if(0 == status) {
    make_the_string(length, ciphertext);
  }
  return status;
}
#endif // !defined(TEST_ENCODE) && !defined(TEST_DECODE)

#include<openssl/pem.h>
#include<openssl/rsa.h>

static void base64encode(int plen, unsigned char *ptext, int *clen, unsigned char *ctext){
  BIO *mem;
  BIO *b64;

  // Create Basic IO chain with a memory and base64 encoder/decoder
  mem = BIO_new(BIO_s_mem());
  b64 = BIO_new(BIO_f_base64());
  BIO_push(b64, mem);

  // Write unencoded data through bas64 encoder to memory
  BIO_write(b64, ptext, plen);
  BIO_flush(b64);

  // Read the encoded data from memory
  *clen = BIO_read(mem, ctext, B64LEN);

  // Free BIO resources
  BIO_free(mem);
  BIO_free(b64);
}

static void base64decode(int clen, unsigned char *ctext, int *plen, unsigned char *ptext){
  BIO *mem;
  BIO *b64;

  // Create Basic IO chain with a memory and base64 encoder/decoder
  mem = BIO_new(BIO_s_mem());
  b64 = BIO_new(BIO_f_base64());
  BIO_push(b64, mem);

  // Write base64 encoded data to memory
  BIO_write(mem,ctext, clen);
  
  // Read decoded data from base64 decoder
  *plen = BIO_read(b64, ptext, KEYLEN);

  // Free BIO resources
  BIO_free(mem);
  BIO_free(b64);
}


// RSA encrypting of a block of data in ptext into ctext
// This code assumes the RSA private key is embedded in PEM format in 
// the executable via simlib as 'simatra_private'
int encode(int plen, unsigned char *ptext, int *clen, unsigned char *ctext){
  // Do not include the encoding code within simEngine, only within the key generation utility
#ifdef ENABLE_ENCODE
  RSA *rsa_priv;
  BIO *mem;
  BIO *b64;
  char *keydata;
  int keylen;
  unsigned char tmp_ctext[KEYLEN];
  int tmp_clen;

  // Check for valid sizes and existence of buffers
  if(plen < 0 || plen > KEYLEN || NULL == ctext || NULL == ptext){
    return 1;
  }

  // Read in the RSA private key
  if(get_contents_from_archive("", "simatra_private", &keylen, &keydata)){
    return 2;
  }
  mem = BIO_new_mem_buf(keydata, keylen);
  rsa_priv = PEM_read_bio_RSAPrivateKey(mem, NULL, NULL, NULL);
  BIO_free(mem);
  if(NULL == rsa_priv){
    return 3;
  }

  // Encrypt the plain text into cipher text
  tmp_clen = RSA_private_encrypt(plen, ptext, tmp_ctext, rsa_priv, RSA_PKCS1_PADDING);
  if(tmp_clen < 1)
    return 4;

  // Convert the encrypted license data to base64 encoding
  base64encode(tmp_clen, tmp_ctext, clen, ctext);

  return 0;
#else
  return 5;
#endif // ENABLE_ENCODE
}

// RSA decrypting of a block of data in ctext into ptext
// This code assumes the RSA public key is embedded in PEM format in 
// the executable via simlib as 'simatra'
int decode(int clen, unsigned char *ctext, int *plen, unsigned char *ptext){
  RSA *rsa_pub;
  BIO *mem;
  char *keydata;
  int keylen;
  int tmp_clen;
  unsigned char tmp_ctext[KEYLEN];

  // Check for valid sizes and existence of buffers
  if(clen < 0 || clen > B64LEN || NULL == ctext || NULL == ptext){
    return 1;
  }

  // Read in the RSA public key
  if(get_contents_from_archive("", "simatra", &keylen, &keydata)){
    return 2;
  }
  mem = BIO_new_mem_buf(keydata, keylen);
  rsa_pub = PEM_read_bio_RSA_PUBKEY(mem, NULL, NULL, NULL);
  BIO_free(mem);
  if(NULL == rsa_pub){
    return 3;
  }

  // Convert the base64 encoded license to the raw encrypted cipher text form
  base64decode(clen, ctext, &tmp_clen, tmp_ctext);

  // Decrypt the cipher text into plain text
  *plen = RSA_public_decrypt(tmp_clen, tmp_ctext, ptext, rsa_pub, RSA_PKCS1_PADDING);
  if(*plen < 1)
    return 4;

  return 0;
}

// Standalone main() functions for testing purposes
#ifdef TEST_ENCODE
int main(int argc, char **argv){
  FILE *ptextfile;
  FILE *ctextfile;
  int plen;
  int clen;
  unsigned char ptext[KEYLEN];
  unsigned char ctext[2*KEYLEN];
  int err;

  if(argc != 3){
    printf("usage: %s <ptextfile> <ctextfile>\n", argv[0]);
    return 1;
  }

  // Open the plain text and cipher text files
  ptextfile = fopen(argv[1], "r");
  if(NULL == ptextfile){
    printf("Error opening %s\n", argv[1]);
    return 1;
  }
  ctextfile = fopen(argv[2], "w");
  if(NULL == ctextfile){
    printf("Error opening %s\n", argv[2]);
    return 1;
  }

  // Read in the plain text
  plen = fread(ptext, 1, sizeof(ptext), ptextfile);
  if(plen < 1){
    printf("Error reading from %s.\n", argv[1]);
    return 1;
  }
  fclose(ptextfile);

  // Encode
  err = encode(plen, ptext, &clen, ctext);
  if(err){
    printf("Error %d in encoding.\n", err);
    return 1;
  }

  // Write the cipher text to file
  if(clen != fwrite(ctext, 1, clen, ctextfile)){
    printf("Error writing to %s.\n", argv[2]);
    return 1;
  }
  fclose(ctextfile);
  printf("Done.\n");
}
#endif // TEST_ENCODE

#ifdef TEST_DECODE
int main(int argc, char **argv){
  FILE *ptextfile;
  FILE *ctextfile;
  int plen;
  int clen;
  unsigned char ptext[KEYLEN];
  unsigned char ctext[2*KEYLEN];
  int err;

  if(argc != 3){
    printf("usage: %s <ctextfile> <ptextfile>\n", argv[0]);
    return 1;
  }

  // Open the plain text and cipher text files
  ctextfile = fopen(argv[1], "r");
  if(NULL == ctextfile){
    printf("Error opening %s\n", argv[1]);
    return 1;
  }
  ptextfile = fopen(argv[2], "w");
  if(NULL == ptextfile){
    printf("Error opening %s\n", argv[2]);
    return 1;
  }

  // Read in the cipher text
  clen = fread(ctext, 1, sizeof(ctext), ctextfile);
  if(clen < 1){
    printf("Error reading from %s.\n", argv[1]);
    return 1;
  }
  fclose(ctextfile);

  // Decode
  err = decode(clen, ctext, &plen, ptext);
  if(err){
    printf("Error %d in decoding.\n", err);
    return 1;
  }

  // Write the plain text to file
  if(plen != fwrite(ptext, 1, plen, ptextfile)){
    printf("Error writing to %s.\n", argv[2]);
    return 1;
  }
  fclose(ptextfile);
  printf("Done.\n");
}
#endif // TEST_DECODE
