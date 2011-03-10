/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmBase64.h"

#ifdef GDCM_USE_SYSTEM_OPENSSL
// http://www.openssl.org/docs/crypto/BIO_f_base64.html
#include <openssl/sha.h>
#include <openssl/hmac.h>
#include <openssl/evp.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>
#endif

#if 0
#include "gdcm_polarssl.h"
#endif
#include <string.h>

/*
 */
namespace gdcm
{

class Base64Internals
{
public:
};

Base64::Base64()
{
  Internals = new Base64Internals;
}

Base64::~Base64()
{
  delete Internals;
}

int Base64::GetEncodeLength(const char *src, int slen )
{
#if 0
  int dlen = 0;
  int r = base64_encode( NULL, &dlen, (unsigned char*)(src), slen );
  if( r == POLARSSL_ERR_BASE64_INVALID_CHARACTER )
    return -1;
  return dlen;
#endif

#ifdef GDCM_USE_SYSTEM_OPENSSL
  BIO *bmem, *b64;
  BUF_MEM *bptr;

  b64 = BIO_new(BIO_f_base64());
  bmem = BIO_new(BIO_s_mem());
  b64 = BIO_push(b64, bmem);
  BIO_write(b64, src, slen);
  (void)BIO_flush(b64);
  BIO_get_mem_ptr(b64, &bptr);
  int dlen = bptr->length;
  BIO_free_all(b64);
  return dlen;
#else
  (void)src;
  (void)slen;
#endif
  return -1;
}

int Base64::Encode( char *dst, int dlen,
                   const char *src, int  slen )
{
#if 0
  return base64_encode( (unsigned char*)dst, &dlen, (unsigned char*)(src), slen );
#endif

#ifdef GDCM_USE_SYSTEM_OPENSSL
  // Create a memory buffer which will contain the Base64 encoded string
  BIO * mem = BIO_new(BIO_s_mem());

  // Push on a Base64 filter so that writing to the buffer encodes the data
  BIO * b64 = BIO_new(BIO_f_base64());
  BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
  mem = BIO_push(b64, mem);

  // Encode all the data
  BIO_write(mem, src, slen);
  (void)BIO_flush(mem);

  // Create a new string from the data in the memory buffer
  char * base64Pointer;
  long base64Length = BIO_get_mem_data(mem, &base64Pointer);
  if( base64Length > dlen ) return -1;

  memcpy( dst, base64Pointer, base64Length);

  // Clean up and go home
  BIO_free_all(mem);
  return 0;
#else
  (void)dst;
  (void)dlen;
  (void)src;
  (void)slen;
  return -1;
#endif
}

int Base64::GetDecodeLength( const char *src, int  slen )
{
#if 0
  int dlen = 0;
  int r = base64_decode( NULL, &dlen, (unsigned char*)(src), slen );
  if( r == POLARSSL_ERR_BASE64_INVALID_CHARACTER )
    return -1;
  return dlen;
#endif

#ifdef GDCM_USE_SYSTEM_OPENSSL
  // Create a memory buffer containing Base64 encoded string data
  BIO * mem = BIO_new_mem_buf((void *) src, slen);

  // Push a Base64 filter so that reading from the buffer decodes it
  BIO * b64 = BIO_new(BIO_f_base64());
  BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
  mem = BIO_push(b64, mem);

  // Decode
  char *dst = new char[slen]; // FIXME
  int len = BIO_read(mem, dst, slen);
  delete[] dst;

  // Clean up and go home
  BIO_free_all(mem);

  return len;
#else
  (void)src;
  (void)slen;
  return -1;
#endif
}
// http://www.openssl.org/docs/crypto/BIO_f_base64.html

int Base64::Decode( char *dst, int dlen,
                   const char *src, int  slen )
{
 //http://markmail.org/message/cdndl7pofs7maixq#query:+page:1+mid:cdndl7pofs7maixq+state:results
#if 0
  return base64_decode( (unsigned char*)dst, &dlen, (unsigned char*)(src), slen );
#endif

#ifdef GDCM_USE_SYSTEM_OPENSSL
  // Create a memory buffer containing Base64 encoded string data
  BIO * mem = BIO_new_mem_buf((void *) src, slen);

  // Push a Base64 filter so that reading from the buffer decodes it
  BIO * b64 = BIO_new(BIO_f_base64());
  BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
  mem = BIO_push(b64, mem);

  // Decode
  int len = BIO_read(mem, dst, dlen);
  assert( len < dlen );
  if( len < 0 )
    {
    return -1;
    }

  // Clean up and go home
  BIO_free_all(mem);
  return 0;
#else
  (void)dst;
  (void)dlen;
  (void)src;
  (void)slen;
  return -1;
#endif
}


//int Base64::SelfTest( int verbose ) const
//{
//  return base64_self_test( verbose );
//}


} // end namespace gdcm
