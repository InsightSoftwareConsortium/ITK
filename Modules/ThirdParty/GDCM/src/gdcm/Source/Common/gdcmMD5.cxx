/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmMD5.h"
#include "gdcmSystem.h"

#ifdef GDCM_USE_SYSTEM_OPENSSL
#include <openssl/md5.h>
#elif defined(GDCM_BUILD_TESTING)
#include "gdcm_md5.h"
#endif
#include <fstream>
#include <vector>

// http://stackoverflow.com/questions/13256446/compute-md5-hash-value-by-c-winapi
namespace gdcm
{

bool MD5::Compute(const char *buffer, size_t buf_len, char digest_str[33])
{
  if( !buffer || !buf_len )
    {
    return false;
    }
#ifdef GDCM_USE_SYSTEM_OPENSSL
  unsigned char digest[16];
  MD5_CTX ctx;
  MD5_Init(&ctx);
  MD5_Update(&ctx, buffer, buf_len);
  MD5_Final(digest, &ctx);
  for (int di = 0; di < 16; ++di)
    sprintf(digest_str+2*di, "%02x", digest[di]);
  digest_str[2*16] = '\0';
  return true;
#elif defined(GDCM_BUILD_TESTING)
  md5_byte_t digest[16];
  md5_state_t state;
  md5_init(&state);
  md5_append(&state, (const md5_byte_t *)buffer, (int)buf_len);
  md5_finish(&state, digest);
  for (int di = 0; di < 16; ++di)
    sprintf(digest_str+2*di, "%02x", digest[di]);
  digest_str[2*16] = '\0';
  return true;
#else
  (void)digest_str;
  return false;
#endif
}

#ifdef GDCM_USE_SYSTEM_OPENSSL
static bool process_file(const char *filename, unsigned char *digest)
{
  if( !filename || !digest ) return false;

  std::ifstream file(filename, std::ios::binary);
  if(!file) return false;

  const size_t file_size = System::FileSize(filename);
  std::vector<char> v( file_size );

  char *buffer = &v[0];
  file.read(buffer, file_size);

  MD5_CTX ctx;
  MD5_Init(&ctx);
  MD5_Update(&ctx, buffer, file_size);
  MD5_Final(digest, &ctx);

  return true;
}
#elif defined(GDCM_BUILD_TESTING)
static bool process_file(const char *filename, md5_byte_t *digest)
{
  if( !filename || !digest ) return false;

  std::ifstream file(filename, std::ios::binary);
  if(!file) return false;

  const size_t file_size = System::FileSize(filename);
  std::vector<char> v( file_size );

  char *buffer = &v[0];
  file.read(buffer, file_size);

  md5_state_t state;
  md5_init(&state);
  md5_append(&state, (const md5_byte_t *)buffer, (int)file_size);
  md5_finish(&state, digest);

  return true;
}
#else
static inline bool process_file(const char *, unsigned char *)
{
  return false;
}
#endif

bool MD5::ComputeFile(const char *filename, char digest_str[33])
{
#ifdef GDCM_USE_SYSTEM_OPENSSL
  unsigned char digest[16];
#elif defined(GDCM_BUILD_TESTING)
  md5_byte_t digest[16];
#else
  unsigned char digest[16] = {};
#endif
  /* Do the file */
  if( !process_file(filename, digest) )
    {
    return false;
    }

  for (int di = 0; di < 16; ++di)
    {
    sprintf(digest_str+2*di, "%02x", digest[di]);
    }
  digest_str[2*16] = '\0';
  return true;
}


} // end namespace gdcm
