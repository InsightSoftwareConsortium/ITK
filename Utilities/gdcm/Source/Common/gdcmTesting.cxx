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
#include "gdcmTesting.h"
#include "gdcmFilename.h"
#include "gdcmSystem.h"
#include "gdcmMD5.h"

#include <string.h> // strcmp
#include <stdlib.h> // malloc


namespace gdcm
{

#ifndef GDCM_BUILD_TESTING
#error how did that happen ?
#endif

#include "gdcmDataFileNames.cxx"
#include "gdcmMD5DataImages.cxx"
#include "gdcmMediaStorageDataFiles.cxx"

bool Testing::ComputeMD5(const char *buffer, unsigned long buf_len,
  char digest_str[33])
{
  return MD5::Compute(buffer, buf_len, digest_str);
}

bool Testing::ComputeFileMD5(const char *filename, char *digest_str)
{
  return MD5::ComputeFile(filename, digest_str);
}

const char * const *Testing::GetFileNames()
{
  return gdcmDataFileNames;
}

unsigned int Testing::GetNumberOfFileNames()
{
  // Do not count NULL value:
  static const unsigned int size = sizeof(gdcmDataFileNames)/sizeof(*gdcmDataFileNames) - 1;
  return size;
}

const char * Testing::GetFileName(unsigned int file)
{
  if( file < Testing::GetNumberOfFileNames() ) return gdcmDataFileNames[file];
  return NULL;
}

Testing::MediaStorageDataFilesType Testing::GetMediaStorageDataFiles()
{
  return gdcmMediaStorageDataFiles;
}
unsigned int Testing::GetNumberOfMediaStorageDataFiles()
{
  // Do not count NULL value:
  static const unsigned int size = sizeof(gdcmMediaStorageDataFiles)/sizeof(*gdcmMediaStorageDataFiles) - 1;
  return size;
}
const char * const * Testing::GetMediaStorageDataFile(unsigned int file)
{
  if( file < Testing::GetNumberOfMediaStorageDataFiles() ) return gdcmMediaStorageDataFiles[file];
  // else return the {0x0, 0x0} sentinel:
  assert( *gdcmMediaStorageDataFiles[ Testing::GetNumberOfMediaStorageDataFiles() ] == 0 );
  return gdcmMediaStorageDataFiles[ Testing::GetNumberOfMediaStorageDataFiles() ];
}
const char * Testing::GetMediaStorageFromFile(const char *filepath)
{
  unsigned int i = 0;
  MediaStorageDataFilesType mediastorages = GetMediaStorageDataFiles();
  const char *p = mediastorages[i][0];
  Filename comp(filepath);
  const char *filename = comp.GetName();
  while( p != 0 )
    {
    if( strcmp( filename, p ) == 0 )
      {
      break;
      }
    ++i;
    p = mediastorages[i][0];
    }
  // \postcondition always valid (before sentinel)
  assert( i <= GetNumberOfMediaStorageDataFiles() );
  return mediastorages[i][1];
}


Testing::MD5DataImagesType Testing::GetMD5DataImages()
{
  return gdcmMD5DataImages;
}
unsigned int Testing::GetNumberOfMD5DataImages()
{
  // Do not count NULL value:
  static const unsigned int size = sizeof(gdcmMD5DataImages)/sizeof(*gdcmMD5DataImages) - 1;
  return size;
}

const char * const * Testing::GetMD5DataImage(unsigned int file)
{
  if( file < Testing::GetNumberOfMD5DataImages() ) return gdcmMD5DataImages[file];
  // else return the {0x0, 0x0} sentinel:
  assert( *gdcmMD5DataImages[ Testing::GetNumberOfMD5DataImages() ] == 0 );
  return gdcmMD5DataImages[ Testing::GetNumberOfMD5DataImages() ];
}

const char * Testing::GetMD5FromFile(const char *filepath)
{
  if(!filepath) return NULL;
  unsigned int i = 0;
  MD5DataImagesType md5s = GetMD5DataImages();
  const char *p = md5s[i][1];
  Filename comp(filepath);
  const char *filename = comp.GetName();
  while( p != 0 )
    {
    if( strcmp( filename, p ) == 0 )
      {
      break;
      }
    ++i;
    p = md5s[i][1];
    }
  // \postcondition always valid (before sentinel)
  assert( i <= GetNumberOfMD5DataImages() );
  return md5s[i][0];
}

const char *Testing::GetDataRoot()
{
  return GDCM_DATA_ROOT;
}

const char *Testing::GetDataExtraRoot()
{
  return GDCM_DATA_EXTRA_ROOT;
}

const char *Testing::GetPixelSpacingDataRoot()
{
  return GDCM_PIXEL_SPACING_DATA_ROOT;
}

const char *Testing::GetTempDirectory(const char * subdir)
{
  if( !subdir ) return GDCM_TEMP_DIRECTORY;
  // else
  static std::string buffer;
  std::string tmpdir = GDCM_TEMP_DIRECTORY;
  tmpdir += "/";
  tmpdir += subdir;
  buffer = tmpdir;
  return buffer.c_str();
}

const wchar_t *Testing::GetTempDirectoryW(const wchar_t * subdir)
{
  static std::wstring buffer;
  wchar_t wname[4096]; // FIXME
  size_t len = mbstowcs(wname,GDCM_TEMP_DIRECTORY,sizeof(wname));
  (void)len;
  if( !subdir )
    {
    buffer = wname;
    return buffer.c_str();
    }
  // else
  std::wstring tmpdir = wname;
  tmpdir += L"/";
  tmpdir += subdir;
  buffer = tmpdir;
  return buffer.c_str();
}

const char * Testing::GetTempFilename(const char *filename, const char * subdir)
{
  if( !filename ) return 0;

  static std::string buffer;
  std::string outfilename = GetTempDirectory(subdir);
  outfilename += "/";
  gdcm::Filename out(filename);
  outfilename += out.GetName();
  buffer = outfilename;

  return buffer.c_str();
}

void Testing::Print(std::ostream &os)
{
  os << "DataFileNames:\n";
  const char * const * filenames = gdcmDataFileNames;
  while( *filenames )
    {
    os << *filenames << "\n";
    ++filenames;
    }

  os << "MD5DataImages:\n";
  MD5DataImagesType md5s = gdcmMD5DataImages;
  while( (*md5s)[0] )
    {
    os << (*md5s)[0] << " -> " << (*md5s)[1] << "\n";
    ++md5s;
    }
}

const wchar_t* Testing::GetTempFilenameW(const wchar_t *filename, const wchar_t* subdir)
{
  // mbsrtowcs
  // mbstowcs
  if( !filename ) return 0;

  static std::wstring buffer;
  std::wstring outfilename = GetTempDirectoryW(subdir);
  outfilename += L"/";

  //gdcm::Filename out(filename);
  //outfilename += out.GetName();
  buffer = outfilename;
  buffer += filename;

  return buffer.c_str();
}

const char *Testing::GetSourceDirectory()
{
  return GDCM_SOURCE_DIR;
}

} // end of namespace gdcm
