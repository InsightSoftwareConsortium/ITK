/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDirList.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#include "gdcmDirList.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"

#include <iterator>
#include <assert.h>
#include <errno.h>
#include <sys/stat.h>  //stat function

#ifdef _MSC_VER
   #include <windows.h> 
   #include <direct.h>
#else
   #include <dirent.h>   
   #include <sys/types.h>
#endif

namespace gdcm
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor  
 * @param  dirName root directory name
 * @param  recursive whether we want to explore recursively or not 
 */
DirList::DirList(std::string const &dirName, bool recursive)
{
   DirName = dirName;
   Explore(dirName, recursive);
}

/**
 * \brief  Destructor
 */
DirList::~DirList()
{
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief Tells us if file name corresponds to a Directory   
 * @param  dirName file name to check
 * @return true if the file IS a Directory
 */
bool DirList::IsDirectory(std::string const &dirName)
{
   struct stat fs;
   assert( dirName[dirName.size()-1] != '/' );
   if ( stat(dirName.c_str(), &fs) == 0 )
   {
#if _WIN32
      return ((fs.st_mode & _S_IFDIR) != 0);
#else
      return S_ISDIR(fs.st_mode);
#endif
   }
   else
   {
      gdcmErrorMacro( strerror(errno) );
      return false;
   }
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private
/**
 * \brief   Explore a directory with possibility of recursion
 *          return number of files read
 * @param  dirpath   directory to explore
 * @param  recursive whether we want recursion or not
 */
int DirList::Explore(std::string const &dirpath, bool recursive)
{
   int numberOfFiles = 0;
   std::string fileName;
   std::string dirName = Util::NormalizePath(dirpath);
#ifdef _MSC_VER
   WIN32_FIND_DATA fileData;
   //assert( dirName[dirName.size()-1] == '' );
   HANDLE hFile = FindFirstFile((dirName+"*").c_str(), &fileData);

   for(BOOL b = (hFile != INVALID_HANDLE_VALUE); b;
       b = FindNextFile(hFile, &fileData))
   {
      fileName = fileData.cFileName;
      if ( fileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
      {
         // Need to check for . and .. to avoid infinite loop
         if ( fileName != "." && fileName != ".." && recursive )
         {
            numberOfFiles += Explore(dirName+fileName,recursive);
         }
      }
      else
      {
         Filenames.push_back(dirName+fileName);
         numberOfFiles++;
      }
   }
   DWORD dwError = GetLastError();
   if (hFile != INVALID_HANDLE_VALUE) 
      FindClose(hFile);
   if (dwError != ERROR_NO_MORE_FILES) 
   {
      LPVOID lpMsgBuf;
      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|
                    FORMAT_MESSAGE_FROM_SYSTEM|
                    FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL,GetLastError(),
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                    (LPTSTR) &lpMsgBuf,0,NULL);

      gdcmErrorMacro("FindNextFile error. Error is " << (char *)lpMsgBuf
                   <<" for the directory : "<<dirName);
      return -1;
   }

#else
  // Real POSIX implementation: scandir is a BSD extension only, and doesn't 
  // work on debian for example

   DIR* dir = opendir(dirName.c_str());
   if (!dir)
   {
      return 0;
   }

   // According to POSIX, the dirent structure contains a field char d_name[]
   // of unspecified size, with at most NAME_MAX characters preceeding the
   // terminating null character. Use of other fields will harm the  porta-
   // bility of your programs.

   struct stat buf;
   dirent *d;
   for (d = readdir(dir); d; d = readdir(dir))
   {
      fileName = dirName + d->d_name;
      if( stat(fileName.c_str(), &buf) != 0 )
      {
         gdcmErrorMacro( strerror(errno) );
      }
      if ( S_ISREG(buf.st_mode) )    //is it a regular file?
      {
         Filenames.push_back( fileName );
         numberOfFiles++;
      }
      else if ( S_ISDIR(buf.st_mode) ) //directory?
      {
         if ( d->d_name[0] != '.' && recursive ) //we also skip hidden files
         {
            numberOfFiles += Explore( fileName, recursive);
         }
      }
      else
      {
         gdcmErrorMacro( "Unexpected error" );
         return -1;
      }
   }
   if( closedir(dir) != 0 )
   {
      gdcmErrorMacro( strerror(errno) );
   }
#endif

  return numberOfFiles;
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Print method
 * @param os ostream to write to 
 */
void DirList::Print(std::ostream &os, std::string const &)
{
   std::copy(Filenames.begin(), Filenames.end(), 
             std::ostream_iterator<std::string>(os, "\n"));
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
