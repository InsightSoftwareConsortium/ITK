/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkDirectory.h"

ITK_NAMESPACE_BEGIN

/**
 *
 */
Directory
::Directory() 
{
}

/**
 *
 */
Directory
::~Directory() 
{
}

/*
ostream& operator << (ostream& os, string& s)
{
  os << s.c_str();
  return os;
}
*/

/**
 *
 */
void 
Directory
::PrintSelf(std::ostream& os, Indent indent)
{ 
  Object::PrintSelf(os, indent);
  os << indent << "Directory for: " << m_Path << "\n";
  os << indent << "Contains the following files:\n";
  indent = indent.GetNextIndent();
  for(std::vector<std::string>::iterator i = m_Files.begin();
      i != m_Files.end(); ++i)
    {
    os << indent << (*i) << "\n";
    }
}

ITK_NAMESPACE_END

// First microsoft compilers

#ifdef _MSC_VER
#include <windows.h>
#include <io.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

ITK_NAMESPACE_BEGIN
  
/**
 *
 */
bool 
Directory
::Load(const char* name)
{
  char* buf;
  int n = strlen(name);
  if ( name[n - 1] == '/' ) 
    {
    buf = new char[n + 1 + 1];
    sprintf(buf, "%s*", name);
    } 
  else
    {
    buf = new char[n + 2 + 1];
    sprintf(buf, "%s/*", name);
    }
  struct _finddata_t data;	// data of current file
  
  // Now put them into the file array
  long srchHandle = _findfirst(buf, &data);
  delete [] buf;
  
  if ( srchHandle == -1 )
    {
    return 0;
    }
  
  // Loop through names
  do 
    {
    m_Files.push_back(data.name);
    } 
  while ( _findnext(srchHandle, &data) != -1 );
  m_Path = name;
  return _findclose(srchHandle) != -1;
}

ITK_NAMESPACE_END

#else

// Now the POSIX style directory access

#include <sys/types.h>
#include <dirent.h>

ITK_NAMESPACE_BEGIN
  
/**
 *
 */
bool 
Directory
::Load(const char* name)
{
  DIR* dir = opendir(name);
  if ( !dir ) 
    {
    return 0;
    }
  
  dirent* d =0;
  dir = opendir(name);
  for ( d = readdir(dir); d; d = readdir(dir) )
    {
    m_Files.push_back(d->d_name);
    }
  m_Path = name;
  closedir(dir);
  return 1;
}

ITK_NAMESPACE_END

#endif

ITK_NAMESPACE_BEGIN
  
/**
 *
 */
const char* 
Directory
::GetFile(unsigned int index)
{
  if ( index >= m_Files.size() )
    {
    itkErrorMacro( << "Bad index for GetFile on itk::Directory\n");
    return 0;
    }
  
  return m_Files[index].c_str();
}

ITK_NAMESPACE_END

