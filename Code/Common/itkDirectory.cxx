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

itkDirectory::itkDirectory() 
  :m_NumberOfFiles(0), m_Files(0), m_Path(0)
{
}

itkDirectory::~itkDirectory() 
{
  for ( int i =0; i < m_NumberOfFiles; i++ )
    {
    delete [] m_Files[i];
    }
  delete [] m_Files;
}

void itkDirectory::PrintSelf(std::ostream& os, itkIndent indent)
{ 
  itkObject::PrintSelf(os, indent);
  if ( !m_Path )
    {
    os << indent << "Directory not open\n";
    return;
    }
  
  os << indent << "Directory for: " <<  m_Path << "\n";
  os << indent << "Contains the following files:\n";
  indent = indent.GetNextIndent();
  for ( int i =0; i < m_NumberOfFiles; i++ )
    {
    os << indent << m_Files[i] << "\n";
    }
}

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

bool itkDirectory::Load(const char* name)
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
  
  // First count the number of files in the directory
  long srchHandle = _findfirst(buf, &data);
  if ( srchHandle == -1 )
    {
    cerr << "can't open directory " << buf << endl;
    m_NumberOfFiles = 0;
    return 0;
    }
  
  m_NumberOfFiles = 1;
  while ( _findnext(srchHandle, &data) != -1 )
    {
    m_NumberOfFiles++;
    }
  m_Files = new char*[m_NumberOfFiles];

  // Now put them into the file array
  srchHandle = _findfirst(buf, &data);
  delete [] buf;
  
  if ( srchHandle == -1 )
    {
    m_NumberOfFiles = 0;
    return 0;
    }
  
  // Loop through names
  int i = 0;
  do 
    {
    m_Files[i] = strcpy(new char[strlen(data.name)+1], data.name);
    i++;
    } 
  while ( _findnext(srchHandle, &data) != -1 );
  m_Path = strcpy(new char[strlen(name)+1], name);
  return _findclose(srchHandle) != -1;
}

#else

// Now the POSIX style directory access

#include <sys/types.h>
#include <dirent.h>

int itkDirectory::Open(const char* name)
{
  DIR* dir = opendir(name);
  if ( !dir ) 
    {
    return 0;
    }
  
  m_NumberOfFiles = 0;
  dirent* d =0;
  
  for ( d = readdir(dir); d; d = readdir(dir) )
    {
    m_NumberOfFiles++;
    }
  m_Files = new char*[m_NumberOfFiles];
  closedir(dir);
  
  dir = opendir(name);
  int i = 0;
  for ( d = readdir(dir); d; d = readdir(dir) )
    {
    m_Files[i] = strcpy(new char[strlen(d->d_name)+1], d->d_name);
    i++;
    }
  m_Path = strcpy(new char[strlen(name)+1], name);
  return 1;
}

#endif


const char* itkDirectory::GetFile(int index)
{
  if ( index >= m_NumberOfFiles || index < 0 )
    {
    itkErrorMacro( << "Bad index for GetFile on itkDirectory\n");
    return 0;
    }
  
  return m_Files[index];
}

