/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDirectory.h"

namespace itk
{

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
::PrintSelf(std::ostream& os, Indent indent) const
{ 
  Superclass::PrintSelf(os, indent);
  os << indent << "Directory for: " << m_Path << "\n";
  os << indent << "Contains the following files:\n";
  indent = indent.GetNextIndent();
  for(std::vector<std::string>::const_iterator i = m_Files.begin();
      i != m_Files.end(); ++i)
    {
    os << indent << (*i) << "\n";
    }
}

} // end namespace itk

// First microsoft compilers

#ifdef _MSC_VER
#include "itkWindows.h"
#include <io.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

namespace itk
{
  
/**
 *
 */
bool 
Directory
::Load(const char* name)
{
  char* buf;
  int n = static_cast<int>( strlen(name) );
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
  struct _finddata_t data;  // data of current file
  
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

} // end namespace itk

#else

// Now the POSIX style directory access

#include <sys/types.h>
#include <dirent.h>

namespace itk
{
  
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
  for (dirent* d = readdir(dir); d; d = readdir(dir) )
    {
    m_Files.push_back(d->d_name);
    }
  m_Path = name;
  closedir(dir);
  return 1;
}

} // end namespace itk

#endif

namespace itk
{
  
/**
 *
 */
const char* 
Directory
::GetFile(unsigned int index)
{
  if ( index >= m_Files.size() )
    {
    itkGenericOutputMacro( << "Bad index for GetFile on itk::Directory\n");
    return 0;
    }
  
  return m_Files[index].c_str();
}

} // end namespace itk

