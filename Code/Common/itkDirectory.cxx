/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
::PrintSelf(std::ostream& os, Indent indent)
{ 
  Superclass::PrintSelf(os, indent);
  os << indent << "Directory for: " << m_Path << "\n";
  os << indent << "Contains the following files:\n";
  indent = indent.GetNextIndent();
  for(std::vector<std::string>::iterator i = m_Files.begin();
      i != m_Files.end(); ++i)
    {
    os << indent << (*i) << "\n";
    }
}

} // end namespace itk

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
  
  dir = opendir(name);
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
    itkErrorMacro( << "Bad index for GetFile on itk::Directory\n");
    return 0;
    }
  
  return m_Files[index].c_str();
}

} // end namespace itk

