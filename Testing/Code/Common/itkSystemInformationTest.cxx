/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSystemInformationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkLightObject.h"
#include <fstream>
#include <Code/Common/itkSystemInformationTest.h>
#include <sys/stat.h>

#if defined(ITK_BINARY_DIR)
# define ITK_SYSTEM_INFORMATION_DIR ITK_BINARY_DIR
#else
# define ITK_SYSTEM_INFORMATION_DIR ITKTesting_BINARY_DIR
#endif

void itkSystemInformationPrintFile(const char* name, std::ostream& os)
{
  os << "================================================================\n";
  struct stat fs;
  if(stat(name, &fs) != 0)
    {
    os << "The file \"" << name << "\" does not exist.\n";
    return;
    }

#ifdef _WIN32
  std::ifstream fin(name, std::ios::in | std::ios::binary);
#else
  std::ifstream fin(name, std::ios::in);
#endif

  if(fin)
    {
    os << "Contents of \"" << name << "\":\n";
    os << "----------------------------------------------------------------\n";
    const int bufferSize = 4096;
    char buffer[bufferSize];
    // This copy loop is very sensitive on certain platforms with
    // slightly broken stream libraries (like HPUX).  Normally, it is
    // incorrect to not check the error condition on the fin.read()
    // before using the data, but the fin.gcount() will be zero if an
    // error occurred.  Therefore, the loop should be safe everywhere.
    while(fin)
      {
      fin.read(buffer, bufferSize);
      if(fin.gcount())
        {
        os.write(buffer, fin.gcount());
        }
      }
    os.flush();
    }
  else
    {
    os << "Error opening \"" << name << "\" for reading.\n";
    }
}

int itkSystemInformationTest(int,char *[])
{
  const char* files[] =
    {
      ITK_SYSTEM_INFORMATION_DIR "/CMakeCache.txt", 
      ITK_SYSTEM_INFORMATION_DIR "/itkConfigure.h",
      ITK_SYSTEM_INFORMATION_DIR "/CMakeError.log",
      ITK_SYSTEM_INFORMATION_DIR "/ITKBuildSettings.cmake",
      ITK_SYSTEM_INFORMATION_DIR "/ITKLibraryDepends.cmake",
      ITK_SYSTEM_INFORMATION_DIR "/ITKConfig.cmake",
      0
    };

  const char** f;
  for(f = files; *f; ++f)
    {
    itkSystemInformationPrintFile(*f, std::cout);
    }
  
  return 0;
} 

// This test has been derived from the equivalent test in VTK:
/*=========================================================================

  Program:   Visualization Toolkit
  Module:    itkSystemInformationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 1993-2002 Ken Martin, Will Schroeder, Bill Lorensen 
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
