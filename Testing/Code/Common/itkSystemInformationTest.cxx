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

#include <iostream>
#include <fstream>
#include <string>
#include <Code/Common/itkSystemInformationTest.h>
#include <sys/stat.h>
#include <time.h>

#if defined(ITK_BINARY_DIR)
# define ITK_SYSTEM_INFORMATION_DIR ITK_BINARY_DIR
#else
# define ITK_SYSTEM_INFORMATION_DIR ITKTesting_BINARY_DIR
#endif

// Construct the name of the notes file.
#define ITK_SYSTEM_INFORMATION_NOTES \
  ITK_SYSTEM_INFORMATION_DIR "/Testing/HTML/TestingResults/Sites/" \
  ITKTesting_SITE "/" ITKTesting_BUILD_NAME "/BuildNameNotes.xml"

std::string itkGetCurrentDateTime(const char* format)
{
  char buf[1024];
  time_t t;
  time(&t);
  strftime(buf, sizeof(buf), format, localtime(&t));
  return buf;
}


void itkSystemInformationPrintFile(const char* name, std::ostream& os,
                                   bool note=false )
{
  if (!note)
    {
    os << "================================================================\n";
    }
  struct stat fs;
  if(stat(name, &fs) != 0)
    {
    os << "The file \"" << name << "\" does not exist.\n";
    return;
    }

#ifdef _WIN32
  std::ifstream fin(name, std::ios::in);
#else
  std::ifstream fin(name, std::ios::in);
#endif

  if(fin)
    {
    if (!note)
      {
      os << "Contents of \"" << name << "\":\n";
      os << "----------------------------------------------------------------\n";
      }
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

int main(int argc,char *argv[])
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
  
  std::ofstream outf(ITK_SYSTEM_INFORMATION_NOTES, std::ios::out);
  if(outf)
    {
    std::cout << "Also writing this information to file " << ITK_SYSTEM_INFORMATION_NOTES << "\n";
  
    outf << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << std::endl;
    switch (argc)
      {
      case 1:
        outf << "<Site BuildName=\"Local\"  Name=\"localhost\">" << std::endl;
        break;
      case 2:
        outf << "<Site BuildName=\"Local\"  Name=\"" << argv[1] << "\">" << std::endl;
        break;
      case 3:
        outf << "<Site BuildName=\"" << argv[2] << "\"  Name=\"" << argv[1] << "\">" << std::endl;
        break;
      default:
        outf << "<Site BuildName=\"Local\"  Name=\"localhost\">" << std::endl;
        break;
      }
    outf << "<BuildNameNotes>" << std::endl;
    for(f = files; *f; ++f)
      {
      outf << "<Note Name=\"" << *f << "\">" << std::endl;
      outf << "<DateTime>"
           << itkGetCurrentDateTime("%a %b %d %Y %H:%M:%S %Z")
           << "</DateTime>" << std::endl;
      outf << "<Text>" << std::endl;
    
      itkSystemInformationPrintFile(*f, outf, true);

      outf << "</Text>" << std::endl;
      outf << "</Note>" << std::endl;
      }
    
    outf << "</BuildNameNotes>" << std::endl;
    outf << "</Site>" << std::endl;
    }
  else
    {
    std::cerr << "Error writing this information to file " << ITK_SYSTEM_INFORMATION_NOTES << "\n";
    return 1;
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
