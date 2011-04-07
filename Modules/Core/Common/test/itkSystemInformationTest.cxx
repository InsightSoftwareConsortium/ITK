/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <cstring>
#include "itkConfigure.h"
#include <sys/stat.h>
#include <time.h>
#include <string.h>

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
    char bufferIn[bufferSize];
    char bufferOut[6*bufferSize]; // worst case scenario
    // This copy loop is very sensitive on certain platforms with
    // slightly broken stream libraries (like HPUX).  Normally, it is
    // incorrect to not check the error condition on the fin.read()
    // before using the data, but the fin.gcount() will be zero if an
    // error occurred.  Therefore, the loop should be safe everywhere.
    while(fin)
      {
      fin.read(bufferIn, bufferSize);
      if(fin.gcount())
        {
        // convert buffer to an XML safe form
        const char *s = bufferIn;
        char *x = bufferOut;
        *x = '\0';
        for (int i = 0; i < static_cast<int>(fin.gcount()); i++)
          {
          // replace all special characters
          switch (*s)
            {
            case '&':
              strcat(x, "&amp;"); x += 5;
              break;
            case '"':
              strcat(x, "&quot;"); x += 6;
              break;
            case '\'':
              strcat(x, "&apos;"); x += 6;
              break;
            case '<':
              strcat(x, "&lt;"); x += 4;
              break;
            case '>':
              strcat(x, "&gt;"); x += 4;
              break;
            default:
              *x = *s; x++;
              *x = '\0'; // explicitly terminate the new string
            }
          s++;
          }
        os.write(bufferOut, x - bufferOut);
        }
      }
    os.flush();
    }
  else
    {
    os << "Error opening \"" << name << "\" for reading.\n";
    }
}

int main(int argc, const char* argv[])
{
  if(argc != 2)
    {
    std::cerr << "Usage: itkSystemInformationTest <top-of-build-tree>\n";
    return EXIT_FAILURE;
    }
  std::string build_dir = argv[1];
  build_dir += "/";

  const char* files[] =
    {
      "CMakeCache.txt",
      "Modules/Core/Common/itkConfigure.h",
      "CMakeFiles/CMakeOutput.log",
      "CMakeFiles/CMakeError.log",
      "ITKConfig.cmake",
      "ITKConfigVersion.cmake",
      "ITKTargets.cmake",
      0
    };

  std::cout << "CTEST_FULL_OUTPUT\n";
  for(const char** f = files; *f; ++f)
    {
    std::string fname = build_dir + *f;
    itkSystemInformationPrintFile(fname.c_str(), std::cout);
    }
  return EXIT_SUCCESS;
}
