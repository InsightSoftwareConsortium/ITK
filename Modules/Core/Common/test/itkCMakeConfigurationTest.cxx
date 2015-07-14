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


#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <sys/stat.h>
#include <time.h>
#include <string.h>

void itkCMakeInformationPrintFile(const char* name, std::ostream& os)
{
  // Preserve valuable output regardless of the limits set in
  // CMake/CTestCustom.cmake
  os << "CTEST_FULL_OUTPUT\n";
  os << "System Information File \"" << name << "\"";
  struct stat fs;
  if(stat(name, &fs) != 0)
    {
    os << " does not exist.\n";
    return;
    }
  else
    {
    os << " has " << fs.st_size << " bytes";
    }

  std::ifstream fin(name);
  if(fin)
    {
    const char* div = "=======================================================================";
    os << ":\n[" << div << "[\n";
    os << fin.rdbuf();
    os << "]" << div << "]\n";
    os.flush();
    }
  else
    {
    os << " but cannot be opened for read.\n";
    }
}

int main(int argc, char* argv[])
{
  if(argc != 2)
    {
    std::cerr << "Usage: itkCMakeInformationTest <top-of-build-tree>\n";
    return EXIT_FAILURE;
    }
  std::string build_dir = argv[1];
  build_dir += "/";

  const char* files[] =
    {
      "CMakeCache.txt",
      "Modules/Core/Common/itkConfigure.h",
      "ITKConfig.cmake",
      "ITKConfigVersion.cmake",
      "ITKTargets.cmake",
      NULL
    };

  for(const char** f = files; *f; ++f)
    {
    std::string fname = build_dir + *f;
    itkCMakeInformationPrintFile(fname.c_str(), std::cout);
    }
  return EXIT_SUCCESS;
}
