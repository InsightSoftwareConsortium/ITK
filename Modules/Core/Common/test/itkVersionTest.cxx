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

#include "itkVersion.h"
#include "itkTestingMacros.h"

#include <iostream>


int itkVersionTest( int, char* [] )
{
  int testPassStatus = EXIT_SUCCESS;

  itk::Version::Pointer version = itk::Version::New();

  EXERCISE_BASIC_OBJECT_METHODS( version, Version, Object );

  const char * itkVersion = itk::Version::GetITKVersion();
  std::cout << "itk version: " << itkVersion << std::endl;

  int itkMajorVersion = itk::Version::GetITKMajorVersion();
  std::cout << "itk Major version: " << itkMajorVersion << std::endl;

  int itkMinorVersion = itk::Version::GetITKMinorVersion();
  std::cout << "itk Minor version: " << itkMinorVersion << std::endl;

  int itkBuildVersion = itk::Version::GetITKBuildVersion();
  std::cout << "itk Build version: " << itkBuildVersion << std::endl;

  const char * itkSourceVersion = itk::Version::GetITKSourceVersion();
  std::cout << "itk Source version: " << itkSourceVersion << std::endl;

  return testPassStatus;
}
