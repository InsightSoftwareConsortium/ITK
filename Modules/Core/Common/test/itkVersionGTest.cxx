/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkVersion.h"
#include "itkGTest.h"

#include <iostream>
#include <string_view>

TEST(Version, BasicObjectMethods)
{
  const itk::Version::Pointer version = itk::Version::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(version, Version, Object);
}

TEST(Version, VersionStrings)
{
  const char * itkVersion = itk::Version::GetITKVersion();
  std::cout << "itk version: " << itkVersion << std::endl;
  EXPECT_NE(itkVersion, nullptr);
  EXPECT_FALSE(std::string_view(itkVersion).empty());

  const int itkMajorVersion = itk::Version::GetITKMajorVersion();
  std::cout << "itk Major version: " << itkMajorVersion << std::endl;
  EXPECT_EQ(itkMajorVersion, ITK_VERSION_MAJOR);

  const int itkMinorVersion = itk::Version::GetITKMinorVersion();
  std::cout << "itk Minor version: " << itkMinorVersion << std::endl;
  EXPECT_EQ(itkMinorVersion, ITK_VERSION_MINOR);

  const int itkBuildVersion = itk::Version::GetITKBuildVersion();
  std::cout << "itk Build version: " << itkBuildVersion << std::endl;
  EXPECT_EQ(itkBuildVersion, ITK_VERSION_PATCH);

  const char * itkSourceVersion = itk::Version::GetITKSourceVersion();
  std::cout << "itk Source version: " << itkSourceVersion << std::endl;
  EXPECT_NE(itkSourceVersion, nullptr);
  EXPECT_FALSE(std::string_view(itkSourceVersion).empty());
}
