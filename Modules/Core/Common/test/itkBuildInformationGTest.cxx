/*=========================================================================
 *
 *  Copyright NumFOCUS
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

/*
 * Tests for the itk::BuildInformation class
 */

#include <iostream>
#include <array>
#include <type_traits>

#include "itkGTest.h"
#include "itkBuildInformation.h"


TEST(ITKBuildInformation, InformationFeatures)
{
  using MapType = itk::BuildInformation::MapType;

  itk::BuildInformation::Pointer instance = itk::BuildInformation::GetInstance();
  EXPECT_EQ(instance.IsNull(), false);


  const MapType & localMap = instance->GetMap();

  auto it = localMap.find("PROJECT_URL");
  EXPECT_EQ(it != localMap.end(), true);
  if (it != localMap.end())
  {
    EXPECT_EQ(it->first, std::string{ "PROJECT_URL" });
    EXPECT_EQ(it->second.m_Value, std::string{ "http://www.itk.org" });
    EXPECT_EQ(it->second.m_Description, std::string{ "The URL of project." });
  }

  for (auto mapEntry : localMap)
  {
    std::cout << "--------------------------------------------------------------------" << std::endl
              << "Key: " << mapEntry.first << std::endl
              << "\tValue: " << mapEntry.second.m_Value << std::endl
              << "\tDescription: " << mapEntry.second.m_Description << std::endl;
  }

  EXPECT_EQ(itk::BuildInformation::GetValue("PROJECT_URL"), "http://www.itk.org");

  EXPECT_GT(itk::BuildInformation::GetAllKeys().size(), static_cast<size_t>(5));

  for (const auto & keyEntry : itk::BuildInformation::GetAllKeys())
  {
    std::cout << keyEntry << std::endl;
  }
}
