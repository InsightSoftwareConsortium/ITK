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
#include "itkHDF5ImageIO.h"
#include "itkMetaDataObject.h"
#include "itkGTest.h"

#include <string>

TEST(HDF5ImageIO, WriteImageInformationNonConstCStringMetaDataRoundTrips)
{
  const std::string path = std::string(::testing::TempDir()) + "/itkHDF5ImageIOGTest_cstring_meta.hdf5";

  auto io = itk::HDF5ImageIO::New();
  io->SetFileName(path);
  io->SetNumberOfDimensions(2);
  io->SetDimensions(0, 2);
  io->SetDimensions(1, 2);
  io->SetSpacing(0, 1.0);
  io->SetSpacing(1, 1.0);
  io->SetOrigin(0, 0.0);
  io->SetOrigin(1, 0.0);
  io->SetDirection(0, std::vector<double>{ 1.0, 0.0 });
  io->SetDirection(1, std::vector<double>{ 0.0, 1.0 });
  io->SetNumberOfComponents(1);
  io->SetPixelType(itk::IOPixelEnum::SCALAR);
  io->SetComponentType(itk::IOComponentEnum::UCHAR);

  char   value[] = "payload";
  char * cstr = value;
  itk::EncapsulateMetaData<char *>(io->GetMetaDataDictionary(), "CStringMeta", cstr);

  ASSERT_NO_THROW(io->WriteImageInformation());

  auto readIO = itk::HDF5ImageIO::New();
  readIO->SetFileName(path);
  ASSERT_NO_THROW(readIO->ReadImageInformation());

  std::string readValue;
  ASSERT_TRUE(itk::ExposeMetaData<std::string>(readIO->GetMetaDataDictionary(), "CStringMeta", readValue));
  EXPECT_EQ(readValue, "payload");
}
