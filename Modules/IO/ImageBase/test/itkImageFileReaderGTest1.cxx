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

#include "itkImageFileReader.h"
#include "itkGTest.h"


TEST(ImageFileReader, ConvertedLegacyTest)
{
  using ImageNDType = itk::Image<short, 2>;
  using ReaderType = itk::ImageFileReader<ImageNDType>;

  // Try an empty read
  auto reader = ReaderType::New();
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);


  // Now try a read with an image that doesn't exist
  reader->SetFileName("this_file_should_not_exist");
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);


  // Let's try to read a file where no ImageIO can read it.
  // Use the test binary itself, which is guaranteed to exist on disk.
  reader->SetFileName(::testing::internal::GetArgvs()[0]);
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}
