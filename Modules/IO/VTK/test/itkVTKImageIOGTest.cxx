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
#include "itkVTKImageIO.h"
#include "itkGTest.h"

#include <fstream>
#include <string>

TEST(VTKImageIO, TruncatedBinaryFileThrowsOnRead)
{
  using ImageType = itk::Image<unsigned char, 2>;
  const std::string path = std::string(::testing::TempDir()) + "/itkVTKImageIOGTest_truncated.vtk";
  {
    std::ofstream ofs(path, std::ios::binary);
    ofs << "# vtk DataFile Version 3.0\n"
        << "truncated binary fixture\n"
        << "BINARY\n"
        << "DATASET STRUCTURED_POINTS\n"
        << "DIMENSIONS 2 2 1\n"
        << "SPACING 1 1 1\n"
        << "ORIGIN 0 0 0\n"
        << "POINT_DATA 4\n"
        << "SCALARS scalars unsigned_char 1\n"
        << "LOOKUP_TABLE default\n";
    const unsigned char partialPixelData[2] = { 1, 2 };
    ofs.write(reinterpret_cast<const char *>(partialPixelData), sizeof(partialPixelData));
  }

  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(itk::VTKImageIO::New());
  reader->SetFileName(path);
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}
