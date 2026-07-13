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

TEST(VTKImageIO, MalformedDimensionsLineThrows)
{
  const std::string path = std::string(::testing::TempDir()) + "/itkVTKImageIOGTest_malformed_dims.vtk";
  {
    std::ofstream ofs(path);
    ofs << "# vtk DataFile Version 3.0\n"
        << "malformed dimensions fixture\n"
        << "ASCII\n"
        << "DATASET STRUCTURED_POINTS\n"
        << "DIMENSIONS 5\n"
        << "SPACING 1 1 1\n"
        << "ORIGIN 0 0 0\n"
        << "SCALARS scalars float 1\n"
        << "LOOKUP_TABLE default\n"
        << "1 2 3 4 5\n";
  }

  auto vtkIO = itk::VTKImageIO::New();
  vtkIO->SetFileName(path);
  EXPECT_THROW(vtkIO->ReadImageInformation(), itk::ExceptionObject);
}

TEST(VTKImageIO, ScalarsNameContainingVectorSubstringParsesAsScalars)
{
  const std::string path = std::string(::testing::TempDir()) + "/itkVTKImageIOGTest_scalars_vector_substr.vtk";
  {
    std::ofstream ofs(path);
    ofs << "# vtk DataFile Version 3.0\n"
        << "scalars array name containing the substring vector\n"
        << "ASCII\n"
        << "DATASET STRUCTURED_POINTS\n"
        << "DIMENSIONS 2 1 1\n"
        << "SPACING 1 1 1\n"
        << "ORIGIN 0 0 0\n"
        << "POINT_DATA 2\n"
        << "SCALARS vector_field float\n"
        << "LOOKUP_TABLE default\n"
        << "1.0 2.0\n";
  }

  auto vtkIO = itk::VTKImageIO::New();
  vtkIO->SetFileName(path);
  ASSERT_NO_THROW(vtkIO->ReadImageInformation());

  EXPECT_EQ(vtkIO->GetPixelType(), itk::IOPixelEnum::SCALAR);
  EXPECT_EQ(vtkIO->GetNumberOfComponents(), 1u);
}

TEST(VTKImageIO, IndentedAttributeKeywordsParse)
{
  const std::string path = std::string(::testing::TempDir()) + "/itkVTKImageIOGTest_indented_keywords.vtk";
  {
    std::ofstream ofs(path);
    ofs << "# vtk DataFile Version 3.0\n"
        << "attribute keywords with leading whitespace\n"
        << "ASCII\n"
        << "DATASET STRUCTURED_POINTS\n"
        << "DIMENSIONS 2 1 1\n"
        << "  SPACING 2 3 1\n"
        << "\tORIGIN 5 6 0\n"
        << "POINT_DATA 2\n"
        << " \t SCALARS scalars float 1\n"
        << "LOOKUP_TABLE default\n"
        << "1.0 2.0\n";
  }

  auto vtkIO = itk::VTKImageIO::New();
  vtkIO->SetFileName(path);
  ASSERT_NO_THROW(vtkIO->ReadImageInformation());

  EXPECT_EQ(vtkIO->GetPixelType(), itk::IOPixelEnum::SCALAR);
  EXPECT_EQ(vtkIO->GetNumberOfComponents(), 1u);
  EXPECT_DOUBLE_EQ(vtkIO->GetSpacing(0), 2.0);
  EXPECT_DOUBLE_EQ(vtkIO->GetSpacing(1), 3.0);
  EXPECT_DOUBLE_EQ(vtkIO->GetOrigin(0), 5.0);
  EXPECT_DOUBLE_EQ(vtkIO->GetOrigin(1), 6.0);
}
