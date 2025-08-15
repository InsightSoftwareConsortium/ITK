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

#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkMetaDataObject.h"
#include "itkGTest.h"
#include "itksys/SystemTools.hxx"
#include "itkImageSeriesReader.h"
#include <string>
#include <vector>


#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

struct ITKGDCMImageIO : public ::testing::Test
{
  void
  SetUp() override
  {
    itksys::SystemTools::MakeDirectory(m_TempDir);
  }

  void
  TearDown() override
  {
    itksys::SystemTools::RemoveADirectory(m_TempDir);
  }

  const std::string m_TempDir{ TOSTRING(ITK_TEST_OUTPUT_DIR) + "/ITKGDCMImageIO" };
  const std::string m_DicomSeriesInput{ TOSTRING(DICOM_SERIES_INPUT) };
};

} // namespace

TEST_F(ITKGDCMImageIO, ReadSlicesNegativeSpacing)
{
  using PixelType = uint16_t;
  constexpr unsigned int Dimension = 3;
  using ImageType = itk::Image<PixelType, Dimension>;

  using NamesGeneratorType = itk::GDCMSeriesFileNames;
  auto namesGenerator = NamesGeneratorType::New();
  namesGenerator->SetDirectory(m_DicomSeriesInput);
  namesGenerator->SetUseSeriesDetails(true);
  std::vector<std::string> fileNames = namesGenerator->GetInputFileNames();

  using SeriesReaderType = itk::ImageSeriesReader<ImageType>;
  auto seriesReader = SeriesReaderType::New();
  seriesReader->SetFileNames(fileNames);
  auto gdcmIO = itk::GDCMImageIO::New();
  seriesReader->SetImageIO(gdcmIO);
  ASSERT_NO_THROW(seriesReader->UpdateLargestPossibleRegion());

  ImageType::Pointer outputImage = seriesReader->GetOutput();
  outputImage->DisconnectPipeline();

  seriesReader->ReverseOrderOn();
  ASSERT_NO_THROW(seriesReader->UpdateLargestPossibleRegion());
  ImageType::Pointer reversedOutputImage = seriesReader->GetOutput();
  reversedOutputImage->DisconnectPipeline();

  EXPECT_EQ(outputImage->GetLargestPossibleRegion().GetSize(),
            reversedOutputImage->GetLargestPossibleRegion().GetSize());
  ITK_EXPECT_VECTOR_NEAR(outputImage->GetSpacing(), reversedOutputImage->GetSpacing(), 1e-6);
  EXPECT_NE(outputImage->GetOrigin(), reversedOutputImage->GetOrigin());

  // calculate the index at the middle of the image
  ImageType::IndexType middleIndex;
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    middleIndex[d] =
      outputImage->GetLargestPossibleRegion().GetIndex()[d] + outputImage->GetLargestPossibleRegion().GetSize()[d] / 2;
  }

  const std::vector<ImageType::IndexType> testIndices = {
    { { 0, 0, 0 } }, { { 1, 1, 1 } }, { { 2, 2, 2 } }, middleIndex
  };

  // test that the reversed image has the same pixel values at the same physical location
  for (const auto & idx : testIndices)
  {
    ImageType::PointType point;
    outputImage->TransformIndexToPhysicalPoint(idx, point);
    auto reverseIdx = reversedOutputImage->TransformPhysicalPointToIndex(point);

    std::cout << "Testing idx: " << idx << " reverseIdx: " << reverseIdx << std::endl;
    ASSERT_TRUE(reversedOutputImage->GetLargestPossibleRegion().IsInside(reverseIdx));
    EXPECT_EQ(outputImage->GetPixel(idx), reversedOutputImage->GetPixel(reverseIdx));
  }
}
