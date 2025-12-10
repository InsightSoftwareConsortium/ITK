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

class ITKGDCMSeriesTestData : public ITKGDCMImageIO
{
public:
  using PixelType = uint16_t;
  static constexpr unsigned int Dimension = 2;
  using ImageType = itk::Image<PixelType, Dimension>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  void
  SetUp() override
  {
    itksys::SystemTools::MakeDirectory(m_TempDir);
    CreateTestDicomSeries();
  }

  void
  TearDown() override
  {
    itksys::SystemTools::RemoveADirectory(m_TempDir);
  }

protected:
  const std::string        m_TempDir{ TOSTRING(ITK_TEST_OUTPUT_DIR) + "/ITKGDCMSeriesTestData" };
  std::vector<std::string> m_DicomFiles;


private:
  void
  CreateTestDicomSeries()
  {
    // DICOM meta-data values from the report
    const std::vector<std::string> positions = {
      "-216.500\\-216.500\\70.000",   // slice 1 (top)
      "-216.500\\-216.500\\-187.500", // slice 2 (middle)
      "-216.500\\-216.500\\-445.000"  // slice 3 (bottom)
    };

    const std::string orientation = "1.000000\\0.000000\\0.000000\\0.000000\\1.000000\\0.000000";

    // Create a 2x2 image for each slice (equivalent to 3D [2,2,3] sliced)
    ImageType::SizeType size;
    size[0] = 2;
    size[1] = 2;

    ImageType::IndexType start;
    start.Fill(0);

    ImageType::RegionType region(start, size);

    auto gdcmIO = itk::GDCMImageIO::New();
    gdcmIO->KeepOriginalUIDOn();
    auto writer = WriterType::New();
    writer->SetImageIO(gdcmIO);

    // Write each slice as a DICOM file with appropriate tags
    for (size_t i = 0; i < positions.size(); ++i)
    {
      auto image = ImageType::New();
      image->SetRegions(region);
      image->Allocate();
      image->FillBuffer(100); // Just to have nonzero pixel values

      // Get the metadata dictionary
      auto & dict = image->GetMetaDataDictionary();

      // Set required DICOM tags
      itk::EncapsulateMetaData<std::string>(dict, "0020|0032", positions[i]);          // ImagePositionPatient
      itk::EncapsulateMetaData<std::string>(dict, "0020|0037", orientation);           // ImageOrientationPatient
      itk::EncapsulateMetaData<std::string>(dict, "0008|0060", "CT");                  // Modality
      itk::EncapsulateMetaData<std::string>(dict, "0020|0013", std::to_string(i + 1)); // InstanceNumber
      itk::EncapsulateMetaData<std::string>(dict, "0010|0010", "Test^Patient");        // PatientName
      itk::EncapsulateMetaData<std::string>(
        dict, "0020|000e", "1.2.3.4.5.6.7.8"); // SeriesInstanceUID (same for all slices)

      // Additional required DICOM tags for proper series
      itk::EncapsulateMetaData<std::string>(
        dict, "0008|0016", "1.2.840.10008.5.1.4.1.1.2"); // SOPClassUID (CT Image Storage)
      itk::EncapsulateMetaData<std::string>(
        dict, "0008|0018", "1.2.3.4.5.6.7.8.9." + std::to_string(i + 1));          // SOPInstanceUID
      itk::EncapsulateMetaData<std::string>(dict, "0020|000d", "1.2.3.4.5.6.7.8"); // StudyInstanceUID
      itk::EncapsulateMetaData<std::string>(dict, "0010|0020", "12345");           // PatientID
      itk::EncapsulateMetaData<std::string>(dict, "0008|0020", "20240101");        // StudyDate
      itk::EncapsulateMetaData<std::string>(dict, "0008|0030", "120000");          // StudyTime

      const std::string filename = m_TempDir + "/slice_" + std::to_string(i + 1) + ".dcm";
      writer->SetFileName(filename);
      writer->SetInput(image);
      writer->Update();

      m_DicomFiles.push_back(filename);
    }
  }
};

} // namespace

TEST_F(ITKGDCMSeriesTestData, ReadSlicesReverseOrder)
{
  // This test image series has non-unit meta-data:
  // spacing: [0.859375, 0.85939, 1.60016]
  // origin:[-112, -21.688, 126.894]
  // direction:
  //  [1 0 0,
  //   0 0.466651 0.884442,
  //   0 -0.884442 0.466651]
  constexpr unsigned int VolumeDimension = 3;
  using VolumeImageType = itk::Image<PixelType, VolumeDimension>;

  using NamesGeneratorType = itk::GDCMSeriesFileNames;
  auto namesGenerator = NamesGeneratorType::New();
  namesGenerator->SetDirectory(m_DicomSeriesInput);
  namesGenerator->SetUseSeriesDetails(true);
  std::vector<std::string> fileNames = namesGenerator->GetInputFileNames();

  using SeriesReaderType = itk::ImageSeriesReader<VolumeImageType>;
  auto seriesReader = SeriesReaderType::New();
  seriesReader->SetFileNames(fileNames);
  auto gdcmIO = itk::GDCMImageIO::New();
  seriesReader->SetImageIO(gdcmIO);
  ASSERT_NO_THROW(seriesReader->UpdateLargestPossibleRegion());

  VolumeImageType::Pointer outputImage = seriesReader->GetOutput();
  outputImage->DisconnectPipeline();

  seriesReader->ReverseOrderOn();
  ASSERT_NO_THROW(seriesReader->UpdateLargestPossibleRegion());
  VolumeImageType::Pointer reversedOutputImage = seriesReader->GetOutput();
  reversedOutputImage->DisconnectPipeline();

  std::cout << "baseline direction: " << outputImage->GetDirection() << std::endl;
  std::cout << "reversed direction: " << reversedOutputImage->GetDirection() << std::endl;
  EXPECT_EQ(outputImage->GetLargestPossibleRegion().GetSize(),
            reversedOutputImage->GetLargestPossibleRegion().GetSize());
  ITK_EXPECT_VECTOR_NEAR(outputImage->GetSpacing(), reversedOutputImage->GetSpacing(), 1e-6);
  EXPECT_NE(outputImage->GetOrigin(), reversedOutputImage->GetOrigin());

  // calculate the index at the middle of the image
  VolumeImageType::IndexType middleIndex;
  for (unsigned int d = 0; d < VolumeDimension; ++d)
  {
    middleIndex[d] =
      outputImage->GetLargestPossibleRegion().GetIndex()[d] + outputImage->GetLargestPossibleRegion().GetSize()[d] / 2;
  }

  const std::vector<VolumeImageType::IndexType> testIndices = {
    { { 0, 0, 0 } }, { { 1, 1, 1 } }, { { 2, 2, 2 } }, middleIndex
  };

  // test that the reversed image has the same pixel values at the same physical location
  for (const auto & idx : testIndices)
  {
    VolumeImageType::PointType point;
    outputImage->TransformIndexToPhysicalPoint(idx, point);
    auto reverseIdx = reversedOutputImage->TransformPhysicalPointToIndex(point);

    std::cout << "Testing idx: " << idx << " reverseIdx: " << reverseIdx << std::endl;
    ASSERT_TRUE(reversedOutputImage->GetLargestPossibleRegion().IsInside(reverseIdx));
    EXPECT_EQ(outputImage->GetPixel(idx), reversedOutputImage->GetPixel(reverseIdx));
  }
}

TEST_F(ITKGDCMSeriesTestData, CreateAndReadTestSeries)
{
  // Verify that the DICOM files were created
  ASSERT_EQ(m_DicomFiles.size(), 3);

  for (const auto & filename : m_DicomFiles)
  {
    ASSERT_TRUE(itksys::SystemTools::FileExists(filename));
  }

  // Read the series using GDCMSeriesFileNames
  using NamesGeneratorType = itk::GDCMSeriesFileNames;
  auto namesGenerator = NamesGeneratorType::New();
  namesGenerator->SetDirectory(m_TempDir);
  namesGenerator->SetUseSeriesDetails(true);

  std::vector<std::string> fileNames = namesGenerator->GetInputFileNames();
  ASSERT_EQ(fileNames.size(), 3);

  // Read the series
  using ImageType3D = itk::Image<uint16_t, 3>;
  using SeriesReaderType = itk::ImageSeriesReader<ImageType3D>;
  auto seriesReader = SeriesReaderType::New();
  seriesReader->SetFileNames(fileNames);

  auto gdcmIO = itk::GDCMImageIO::New();
  seriesReader->SetImageIO(gdcmIO);

  ASSERT_NO_THROW(seriesReader->UpdateLargestPossibleRegion());

  ImageType3D::Pointer image = seriesReader->GetOutput();

  // Verify image properties
  ImageType3D::SizeType expectedSize = { { 2, 2, 3 } };
  EXPECT_EQ(image->GetLargestPossibleRegion().GetSize(), expectedSize);

  // Verify pixel values (should be 100)
  EXPECT_EQ(image->GetPixel({ 0, 0, 0 }), 100);
  EXPECT_EQ(image->GetPixel({ 1, 1, 1 }), 100);
}

TEST_F(ITKGDCMSeriesTestData, ReadSeriesTopToBottom)
{
  // Read in top-to-bottom order (files ordered by ImagePositionPatient Z coordinate)
  using ImageType3D = itk::Image<uint16_t, 3>;
  using SeriesReaderType = itk::ImageSeriesReader<ImageType3D>;

  // Get file list in top-to-bottom order
  const std::vector<std::string> & filesTopToBottom = m_DicomFiles;

  auto seriesReader = SeriesReaderType::New();
  auto gdcmIO = itk::GDCMImageIO::New();
  seriesReader->SetImageIO(gdcmIO);
  seriesReader->SetFileNames(filesTopToBottom);
  seriesReader->ForceOrthogonalDirectionOn(); // explicitly set default

  ASSERT_NO_THROW(seriesReader->UpdateLargestPossibleRegion());
  ImageType3D::Pointer image1 = seriesReader->GetOutput();

  std::cout << "Top-to-bottom order:" << std::endl;
  std::cout << "  Origin: " << image1->GetOrigin() << std::endl;
  std::cout << "  Direction: " << image1->GetDirection() << std::endl;
  std::cout << "  Spacing: " << image1->GetSpacing() << std::endl;

  // Verify image properties
  ImageType3D::SizeType expectedSize = { { 2, 2, 3 } };
  EXPECT_EQ(image1->GetLargestPossibleRegion().GetSize(), expectedSize);

  // The origin should be at the position of the first slice (top slice)
  ImageType3D::PointType expectedOrigin{ { -216.500, -216.500, 70.000 } }; // Z position of slice 1 (top)

  ITK_EXPECT_VECTOR_NEAR(image1->GetOrigin(), expectedOrigin, 1e-3);

  // Z spacing should be positive
  EXPECT_GT(image1->GetSpacing()[2], 0.0);
  // but the direction should have a negative Z component
  EXPECT_LT(image1->GetDirection()[2][2], 0.0);
}

TEST_F(ITKGDCMSeriesTestData, ReadSeriesBottomToTop)
{
  // Read in bottom-to-top order (files ordered by ImagePositionPatient Z coordinate)
  using ImageType3D = itk::Image<uint16_t, 3>;
  using SeriesReaderType = itk::ImageSeriesReader<ImageType3D>;

  // Get file list in bottom-to-top order
  std::vector<std::string> filesBottomToTop(m_DicomFiles.rbegin(), m_DicomFiles.rend());

  auto seriesReader = SeriesReaderType::New();
  auto gdcmIO = itk::GDCMImageIO::New();
  seriesReader->SetImageIO(gdcmIO);
  seriesReader->SetFileNames(filesBottomToTop);
  seriesReader->ForceOrthogonalDirectionOn(); // explicitly set default

  ASSERT_NO_THROW(seriesReader->UpdateLargestPossibleRegion());
  ImageType3D::Pointer image2 = seriesReader->GetOutput();

  std::cout << "Bottom-to-top order:" << std::endl;
  std::cout << "  Origin: " << image2->GetOrigin() << std::endl;
  std::cout << "  Direction: " << image2->GetDirection() << std::endl;
  std::cout << "  Spacing: " << image2->GetSpacing() << std::endl;

  // Verify image properties
  ImageType3D::SizeType expectedSize = { { 2, 2, 3 } };
  EXPECT_EQ(image2->GetLargestPossibleRegion().GetSize(), expectedSize);

  // The origin should be at the position of the first slice (bottom slice)
  ImageType3D::PointType expectedOrigin{
    { -216.500, -216.500, -445.000 }
  }; // X,Y from slice 1, Z position of slice 3 (bottom)

  ITK_EXPECT_VECTOR_NEAR(image2->GetOrigin(), expectedOrigin, 1e-3);

  // Z spacing should be positive (going from bottom to top)
  EXPECT_GT(image2->GetSpacing()[2], 0.0);
  // and the direction should have a positive Z component
  EXPECT_GT(image2->GetDirection()[2][2], 0.0);
}
