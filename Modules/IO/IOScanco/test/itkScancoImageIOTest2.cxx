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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScancoImageIO.h"
#include "itkScancoImageIOFactory.h"
#include "itkTestingMacros.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int
itkScancoImageIOTest2(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " Input Output [ReUseIO]" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string inputFileName = argv[1];
  const std::string outputFileName = argv[2];

  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  constexpr unsigned int Dimension = 3;
  using PixelType = short;
  using ImageType = itk::Image<PixelType, Dimension>;
  ImageType::Pointer image;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();

  itk::ScancoImageIOFactory::RegisterOneFactory();
  using IOType = itk::ScancoImageIO;
  IOType::Pointer scancoIO = IOType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(scancoIO, ScancoImageIO, ImageIOBase);


  reader->SetImageIO(scancoIO);
  reader->SetFileName(inputFileName);
  // Populate the IO with file's metadata
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->UpdateOutputInformation());
  // Read the file without explicitly requesting ScancoIO
  ITK_TRY_EXPECT_NO_EXCEPTION(image = itk::ReadImage<ImageType>(inputFileName));
  itk::MetaDataDictionary & metaData = image->GetMetaDataDictionary(); // Get metadata from regularly read image

  std::cout << "Version: \t\t" << scancoIO->GetVersion() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetVersion(), std::string("CTDATA-HEADER_V1"));
  std::string stringMeta;
  itk::ExposeMetaData<std::string>(metaData, "Version", stringMeta);
  ITK_TEST_EXPECT_EQUAL(stringMeta, std::string("CTDATA-HEADER_V1"));
  std::cout << "PatientIndex: \t" << scancoIO->GetPatientIndex() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetPatientIndex(), 78);
  int intMeta;
  itk::ExposeMetaData<int>(metaData, "PatientIndex", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 78);
  std::cout << "ScannerID: \t\t" << scancoIO->GetScannerID() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetScannerID(), 2135);
  itk::ExposeMetaData<int>(metaData, "ScannerID", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 2135);
  std::cout << "SliceThickness: \t" << scancoIO->GetSliceThickness() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSliceThickness(), 0.036, 6, 1e-3));
  double doubleMeta;
  itk::ExposeMetaData<double>(metaData, "SliceThickness", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 0.036, 6, 1e-3));
  std::cout << "SliceIncrement: \t" << scancoIO->GetSliceIncrement() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSliceIncrement(), 0.036, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "SliceIncrement", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 0.036, 6, 1e-3));
  std::cout << "StartPosition: \t" << scancoIO->GetStartPosition() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetStartPosition(), 75.0, 6, 1e-3));
  std::vector<double> vectorDoubleMeta;
  itk::ExposeMetaData<std::vector<double>>(metaData, "DataRange", vectorDoubleMeta);
  std::cout << "DataRange[0]: \t" << scancoIO->GetDataRange()[0] << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[0], -2813.0, 6, 1e-3));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(vectorDoubleMeta[0], -2813.0, 6, 1e-3));
  std::cout << "DataRange[1]: \t" << scancoIO->GetDataRange()[1] << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[1], 32767.0, 6, 1e-3));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(vectorDoubleMeta[1], 32767.0, 6, 1e-3));
  std::cout << "MuScaling: \t\t" << scancoIO->GetMuScaling() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetMuScaling(), 4096.0, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "MuScaling", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 4096.0, 6, 1e-3));
  std::cout << "MuWater: \t\t" << scancoIO->GetMuWater() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetMuWater(), 0.7033, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "MuWater", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 0.7033, 6, 1e-3));
  std::cout << "RescaleType: \t\t" << scancoIO->GetRescaleType() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetRescaleType(), 2);
  itk::ExposeMetaData<int>(metaData, "RescaleType", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 2);
  std::cout << "RescaleSlope: \t\t" << scancoIO->GetRescaleSlope() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetRescaleSlope(), 0.347136, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "RescaleSlope", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 0.347136, 6, 1e-3));
  std::cout << "RescaleIntercept: \t\t" << scancoIO->GetRescaleIntercept() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetRescaleIntercept(), -1000.0, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "RescaleIntercept", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, -1000.0, 6, 1e-3));
  std::cout << "RescaleUnits: \t\t" << scancoIO->GetRescaleUnits() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetRescaleUnits(), std::string("mg HA/ccm"));
  itk::ExposeMetaData<std::string>(metaData, "RescaleUnits", stringMeta);
  ITK_TEST_EXPECT_EQUAL(stringMeta, std::string("mg HA/ccm"));
  std::cout << "CalibrationData: \t\t" << scancoIO->GetCalibrationData() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetCalibrationData(),
                        std::string("45 kVp, 0.5mm Al, BH: 1200mg HA/ccm, Scaling 4096"));
  itk::ExposeMetaData<std::string>(metaData, "CalibrationData", stringMeta);
  ITK_TEST_EXPECT_EQUAL(stringMeta, std::string("45 kVp, 0.5mm Al, BH: 1200mg HA/ccm, Scaling 4096"));
  std::cout << "NumberOfSamples: \t" << scancoIO->GetNumberOfSamples() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetNumberOfSamples(), 1024);
  itk::ExposeMetaData<int>(metaData, "NumberOfSamples", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 1024);
  std::cout << "NumberOfProjections: " << scancoIO->GetNumberOfProjections() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetNumberOfProjections(), 500);
  itk::ExposeMetaData<int>(metaData, "NumberOfProjections", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 500);
  std::cout << "ScanDistance: \t" << scancoIO->GetScanDistance() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetScanDistance(), 36.864, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "ScanDistance", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 36.864, 6, 1e-3));
  std::cout << "ScannerType: \t" << scancoIO->GetScannerType() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetScannerType(), 10);
  itk::ExposeMetaData<int>(metaData, "ScannerType", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 10);
  std::cout << "SampleTime: \t\t" << scancoIO->GetSampleTime() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSampleTime(), 400.0, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "SampleTime", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 400.0, 6, 1e-3));
  std::cout << "MeasurementIndex: \t" << scancoIO->GetMeasurementIndex() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetMeasurementIndex(), 4937);
  itk::ExposeMetaData<int>(metaData, "MeasurementIndex", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 4937);
  std::cout << "Site: \t\t" << scancoIO->GetSite() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetSite(), 5);
  itk::ExposeMetaData<int>(metaData, "Site", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 5);
  std::cout << "ReferenceLine: \t" << scancoIO->GetReferenceLine() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetReferenceLine(), 0);
  itk::ExposeMetaData<double>(metaData, "ReferenceLine", doubleMeta);
  ITK_TEST_EXPECT_EQUAL(doubleMeta, 0);
  std::cout << "ReconstructionAlg: \t" << scancoIO->GetReconstructionAlg() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetReconstructionAlg(), 3);
  itk::ExposeMetaData<int>(metaData, "ReconstructionAlg", intMeta);
  ITK_TEST_EXPECT_EQUAL(intMeta, 3);
  std::cout << "Energy: \t\t" << scancoIO->GetEnergy() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetEnergy(), 45.0, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "Energy", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 45.0, 6, 1e-3));
  std::cout << "Intensity: \t\t" << scancoIO->GetIntensity() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetIntensity(), 0.177, 6, 1e-3));
  itk::ExposeMetaData<double>(metaData, "Intensity", doubleMeta);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(doubleMeta, 0.177, 6, 1e-3));
  std::cout << "CreationDate: \t" << scancoIO->GetCreationDate() << std::endl;
  std::cout << "ModificationDate: \t" << scancoIO->GetModificationDate() << std::endl;

  itk::EncapsulateMetaData<std::string>(metaData, "PatientName", std::string("Zukic"));

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  if (argc > 3) // Explicitly use scancoIO
  {
    ITK_TEST_EXPECT_TRUE(scancoIO->CanWriteFile(outputFileName.c_str()));

    ITK_TEST_EXPECT_TRUE(!scancoIO->CanWriteFile((outputFileName + ".exe").c_str()));
    writer->SetImageIO(scancoIO);
  }
  writer->SetInput(image);
  writer->SetFileName(outputFileName);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
