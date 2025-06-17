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
itkScancoImageIOTest3(int argc, char * argv[])
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
  const std::string versionString = (argc > 4) ? argv[4] : "AIMDATA_V020   ";

  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  constexpr unsigned int Dimension = 3;
  using PixelType = short;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();

  // force use of ScancoIO
  using IOType = itk::ScancoImageIO;
  IOType::Pointer scancoIO = IOType::New();

  itk::ScancoImageIOFactory::RegisterOneFactory();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(scancoIO, ScancoImageIO, ImageIOBase);


  reader->SetImageIO(scancoIO);

  // read the file
  reader->SetFileName(inputFileName);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());
  ImageType::Pointer image = reader->GetOutput();

  std::cout << "Version: \t\t" << scancoIO->GetVersion() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetVersion(), versionString);
  std::cout << "PatientIndex: \t" << scancoIO->GetPatientIndex() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetPatientIndex(), 2573);
  std::cout << "ScannerID: \t\t" << scancoIO->GetScannerID() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetScannerID(), 3401);
  std::cout << "SliceThickness: \t" << scancoIO->GetSliceThickness() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSliceThickness(), 0.0607, 6, 1e-3));
  std::cout << "SliceIncrement: \t" << scancoIO->GetSliceIncrement() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSliceIncrement(), 0.0607, 6, 1e-3));
  std::cout << "StartPosition: \t" << scancoIO->GetStartPosition() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetStartPosition(), 114.845, 6, 1e-3));
  std::cout << "DataRange[0]: \t" << scancoIO->GetDataRange()[0] << std::endl;
  std::cout << "DataRange[1]: \t" << scancoIO->GetDataRange()[1] << std::endl;

  if (scancoIO->GetComponentType() == itk::ImageIOBase::FLOAT)
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[0], -1380.0, 6, 1e-3));
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[1], 8823.0, 6, 1e-3));
  }
  else
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[0], -2478.0, 6, 1e-3));
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[1], 11662.0, 6, 1e-3));
  }

  std::cout << "MuScaling: \t\t" << scancoIO->GetMuScaling() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetMuScaling(), 8192.0, 6, 1e-3));
  std::cout << "MuWater: \t\t" << scancoIO->GetMuWater() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetMuWater(), 0.24090, 6, 1e-3));
  std::cout << "RescaleType: \t\t" << scancoIO->GetRescaleType() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetRescaleType(), 2);
  std::cout << "RescaleSlope: \t\t" << scancoIO->GetRescaleSlope() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetRescaleSlope(), 1603.51904, 6, 1e-3));
  std::cout << "RescaleIntercept: \t\t" << scancoIO->GetRescaleIntercept() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetRescaleIntercept(), -391.209015, 6, 1e-3));
  std::cout << "RescaleUnits: \t\t" << scancoIO->GetRescaleUnits() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetRescaleUnits(), std::string("mg HA/ccm"));
  std::cout << "CalibrationData: \t\t" << scancoIO->GetCalibrationData() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetCalibrationData(), std::string("68 kVp, BH: 200 mg HA/ccm, Scaling 8192, 0.2 CU"));
  std::cout << "NumberOfSamples: \t" << scancoIO->GetNumberOfSamples() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetNumberOfSamples(), 2304);
  std::cout << "NumberOfProjections: " << scancoIO->GetNumberOfProjections() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetNumberOfProjections(), 900);
  std::cout << "ScanDistance: \t" << scancoIO->GetScanDistance() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetScanDistance(), 139.852, 6, 1e-3));
  std::cout << "ScannerType: \t" << scancoIO->GetScannerType() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetScannerType(), 9);
  std::cout << "SampleTime: \t\t" << scancoIO->GetSampleTime() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSampleTime(), 43.0, 6, 1e-3));
  std::cout << "MeasurementIndex: \t" << scancoIO->GetMeasurementIndex() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetMeasurementIndex(), 12839);
  std::cout << "Site: \t\t" << scancoIO->GetSite() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetSite(), 21);
  std::cout << "ReferenceLine: \t" << scancoIO->GetReferenceLine() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetReferenceLine(), 105);
  std::cout << "ReconstructionAlg: \t" << scancoIO->GetReconstructionAlg() << std::endl;
  ITK_TEST_EXPECT_EQUAL(scancoIO->GetReconstructionAlg(), 3);
  std::cout << "Energy: \t\t" << scancoIO->GetEnergy() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetEnergy(), 68.0, 6, 1e-3));
  std::cout << "Intensity: \t\t" << scancoIO->GetIntensity() << std::endl;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetIntensity(), 1.470, 6, 1e-3));
  std::cout << "CreationDate: \t" << scancoIO->GetCreationDate() << std::endl;
  std::cout << "ModificationDate: \t" << scancoIO->GetModificationDate() << std::endl;

  // Generate test image
  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  if (argc > 3 && std::stoi(argv[3]) == 1) // Explicitly use scancoIO
  {
    ITK_TEST_EXPECT_TRUE(scancoIO->CanWriteFile(outputFileName.c_str()));

    ITK_TEST_EXPECT_TRUE(!scancoIO->CanWriteFile((outputFileName + ".exe").c_str()));

    writer->SetImageIO(scancoIO);
    scancoIO->SetPatientName("Kuczynski");
  }
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(outputFileName);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
