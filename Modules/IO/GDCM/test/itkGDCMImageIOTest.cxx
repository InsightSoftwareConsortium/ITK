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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGDCMImageIO.h"
#include "itkMetaDataObject.h"
#include "itkTestingMacros.h"

// Specific ImageIO test

int
itkGDCMImageIOTest(int argc, char * argv[])
{

  if (argc < 6)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " DicomImage OutputDicomImage OutputImage RescaledDicomImage RescaledOutputImage\n";
    return EXIT_FAILURE;
  }
  const char * dicomImageFileName = argv[1];
  const char * outputDicomFileName = argv[2];
  const char * outputImageFileName = argv[3];
  const char * rescaledDicomFileName = argv[4];
  const char * rescaledOutputImageFileName = argv[5];

  constexpr unsigned int Dimension = 2;
  using InputPixelType = short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;

  using ImageIOType = itk::GDCMImageIO;
  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(dicomImageFileName);
  reader->SetImageIO(gdcmImageIO);
  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Error: exception in file reader " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise the get methods
  std::cout << "InternalComponentType: " << gdcmImageIO->GetInternalComponentType() << std::endl;
  std::cout << "RescaleSlope: " << gdcmImageIO->GetRescaleSlope() << std::endl;
  std::cout << "RescaleIntercept: " << gdcmImageIO->GetRescaleIntercept() << std::endl;
  std::cout << "UIDPrefix: " << gdcmImageIO->GetUIDPrefix() << std::endl;
  std::cout << "StudyInstanceUID: " << gdcmImageIO->GetStudyInstanceUID() << std::endl;
  std::cout << "SeriesInstanceUID: " << gdcmImageIO->GetSeriesInstanceUID() << std::endl;
  std::cout << "FrameOfReferenceInstanceUID: " << gdcmImageIO->GetFrameOfReferenceInstanceUID() << std::endl;
  std::cout << "KeepOriginalUID: " << gdcmImageIO->GetKeepOriginalUID() << std::endl;
  std::cout << "LoadPrivateTags: " << gdcmImageIO->GetLoadPrivateTags() << std::endl;
  std::cout << "CompressionType: " << gdcmImageIO->GetCompressionType() << std::endl;

  // Test itk::GDCMImageIO::GetValueFromTag with upper and lower case tagkeys
  std::string tagkeyLower = "0008|103e"; // Series Description
  std::string valueFromLower;
  gdcmImageIO->GetValueFromTag(tagkeyLower, valueFromLower);
  std::string tagkeyUpper = "0008|103E"; // Series Description
  std::string valueFromUpper;
  gdcmImageIO->GetValueFromTag(tagkeyUpper, valueFromUpper);
  // We can't easily verify the content of the tag value against a known
  // baseline, as this test is run multiple times with different input images
  if (valueFromLower != valueFromUpper)
  {
    std::cerr << "itk::GDCMImageIO::GetValueFromTag produces different values for upper and lowercase tags"
              << std::endl;
    return EXIT_FAILURE;
  }

  // Rewrite the image in DICOM format
  using DicomWriterType = itk::ImageFileWriter<InputImageType>;
  DicomWriterType::Pointer dicomWriter = DicomWriterType::New();
  dicomWriter->SetFileName(outputDicomFileName);
  dicomWriter->SetInput(reader->GetOutput());
  dicomWriter->UseInputMetaDataDictionaryOff();
  dicomWriter->SetImageIO(gdcmImageIO);

  try
  {
    dicomWriter->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  using GeneratedDicomReaderType = itk::ImageFileReader<InputImageType>;
  GeneratedDicomReaderType::Pointer generatedDicomReader = GeneratedDicomReaderType::New();
  generatedDicomReader->SetFileName(outputDicomFileName);

  using MetaWriterType = itk::ImageFileWriter<InputImageType>;
  MetaWriterType::Pointer metaWriter = MetaWriterType::New();
  metaWriter->SetFileName(outputImageFileName);
  metaWriter->SetInput(generatedDicomReader->GetOutput());
  try
  {
    metaWriter->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }


  // Rescale intensities and rewrite the image in another format
  //
  using RescaledPixelType = unsigned char;
  using RescaledImageType = itk::Image<RescaledPixelType, Dimension>;
  using RescaleFilterType = itk::RescaleIntensityImageFilter<InputImageType, RescaledImageType>;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);
  rescaler->SetInput(reader->GetOutput());

  // Rewrite the image in DICOM format but using less bits per pixel
  //
  using RescaledDicomWriterType = itk::ImageFileWriter<RescaledImageType>;
  RescaledDicomWriterType::Pointer rescaledDicomWriter = RescaledDicomWriterType::New();
  rescaledDicomWriter->SetFileName(rescaledDicomFileName);
  rescaledDicomWriter->SetInput(rescaler->GetOutput());
  rescaledDicomWriter->UseInputMetaDataDictionaryOff();
  itk::MetaDataDictionary & dict = gdcmImageIO->GetMetaDataDictionary();
  std::ostringstream        ostrm;
  ostrm << itk::Math::Round<int, double>(-1. * rescaler->GetShift());
  itk::EncapsulateMetaData<std::string>(dict, "0028|1052", ostrm.str());
  ostrm.str("");
  ostrm << itk::Math::Ceil<int, double>(1. / rescaler->GetScale());
  itk::EncapsulateMetaData<std::string>(dict, "0028|1053", ostrm.str());
  gdcmImageIO->SetInternalComponentType(itk::IOComponentEnum::UCHAR);
  rescaledDicomWriter->SetImageIO(gdcmImageIO);

  gdcmImageIO->Print(std::cout);

  try
  {
    rescaledDicomWriter->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  using GeneratedRescaledDicomReaderType = itk::ImageFileReader<RescaledImageType>;
  GeneratedRescaledDicomReaderType::Pointer rescaledDicomReader = GeneratedRescaledDicomReaderType::New();
  rescaledDicomReader->SetFileName(rescaledDicomFileName);

  using RescaledMetaWriterType = itk::ImageFileWriter<RescaledImageType>;
  RescaledMetaWriterType::Pointer rescaledMetaWriter = RescaledMetaWriterType::New();
  rescaledMetaWriter->SetFileName(rescaledOutputImageFileName);
  rescaledMetaWriter->SetInput(rescaledDicomReader->GetOutput());

  try
  {
    rescaledMetaWriter->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  // Test streaming enumeration for GDCMImageIOEnums::Compression elements
  const std::set<itk::GDCMImageIOEnums::Compression> allCompression{ itk::GDCMImageIOEnums::Compression::JPEG,
                                                                     itk::GDCMImageIOEnums::Compression::JPEG2000,
                                                                     itk::GDCMImageIOEnums::Compression::JPEGLS,
                                                                     itk::GDCMImageIOEnums::Compression::RLE };
  for (const auto & ee : allCompression)
  {
    std::cout << "STREAMED ENUM VALUE GDCMImageIOEnums::Compression: " << ee << std::endl;
  }

  return EXIT_SUCCESS;
}
