/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkGDCMImageIOTest(int argc, char* argv[])
{

  if(argc < 6)
    {
    std::cerr << "Usage: " << argv[0] << " DicomImage OutputDicomImage OutputImage RescaledDicomImage RescaledOutputImage\n";
    return EXIT_FAILURE;
    }
  const char * dicomImageFileName = argv[1];
  const char * outputDicomFileName = argv[2];
  const char * outputImageFileName = argv[3];
  const char * rescaledDicomFileName = argv[4];
  const char * rescaledOutputImageFileName = argv[5];

  const unsigned int Dimension = 2;
  typedef short                                   InputPixelType;
  typedef itk::Image< InputPixelType, Dimension > InputImageType;

  typedef itk::GDCMImageIO                        ImageIOType;
  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();

  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( dicomImageFileName );
  reader->SetImageIO( gdcmImageIO );
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error: exception in file reader " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise the get methods
  std::cout << "InternalComponentType: "
    << gdcmImageIO->GetInternalComponentType() << std::endl;
  std::cout << "RescaleSlope: "
    << gdcmImageIO->GetRescaleSlope() << std::endl;
  std::cout << "RescaleIntercept: "
    << gdcmImageIO->GetRescaleIntercept() << std::endl;
  std::cout << "UIDPrefix: "
    << gdcmImageIO->GetUIDPrefix() << std::endl;
  std::cout << "StudyInstanceUID: "
    << gdcmImageIO->GetStudyInstanceUID() << std::endl;
  std::cout << "SeriesInstanceUID: "
    << gdcmImageIO->GetSeriesInstanceUID() << std::endl;
  std::cout << "FrameOfReferenceInstanceUID: "
    << gdcmImageIO->GetFrameOfReferenceInstanceUID() << std::endl;
  std::cout << "KeepOriginalUID: "
    << gdcmImageIO->GetKeepOriginalUID() << std::endl;
  std::cout << "LoadPrivateTags: "
    << gdcmImageIO->GetLoadPrivateTags() << std::endl;
  std::cout << "CompressionType: "
    << gdcmImageIO->GetCompressionType() << std::endl;

  // Test itk::GDCMImageIO::GetValueFromTag with upper and lower case tagkeys
  std::string tagkeyLower = "0008|103e"; // Series Description
  std::string valueFromLower;
  gdcmImageIO->GetValueFromTag(tagkeyLower, valueFromLower);
  std::string tagkeyUpper = "0008|103E"; // Series Description
  std::string valueFromUpper;
  gdcmImageIO->GetValueFromTag(tagkeyUpper, valueFromUpper);
  // We can't easily verify the content of the tag value against a known
  // baseline, as this test is run multiple times with different input images
  if(valueFromLower != valueFromUpper)
    {
    std::cerr << "itk::GDCMImageIO::GetValueFromTag produces different values for upper and lowercase tags" << std::endl;
    return EXIT_FAILURE;
    }

  // Rewrite the image in DICOM format
  typedef itk::ImageFileWriter< InputImageType > DicomWriterType;
  DicomWriterType::Pointer dicomWriter = DicomWriterType::New();
  dicomWriter->SetFileName( outputDicomFileName );
  dicomWriter->SetInput( reader->GetOutput() );
  dicomWriter->UseInputMetaDataDictionaryOff();
  dicomWriter->SetImageIO( gdcmImageIO );

  try
    {
    dicomWriter->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileReader< InputImageType > GeneratedDicomReaderType;
  GeneratedDicomReaderType::Pointer generatedDicomReader = GeneratedDicomReaderType::New();
  generatedDicomReader->SetFileName( outputDicomFileName );

  typedef itk::ImageFileWriter< InputImageType > MetaWriterType;
  MetaWriterType::Pointer metaWriter = MetaWriterType::New();
  metaWriter->SetFileName( outputImageFileName );
  metaWriter->SetInput( generatedDicomReader->GetOutput() );
  try
    {
    metaWriter->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
    }


  // Rescale intensities and rewrite the image in another format
  //
  typedef unsigned char                              RescaledPixelType;
  typedef itk::Image< RescaledPixelType, Dimension > RescaledImageType;
  typedef itk::RescaleIntensityImageFilter<
    InputImageType, RescaledImageType >              RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( reader->GetOutput() );

  // Rewrite the image in DICOM format but using less bits per pixel
  //
  typedef itk::ImageFileWriter< RescaledImageType > RescaledDicomWriterType;
  RescaledDicomWriterType::Pointer rescaledDicomWriter = RescaledDicomWriterType::New();
  rescaledDicomWriter->SetFileName( rescaledDicomFileName );
  rescaledDicomWriter->SetInput( rescaler->GetOutput() );
  rescaledDicomWriter->UseInputMetaDataDictionaryOff();
  itk::MetaDataDictionary & dict = gdcmImageIO->GetMetaDataDictionary();
  std::ostringstream ostrm;
  ostrm << itk::Math::Round< int, double >( -1. * rescaler->GetShift() );
  itk::EncapsulateMetaData< std::string >( dict, "0028|1052", ostrm.str() );
  ostrm.str( "" );
  ostrm << itk::Math::Ceil< int, double >( 1. / rescaler->GetScale() );
  itk::EncapsulateMetaData< std::string >( dict, "0028|1053", ostrm.str() );
  gdcmImageIO->SetInternalComponentType( itk::ImageIOBase::UCHAR );
  rescaledDicomWriter->SetImageIO( gdcmImageIO );

  gdcmImageIO->Print( std::cout );

  try
    {
    rescaledDicomWriter->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileReader< RescaledImageType > GeneratedRescaledDicomReaderType;
  GeneratedRescaledDicomReaderType::Pointer rescaledDicomReader = GeneratedRescaledDicomReaderType::New();
  rescaledDicomReader->SetFileName( rescaledDicomFileName );

  typedef itk::ImageFileWriter< RescaledImageType >  RescaledMetaWriterType;
  RescaledMetaWriterType::Pointer rescaledMetaWriter = RescaledMetaWriterType::New();
  rescaledMetaWriter->SetFileName( rescaledOutputImageFileName );
  rescaledMetaWriter->SetInput( rescaledDicomReader->GetOutput() );

  try
    {
    rescaledMetaWriter->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error: exception in file writer " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
