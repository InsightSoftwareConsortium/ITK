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

#include <fstream>


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkGDCMImageIOTest(int ac, char* av[])
{

  if(ac < 5)
    {
    std::cerr << "Usage: " << av[0] << " DicomImage OutputDicomImage OutputImage RescalDicomImage\n";
    return EXIT_FAILURE;
    }


  typedef short                                   InputPixelType;
  typedef itk::Image< InputPixelType, 2 >         InputImageType;
  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::GDCMImageIO                        ImageIOType;
  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );
  reader->SetImageIO( gdcmImageIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
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
  //
  typedef itk::ImageFileWriter< InputImageType >  Writer1Type;
  Writer1Type::Pointer writer1 = Writer1Type::New();
  writer1->SetFileName( av[2] );
  writer1->SetInput( reader->GetOutput() );
  writer1->SetImageIO( gdcmImageIO );

  try
    {
    writer1->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Rescale intensities and rewrite the image in another format
  //
  typedef unsigned char                   WritePixelType;
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  typedef itk::RescaleIntensityImageFilter<
    InputImageType, WriteImageType >      RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( reader->GetOutput() );

  typedef itk::ImageFileWriter< WriteImageType >  Writer2Type;
  Writer2Type::Pointer writer2 = Writer2Type::New();
  writer2->SetFileName( av[3] );
  writer2->SetInput( rescaler->GetOutput() );

  try
    {
    writer2->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Rewrite the image in DICOM format but using less bits per pixel
  //
  typedef itk::ImageFileWriter< WriteImageType >  Writer3Type;

  Writer3Type::Pointer writer3 = Writer3Type::New();
  writer3->SetFileName( av[4] );
  writer3->SetInput( rescaler->GetOutput() );
  writer3->UseInputMetaDataDictionaryOff ();
  writer3->SetImageIO( gdcmImageIO );

  try
    {
    writer3->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  gdcmImageIO->Print( std::cout );

  return EXIT_SUCCESS;

}
