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

//
//  This example illustrates how to read a DICOM series into a volume
//  and then save this volume into another DICOM series using the
//  exact same name.
//  It makes use of the GDCM library
//

#define ITK_LEGACY_TEST
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"

int itkGDCMSeriesReadImageWriteTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] <<
      " DicomDirectory  outputFile OutputDicomDirectory" << std::endl;
    return EXIT_FAILURE;
    }

#if ! defined ( ITK_LEGACY_REMOVE )
  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;
  typedef itk::GDCMImageIO                        ImageIOType;
  typedef itk::GDCMSeriesFileNames                SeriesFileNames;

  ImageIOType::Pointer gdcmIO = ImageIOType::New();
  SeriesFileNames::Pointer it = SeriesFileNames::New();

  // Get the DICOM filenames from the directory
  // First add a restriction *before* selecting the input directory
  // since SetInputDirectory has a side effect of executing
  gdcm::SerieHelper *sh = it->GetSeriesHelper( );
  sh->AddRestriction(0x0010, 0x0010, "Wes Turner", gdcm::GDCM_EQUAL);
  sh->AddRestriction(0x0020, 0x0013, "75", gdcm::GDCM_GREATEROREQUAL);
  sh->AddRestriction(0x0020, 0x0013, "77", gdcm::GDCM_LESSOREQUAL);
  it->SetInputDirectory( argv[1] );

  ReaderType::Pointer reader = ReaderType::New();

  const ReaderType::FileNamesContainer & fileNames = it->GetInputFileNames();
  const size_t numberOfFileNames = fileNames.size();
  std::cout << numberOfFileNames << std::endl;
  for( unsigned int fni = 0; fni < numberOfFileNames; ++fni )
    {
    std::cout << "filename # " << fni << " = ";
    std::cout << fileNames[fni] << std::endl;
    }

  reader->SetFileNames( fileNames );
  reader->SetImageIO( gdcmIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;

    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->SetInput( reader->GetOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;

    return EXIT_FAILURE;
    }

  // Writing image afer downscaling to 8bits (unsigned char)

  typedef itk::Image< unsigned short, 3>            Image3DType;
  typedef itk::Image< unsigned char,  3>            RescaleImageType;
  typedef itk::Image< unsigned char,  2>            OutputImageType;
  typedef itk::RescaleIntensityImageFilter<
               Image3DType, RescaleImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
    rescaler->SetInput( reader->GetOutput() );
    rescaler->SetOutputMinimum(   0 );
    rescaler->SetOutputMaximum( 255 );

  typedef itk::ImageSeriesWriter< RescaleImageType, OutputImageType > SeriesWriterRescaleType;

  SeriesWriterRescaleType::Pointer swriter2 = SeriesWriterRescaleType::New();

  it->SetOutputDirectory( argv[3] );
  swriter2->SetInput( rescaler->GetOutput() );
  swriter2->SetImageIO( gdcmIO );
  gdcmIO->SetInternalComponentType( itk::ImageIOBase::UCHAR );

  swriter2->SetFileNames( it->GetOutputFileNames() );
  swriter2->SetMetaDataDictionaryArray( reader->GetMetaDataDictionaryArray() );

  try
    {
    swriter2->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
#endif

  return EXIT_SUCCESS;
}
