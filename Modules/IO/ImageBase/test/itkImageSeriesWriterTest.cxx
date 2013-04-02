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

#include "itkGDCMImageIO.h"
#include "itkImageSeriesReader.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageSeriesWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkFilterWatcher.h"

int itkImageSeriesWriterTest(int ac, char* av[])
{

  if(ac < 4)
  {
    std::cerr << "Usage: " << av[0] << " DicomDirectory OutputDirectory FileSuffix\n";
    return EXIT_FAILURE;
  }

  typedef itk::Image<short,3>                 ImageNDType;
  typedef itk::ImageSeriesReader<ImageNDType> ReaderType;

  itk::GDCMImageIO::Pointer io = itk::GDCMImageIO::New();

  // Get the DICOM filenames from the directory
  itk::GDCMSeriesFileNames::Pointer nameGenerator = itk::GDCMSeriesFileNames::New();
  nameGenerator->SetDirectory(av[1]);

  typedef std::vector< std::string >    SeriesIdContainer;
  const SeriesIdContainer & seriesUID = nameGenerator->GetSeriesUIDs();
  std::string seriesIdentifier = seriesUID.begin()->c_str();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileNames( nameGenerator->GetFileNames( seriesIdentifier ) );
  reader->SetImageIO( io );

  FilterWatcher watcher(reader);

  try
    {
    reader->Update();
    reader->GetOutput()->Print(std::cout);
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  typedef unsigned char                   WritePixelType;
  typedef itk::Image< WritePixelType, 3 > RescaleImageType;
  typedef itk::Image< WritePixelType, 2 > OutputImageType;

  typedef itk::RescaleIntensityImageFilter<
               ImageNDType, RescaleImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
    rescaler->SetInput(reader->GetOutput());
    rescaler->SetOutputMinimum(   0 );
    rescaler->SetOutputMaximum( 255 );
    rescaler->UpdateLargestPossibleRegion();


  { // This API is being deprecated. Please use NumericSeriesFileNames in the future
    // for generating the list of filenames.  This API will be removed after ITK 1.8
  typedef  itk::ImageSeriesWriter<RescaleImageType,OutputImageType> WriterType;

  WriterType::Pointer writer = WriterType::New();

  FilterWatcher watcher2(writer);

  writer->SetInput(rescaler->GetOutput());
  char format[4096];
  sprintf (format, "%s/series.%%d.%s", av[2], av[3]);
  writer->SetSeriesFormat(format);

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the series with old API" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Verify that attempting to use a MetaDataDictionary without setting the ImageIO
  // should throw an exception.
  writer->SetMetaDataDictionaryArray( reader->GetMetaDataDictionaryArray() );
  try
    {
    writer->Update();
    std::cerr << "Failed to throw expected exception of using MetaDataDictionary without ImageIO" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Exercised expected exception" << std::endl;
    }

  std::cout << "Old API PASSED !" << std::endl;
  }

  { // This is the new API, using the NumericSeriesFileNames (or any other filename generator).
  itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

  typedef  itk::ImageSeriesWriter<RescaleImageType,OutputImageType> WriterType;

  WriterType::Pointer writer = WriterType::New();


  char format[4096];
  sprintf (format, "%s/series.%%d.%s", av[2], av[3]);

  std::cout << "Format = " << format << std::endl;

  ImageNDType::RegionType region = reader->GetOutput()->GetBufferedRegion();
  ImageNDType::SizeType   size = region.GetSize();

  fit->SetStartIndex(0);
  fit->SetEndIndex(size[2]-1);  // The number of slices to write
  fit->SetIncrementIndex(1);
  fit->SetSeriesFormat (format);

  writer->SetInput(rescaler->GetOutput());
  writer->SetFileNames(  fit->GetFileNames() );

  // experiment the UseCompression methods and values
  if( writer->GetUseCompression() )
    {
    std::cerr << "Wrong default use compression value" << std::endl;
    return EXIT_FAILURE;
    }
  writer->SetUseCompression( true );
  writer->UseCompressionOn();
  writer->UseCompressionOff();

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the series with SeriesFileNames generator" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Verify that attempting to use a MetaDataDictionary without setting the ImageIO
  // should throw an exception.
  writer->SetMetaDataDictionaryArray( reader->GetMetaDataDictionaryArray() );
  try
    {
    writer->Update();
    std::cerr << "Failed to throw expected exception of using MetaDataDictionary without ImageIO" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Exercised expected exception" << std::endl;
    }

  std::cout << "Test with NumericSeriesFileNames PASSED !" << std::endl;
  }

  return EXIT_SUCCESS;

}
