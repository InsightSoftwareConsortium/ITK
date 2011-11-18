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
#include "itkImageSeriesWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkJPEG2000ImageIOFactory.h"

#include <fstream>

int itkJPEG2000ImageIOTest05( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " input outputdir extension" << std::endl;
    return EXIT_FAILURE;
    }


  typedef unsigned char             PixelType;
  typedef itk::Image<PixelType,3>   ImageType;
  typedef itk::Image<PixelType,2>   OutputImageType;

  typedef itk::ImageFileReader<ImageType>         ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  //reader->SetUseStreaming( true );

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

  //  Register the factory
  itk::JPEG2000ImageIOFactory::RegisterOneFactory();

  itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

  typedef  itk::ImageSeriesWriter<ImageType,OutputImageType> WriterType;

  WriterType::Pointer writer = WriterType::New();


  char format[4096];
  sprintf (format, "%s/series.%%d.%s", argv[2], argv[3]);

  std::cout << "Format = " << format << std::endl;

  ImageType::RegionType region = reader->GetOutput()->GetBufferedRegion();
  ImageType::SizeType   size = region.GetSize();

  fit->SetStartIndex(0);
  fit->SetEndIndex(size[2]-1);  // The number of slices to write
  fit->SetIncrementIndex(1);
  fit->SetSeriesFormat (format);

  writer->SetInput(reader->GetOutput());
  writer->SetFileNames(  fit->GetFileNames() );

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

  return EXIT_SUCCESS;
}
