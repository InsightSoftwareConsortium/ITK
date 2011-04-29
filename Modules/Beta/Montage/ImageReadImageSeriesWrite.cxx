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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif



#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkNumericSeriesFileNames.h"

int main( int argc, char *argv[] )
{
  if (argc < 4 )
    {
    std::cerr << "Usage: ImageReadImageSeriesWrite inputFile outputPrefix outputExtension" << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::Image< unsigned short, 3 >      ImageType;
  typedef itk::ImageFileReader< ImageType >   ReaderType;
  typedef itk::Image< unsigned char, 3 >      CharImageType;


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  typedef itk::Image< unsigned char, 2 >     Image2DType;

  typedef itk::ImageSeriesWriter< CharImageType, Image2DType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  typedef itk::RescaleIntensityImageFilter< ImageType, CharImageType > RescaleIntensityType;
  RescaleIntensityType::Pointer rescaler = RescaleIntensityType::New();

  rescaler->SetInput( reader->GetOutput() );

  writer->SetInput( rescaler->GetOutput() );


  typedef itk::NumericSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();


  std::string format = argv[2];
  format += "%03d.";
  format += argv[3];   // filename extension

  nameGenerator->SetSeriesFormat( format.c_str() );


  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the image" << std::endl;
    std::cerr << excp << std::endl;
    }


  ImageType::ConstPointer inputImage = reader->GetOutput();
  ImageType::RegionType   region     = inputImage->GetLargestPossibleRegion();
  ImageType::IndexType    start      = region.GetIndex();
  ImageType::SizeType     size       = region.GetSize();


  const unsigned int firstSlice = start[2];
  const unsigned int lastSlice  = start[2] + size[2] - 1;

  nameGenerator->SetStartIndex( firstSlice );
  nameGenerator->SetEndIndex( lastSlice );
  nameGenerator->SetIncrementIndex( 1 );


  writer->SetFileNames( nameGenerator->GetFileNames() );


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the image" << std::endl;
    std::cerr << excp << std::endl;
    }


  return EXIT_SUCCESS;
}
