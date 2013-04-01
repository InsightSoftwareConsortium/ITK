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
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

int itkPromoteDimensionImageTest(int argc, char* argv[])
{
  if( argc < 3)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return -1;
    }

  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];

  typedef unsigned char    CharPixelType;  //IO
  typedef double          RealPixelType;  //Operations

  const    unsigned int    InDimension = 2;
  const    unsigned int    OutDimension = 3;

  typedef itk::Image<CharPixelType, InDimension>    InCharImageType;
  typedef itk::Image<CharPixelType, OutDimension>   OutCharImageType;
  typedef itk::Image<RealPixelType, InDimension>    RealImageType;

  typedef itk::ImageFileReader< InCharImageType >   ReaderType;
  typedef itk::ImageFileWriter< OutCharImageType >  WriterType;

  typedef itk::CastImageFilter<InCharImageType, RealImageType> CastToRealFilterType;
  typedef itk::CastImageFilter<RealImageType, OutCharImageType> CastToCharFilterType;

  typedef itk::RescaleIntensityImageFilter<RealImageType, RealImageType> RescaleFilter;

  //Setting the IO
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  CastToRealFilterType::Pointer toReal = CastToRealFilterType::New();
  CastToCharFilterType::Pointer toChar = CastToCharFilterType::New();
  RescaleFilter::Pointer rescale = RescaleFilter::New();

  //Setting the ITK pipeline filter

  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );

  //The output of an edge filter is 0 or 1
  rescale->SetOutputMinimum(   0 );
  rescale->SetOutputMaximum( 255 );

  toReal->SetInput( reader->GetOutput() );
  rescale->SetInput( toReal->GetOutput() );
  toChar->SetInput( rescale->GetOutput() );
  writer->SetInput( toChar->GetOutput() );

  try
    {
    writer->Update();
    //toChar->GetOutput()->Print(std::cout);
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return -1;
    }

  return EXIT_SUCCESS;

}
