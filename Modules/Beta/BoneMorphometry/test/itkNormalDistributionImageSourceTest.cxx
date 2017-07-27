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

#include "itkNormalDistributionImageSource.h"

#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int itkNormalDistributionImageSourceTest( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImage";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  const char * outputImageFileName  = argv[1];

  const unsigned int                         Dimension = 2;
  typedef float                              PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::NormalDistributionImageSource< ImageType > DistributionSourceType;
  DistributionSourceType::Pointer distributionSource = DistributionSourceType::New();

  EXERCISE_BASIC_OBJECT_METHODS( distributionSource, NormalDistributionImageSource , GenerateImageSource );


  ImageType::SizeType size;
  size.Fill( 128 );
  distributionSource->SetSize( size );

  std::cout << distributionSource << std::endl;

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputImageFileName );
  writer->SetInput( distributionSource->GetOutput() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
