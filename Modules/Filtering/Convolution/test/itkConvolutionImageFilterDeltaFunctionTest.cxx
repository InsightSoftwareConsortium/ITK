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

#include "itkConvolutionImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkConvolutionImageFilterDeltaFunctionTest(int argc, char * argv[])
{
  if ( argc < 3 )
    {
    std::cout << "Usage: " << argv[0] << " kernelImage outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;

  typedef unsigned char                            PixelType;
  typedef itk::Image< PixelType, ImageDimension >  ImageType;
  typedef itk::ImageFileReader< ImageType >        ReaderType;

  // Read kernel image.
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  // Set up delta function image.
  ImageType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
  ImageType::Pointer deltaFunctionImage = ImageType::New();
  deltaFunctionImage->SetRegions( region );
  deltaFunctionImage->Allocate();
  deltaFunctionImage->FillBuffer( 0 );

  // Set the middle pixel (rounded up) to 1.
  ImageType::IndexType middleIndex;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    ImageType::SizeValueType sizeInDimension = region.GetSize()[i];
    middleIndex[i] =
      itk::Math::Floor< ImageType::IndexValueType >( 0.5 * sizeInDimension );
    }
  deltaFunctionImage->SetPixel( middleIndex, 1 );

  typedef itk::ConvolutionImageFilter<ImageType> ConvolutionFilterType;
  ConvolutionFilterType::Pointer convolver
    = ConvolutionFilterType::New();
  convolver->SetInput( deltaFunctionImage );
  convolver->SetImageKernelInput( reader->GetOutput() );

  // Flip output of the convolver and save the file. We should get
  // output identical to the input kernel image.
  itk::FlipImageFilter< ImageType >::Pointer flipper =
    itk::FlipImageFilter< ImageType >::New();
  itk::FlipImageFilter< ImageType >::FlipAxesArrayType axesArray;
  axesArray.Fill( true );
  flipper->SetFlipAxes( axesArray );
  flipper->SetInput( convolver->GetOutput() );

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( flipper->GetOutput() );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
