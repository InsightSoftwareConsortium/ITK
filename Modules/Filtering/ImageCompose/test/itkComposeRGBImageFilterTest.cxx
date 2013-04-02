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

#include <iostream>
#include "itkRGBPixel.h"
#include "itkComposeImageFilter.h"

int itkComposeRGBImageFilterTest(int , char * [])
{
  typedef unsigned char              PixelType;
  typedef itk::Image< PixelType, 3 > InputImageType;

  typedef itk::RGBPixel<unsigned char>   RGBPixelType;
  typedef itk::Image< RGBPixelType, 3 >  OutputImageType;


  typedef itk::ComposeImageFilter< InputImageType, OutputImageType >  FilterType;

  typedef InputImageType::RegionType RegionType;
  typedef InputImageType::SizeType   SizeType;
  typedef InputImageType::IndexType  IndexType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer redImage   = InputImageType::New();
  InputImageType::Pointer greenImage = InputImageType::New();
  InputImageType::Pointer blueImage  = InputImageType::New();

  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start.Fill( 0 );

  RegionType region;
  region.SetIndex( start );
  region.SetSize(  size  );

  redImage->SetRegions( region );
  greenImage->SetRegions( region );
  blueImage->SetRegions( region );

  redImage->Allocate();
  greenImage->Allocate();
  blueImage->Allocate();

  redImage->FillBuffer( 29 );
  greenImage->FillBuffer( 51 );
  blueImage->FillBuffer( 83 );

  filter->SetInput1( redImage );
  filter->SetInput2( greenImage );
  filter->SetInput3( blueImage );

  try
    {
    filter->Update();
    }

  catch( itk::ExceptionObject & excp )
   {
   std::cerr << "Exception caught !" << std::endl;
   std::cerr << excp << std::endl;
   return EXIT_FAILURE;
   }

  OutputImageType::Pointer rgbImage = filter->GetOutput();

  typedef itk::ImageRegionIterator<OutputImageType> OutputIterator;
  typedef itk::ImageRegionIterator<InputImageType>  InputIterator;

  InputIterator ir( redImage,   region );
  InputIterator ig( greenImage, region );
  InputIterator ib( blueImage,  region );

  OutputIterator ot( rgbImage,  region );

  ir.GoToBegin();
  ig.GoToBegin();
  ib.GoToBegin();

  ot.GoToBegin();

  typedef OutputImageType::PixelType  OutputPixelType;

  while( !ot.IsAtEnd() )
    {
    OutputPixelType outp = ot.Get();
    if( ir.Get() != outp.GetRed() )
      {
      std::cerr << "Error in red component" << std::endl;
      return EXIT_FAILURE;
      }
    if( ig.Get() != outp.GetGreen() )
      {
      std::cerr << "Error in green component" << std::endl;
      return EXIT_FAILURE;
      }
    if( ib.Get() != outp.GetBlue() )
      {
      std::cerr << "Error in blue component" << std::endl;
      return EXIT_FAILURE;
      }
    ++ot;
    ++ir;
    ++ig;
    ++ib;
    }

  std::cout << "Test Passed !" << std::endl;

  return EXIT_SUCCESS;

}
