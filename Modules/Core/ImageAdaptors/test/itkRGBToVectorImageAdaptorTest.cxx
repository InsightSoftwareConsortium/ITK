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
/**
 *
 *  This program tests the RGBToVectorImageAdaptor.
 *
 *  This class allows to take an image of pixel type RGBPixel,
 *  and use it as if it was an image of pixel type itk::Vector<>.
 *
 */


#include "itkRGBToVectorImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"

//-------------------------
//
//   Main code
//
//-------------------------
int itkRGBToVectorImageAdaptorTest(int, char* [] )
{


//-------------------------------------
//     Typedefs for convenience
//-------------------------------------

  typedef float  ComponentType;

  typedef itk::RGBPixel< ComponentType >   RGBPixelType;

  const unsigned int ImageDimension = 2;

  typedef itk::Image< RGBPixelType, ImageDimension >  ImageType;

  typedef itk::RGBToVectorImageAdaptor< ImageType >   ImageAdaptorType;

  typedef itk::ImageRegionIteratorWithIndex< ImageType >       IteratorType;

  typedef itk::ImageRegionIteratorWithIndex< ImageAdaptorType >  RedIteratorType;

  ImageType::SizeType size;
  size[0] = 2;
  size[1] = 2;

  ImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  ImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  ImageType::Pointer image = ImageType::New();


  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->SetRequestedRegion( region );
  image->Allocate();

  IteratorType  it1( image, image->GetRequestedRegion() );

  // Value to initialize the pixels
  ImageType::PixelType color;
  color.SetRed(   0.7 );
  color.SetGreen( 0.8 );
  color.SetBlue(  0.5 );

  // Initializing all the pixel in the image
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    it1.Set(color);
    ++it1;
  }

  // Reading the values to verify the image content
  std::cout << "--- Before --- " << std::endl;
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    const ImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
  }

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
  adaptor->SetImage( image );


  RedIteratorType  it2( adaptor, adaptor->GetRequestedRegion() );

  // Set the values of the image, using the adaptor

  typedef ImageAdaptorType::AccessorType::ExternalType  PixelVectorType;

  PixelVectorType vector;

  vector[0] = 13;
  vector[1] = 17;
  vector[2] = 19;

  it2.GoToBegin();
  while( !it2.IsAtEnd() )
  {
    it2.Set( vector );
    ++it2;
  }


  std::cout << "--- After --- " << std::endl;

  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    const ImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
  }


  return EXIT_SUCCESS;
}
