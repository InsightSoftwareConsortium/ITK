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
 *  This program tests the VectorToRGBImageAdaptor.
 *
 *  This class allows to take an image of pixel type RGBPixel,
 *  and use it as if it was an image of pixel type itk::Vector<>.
 *
 */


#include "itkVectorToRGBImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"

//-------------------------
//
//   Main code
//
//-------------------------
int itkVectorToRGBImageAdaptorTest(int, char* [] ) {


//-------------------------------------
//     Typedefs for convenience
//-------------------------------------

typedef float  ValueType;

const unsigned int numberOfComponents = 3;

typedef itk::Vector< ValueType,
                     numberOfComponents >   VectorPixelType;

const unsigned int ImageDimension = 2;

typedef itk::Image< VectorPixelType, ImageDimension >         ImageType;
typedef itk::VectorToRGBImageAdaptor< ImageType >             ImageAdaptorType;
typedef itk::ImageRegionIteratorWithIndex< ImageType >        IteratorType;
typedef itk::ImageRegionIteratorWithIndex< ImageAdaptorType > RGBIteratorType;

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
  ImageType::PixelType vector;
  vector[0] = 1.2;
  vector[1] = 1.3;
  vector[2] = 1.4;

  // Initializing all the pixel in the image
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    it1.Set( vector );
    ++it1;
  }

  // Reading the values to verify the image content
  std::cout << "--- Before --- " << std::endl;
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    const ImageType::PixelType c( it1.Get() );
    std::cout << c[0] << "  ";
    std::cout << c[1] << "  ";
    std::cout << c[2] << std::endl;
    ++it1;
  }

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
  adaptor->SetImage( image );


  RGBIteratorType  it2( adaptor, adaptor->GetRequestedRegion() );

  // Set the values of the image, using the adaptor

  typedef ImageAdaptorType::AccessorType::ExternalType  RGBPixelType;

  RGBPixelType color;

  color[0] = 13;
  color[1] = 17;
  color[2] = 19;

  it2.GoToBegin();
  while( !it2.IsAtEnd() )
  {
    it2.Set( color );
    ++it2;
  }


  std::cout << "--- After --- " << std::endl;

  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    const ImageType::PixelType c( it1.Get() );
    std::cout << c[0] << "  ";
    std::cout << c[1] << "  ";
    std::cout << c[2] << std::endl;
    ++it1;
  }


  return EXIT_SUCCESS;
}
