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
 *  This program illustrates the AdaptImageFilter
 *
 *  The example shows how an Accessor can be used to
 *  convert an RGBPixel image to an image that has
 *  vector pixel type.
 *
 *  This allows to access an RGB image a an image of vectors.
 *
 */


#include "itkAdaptImageFilter.h"
#include "itkRGBToVectorPixelAccessor.h"

#include "vnl/vnl_sample.h"
#include "itkMath.h"


//-------------------------
//
//   Main code
//
//-------------------------
int itkRGBToVectorAdaptImageFilterTest(int, char* [] ) {


  //-------------------------------------
  //     Typedefs for convenience
  //-------------------------------------
  typedef itk::RGBPixel< float >                  RGBPixelType;
  typedef itk::Image< RGBPixelType,   2 >         RGBImageType;

  typedef itk::ImageRegionIteratorWithIndex< RGBImageType >  myRGBIteratorType;

  typedef itk::Accessor::RGBToVectorPixelAccessor<float>    AccessorType;

  typedef AccessorType::ExternalType              VectorPixelType;

  typedef itk::Image< VectorPixelType,   2 >      myImageType;

  typedef itk::ImageRegionIteratorWithIndex< myImageType >  myVectorIteratorType;

  RGBImageType::SizeType size;
  size[0] = 100;
  size[1] = 100;

  RGBImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  RGBImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  RGBImageType::Pointer myImage = RGBImageType::New();


  myImage->SetRegions( region );
  myImage->Allocate();

  myRGBIteratorType  it1( myImage, myImage->GetRequestedRegion() );

  // Value to initialize the pixels
  RGBImageType::PixelType color;

  // Initializing all the pixel in the image
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    color.Set( (float) vnl_sample_uniform(0.0, 1.0),
               (float) vnl_sample_uniform(0.0, 1.0),
               (float) vnl_sample_uniform(0.0, 1.0) );
    it1.Set(color);
    ++it1;
    }

  bool passed = true;

  // Convert to a Vector image
  typedef itk::AdaptImageFilter< RGBImageType,
                                 myImageType,
                                 AccessorType   >  AdaptFilterType;

  AdaptFilterType::Pointer  adaptImageToVector = AdaptFilterType::New();

  adaptImageToVector->SetInput(myImage);
  adaptImageToVector->UpdateLargestPossibleRegion();

  myVectorIteratorType  it(
            adaptImageToVector->GetOutput(),
            adaptImageToVector->GetOutput()->GetRequestedRegion() );

  std::cout << "--- Read Vector values --- " << std::endl;

  it.GoToBegin();
  it1.GoToBegin();
  while( !it.IsAtEnd() )
  {
  VectorPixelType v =   it.Get();
  RGBPixelType    c =  it1.Get();

  if ( itk::Math::NotExactlyEquals(v[0], c.GetRed())   ||
       itk::Math::NotExactlyEquals(v[1], c.GetGreen()) ||
       itk::Math::NotExactlyEquals(v[2], c.GetBlue())     )
    {
    std::cerr << "Vector pixel = " << v << std::endl;
    std::cerr << "does not match " << std::endl;
    std::cerr << "RGB    pixel = " << c << std::endl;
    std::cerr << "myImage->GetRequestedRegion()" << myImage->GetRequestedRegion() << std::endl;
    std::cerr << "adaptImageToVector->GetRequestedRegion()" << adaptImageToVector->GetOutput()->GetRequestedRegion() << std::endl;
    passed = false;
    break;
    }

  ++it;
  ++it1;
  }

  std::cout << std::endl;
  if (passed)
    {
    std::cout << "AdaptImageFilterTest passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "AdaptImageFilterTest passed" << std::endl;
    return EXIT_FAILURE;
    }
}
