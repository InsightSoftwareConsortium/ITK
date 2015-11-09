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
 *  just the red component.
 *
 *  That will allow to pass the red component of this
 *  image as input any filter that expects
 *  a float image
 *
 */


#include "itkAdaptImageFilter.h"
#include "itkRedPixelAccessor.h"
#include "itkGreenPixelAccessor.h"
#include "itkBluePixelAccessor.h"
#include "itkFilterWatcher.h"

#include "vnl/vnl_sample.h"
#include "itkMath.h"


//-------------------------------------
//     Typedefs for convenience
//-------------------------------------
typedef itk::Image< itk::RGBPixel<float>,   2 >             myRGBImageType;
typedef itk::ImageRegionIteratorWithIndex< myRGBImageType > myRGBIteratorType;

typedef itk::RedPixelAccessor<float>   myRedAccessorType;
typedef itk::GreenPixelAccessor<float> myGreenAccessorType;
typedef itk::BluePixelAccessor<float>  myBlueAccessorType;

typedef itk::Image< float,   2 >                         myImageType;
typedef itk::ImageRegionIteratorWithIndex< myImageType > myIteratorType;

//-------------------------
//
//   Main code
//
//-------------------------
int itkAdaptImageFilterTest(int, char* [] ) {


  myRGBImageType::SizeType size;
  size[0] = 2;
  size[1] = 2;

  myRGBImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  myRGBImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  myRGBImageType::Pointer myImage = myRGBImageType::New();


  myImage->SetRegions( region );
  myImage->Allocate();

  myRGBIteratorType  it1( myImage, myImage->GetRequestedRegion() );

  // Value to initialize the pixels
  myRGBImageType::PixelType color;

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

  // Reading the values to verify the image content
  std::cout << "--- Initial image --- " << std::endl;
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    const myRGBImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
    }


  bool passed = true;

  // Convert to a red image
  itk::AdaptImageFilter<myRGBImageType, myImageType, myRedAccessorType>::Pointer  adaptImageToRed = itk::AdaptImageFilter<myRGBImageType, myImageType, myRedAccessorType>::New();
  FilterWatcher redWatcher(adaptImageToRed, "Red");
  adaptImageToRed->SetInput(myImage);
  adaptImageToRed->UpdateLargestPossibleRegion();
  adaptImageToRed->SetFunctor(adaptImageToRed->GetFunctor());

  myIteratorType  it( adaptImageToRed->GetOutput(), adaptImageToRed->GetOutput()->GetRequestedRegion() );

  std::cout << "--- Red values --- " << std::endl;

  it.GoToBegin();
  it1.GoToBegin();
  while( !it.IsAtEnd() )
  {
  std::cout << it.Get()   << std::endl;
  if (itk::Math::NotExactlyEquals(it.Get(), it1.Get().GetRed()))
    {
    passed = false;
    }

  ++it;
  ++it1;
  }

  // Convert to a green image
  itk::AdaptImageFilter<myRGBImageType, myImageType, myGreenAccessorType>::Pointer  adaptImageToGreen = itk::AdaptImageFilter<myRGBImageType, myImageType, myGreenAccessorType>::New();
  FilterWatcher greenWatcher(adaptImageToGreen, "Green");

  adaptImageToGreen->SetInput(myImage);
  adaptImageToGreen->UpdateLargestPossibleRegion();

  it = myIteratorType( adaptImageToGreen->GetOutput(), adaptImageToGreen->GetOutput()->GetRequestedRegion() );

  std::cout << "--- Green values --- " << std::endl;

  it.GoToBegin();
  it1.GoToBegin();
  while( !it.IsAtEnd() )
  {
  std::cout << it.Get()   << std::endl;
  if (itk::Math::NotExactlyEquals(it.Get(), it1.Get().GetGreen()))
    {
    passed = false;
    }

  ++it;
  ++it1;
  }

  // Convert to a blue image
  itk::AdaptImageFilter<myRGBImageType, myImageType, myBlueAccessorType>::Pointer  adaptImageToBlue = itk::AdaptImageFilter<myRGBImageType, myImageType, myBlueAccessorType>::New();
  FilterWatcher blueWatcher(adaptImageToBlue, "Blue");

  adaptImageToBlue->SetInput(myImage);
  adaptImageToBlue->UpdateLargestPossibleRegion();

  it = myIteratorType( adaptImageToBlue->GetOutput(), adaptImageToBlue->GetOutput()->GetRequestedRegion() );

  std::cout << "--- Blue values --- " << std::endl;

  it.GoToBegin();
  it1.GoToBegin();
  while( !it.IsAtEnd() )
  {
  std::cout << it.Get()   << std::endl;
  if (itk::Math::NotExactlyEquals(it.Get(), it1.Get().GetBlue()))
    {
    passed = false;
    }

  ++it;
  ++it1;
  }

  std::cout << std::endl;
  if (passed)
    {
    std::cout << "AdaptImageFilterTest passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cerr << "AdaptImageFilterTest failed." << std::endl;
    return EXIT_FAILURE;
    }
}
