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

// Native ITK stuff
#include "itkImageRegionIterator.h"

// Spatial function stuff
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

int itkFloodFillIteratorTest(int, char* [] )
{
  const unsigned int dim = 3;

  // Image typedef
  typedef itk::Image< int, dim > TImageType;

  //-----------------Create a new input image--------------------
  // Image size and spacing parameters
  TImageType::SizeValueType    sourceImageSize[]  = { 20,20,20 };
  TImageType::SpacingValueType sourceImageSpacing[] = { 1.0,1.0,1.0 };
  TImageType::PointValueType   sourceImageOrigin[] = { 0,0,0 };

  // Creates the sourceImage (but doesn't set the size or allocate memory)
  TImageType::Pointer sourceImage = TImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  std::cout << "New sourceImage created" << std::endl;

  //-----The following block allocates the sourceImage-----

  // Create a size object native to the sourceImage type
  TImageType::SizeType sourceImageSizeObject;
  // Set the size object to the array defined earlier
  sourceImageSizeObject.SetSize( sourceImageSize );
  // Create a region object native to the sourceImage type
  TImageType::RegionType largestPossibleRegion;
  // Resize the region
  largestPossibleRegion.SetSize( sourceImageSizeObject );
  // Set the largest legal region size (i.e. the size of the whole sourceImage) to what we just defined
  sourceImage->SetLargestPossibleRegion( largestPossibleRegion );
  // Set the buffered region
  sourceImage->SetBufferedRegion( largestPossibleRegion );
  // Set the requested region
  sourceImage->SetRequestedRegion( largestPossibleRegion );
  // Now allocate memory for the sourceImage
  sourceImage->Allocate();

  std::cout << "New sourceImage allocated" << std::endl;

  // Initialize the image to hold all 0's
  itk::ImageRegionIterator<TImageType> it =
    itk::ImageRegionIterator<TImageType>(sourceImage, largestPossibleRegion);

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    it.Set(0);
    }

  //---------Create and initialize a spatial function-----------

  typedef itk::SphereSpatialFunction<dim> TFunctionType;
  typedef TFunctionType::InputType        TFunctionPositionType;

  // Create and initialize a new sphere function

  TFunctionType::Pointer spatialFunc = TFunctionType::New();
  spatialFunc->SetRadius( 5 );

  TFunctionPositionType center;
  center[0]=10;
  center[1]=10;
  center[2]=10;
  spatialFunc->SetCenter(center);

  std::cout << "Sphere spatial function created" << std::endl;

  //---------Create and initialize a spatial function iterator-----------
  TImageType::IndexType seedPos;
  const TImageType::IndexValueType pos[] = {10,10,10};
  seedPos.SetIndex(pos);

  typedef itk::FloodFilledSpatialFunctionConditionalIterator
    <TImageType, TFunctionType> TItType;
  TItType sfi = TItType(sourceImage, spatialFunc, seedPos);

  // Iterate through the entire image and set interior pixels to 255
  for(; !( sfi.IsAtEnd() ); ++sfi)
    {

    std::cout << sfi.GetIndex() << ": " << sfi.Get() << std::endl;
    sfi.Set(255);
    }

  std::cout << "Spatial function iterator created, sphere drawn" << std::endl;

  return EXIT_SUCCESS;
}
