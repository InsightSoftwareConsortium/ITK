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

/*
This file test the various inclusion strategies available
to itkFloodFilledSpatialFunctionConditionalIterator.
*/


#include "itkImageRegionIterator.h"
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

int itkFloodFilledSpatialFunctionTest(int, char* [])
{
  const unsigned int dim = 2;

  // Image typedef
  typedef itk::Image< bool, dim > ImageType;

  typedef ImageType::SizeValueType      SizeValueType;
  typedef ImageType::SpacingValueType   SpacingValueType;
  typedef ImageType::PointValueType     PointValueType;

  // Image size and spacing parameters
  SizeValueType     sourceImageSize[]  = { 5, 5};
  SpacingValueType  sourceImageSpacing[] = { 1.0,1.0 };
  PointValueType    sourceImageOrigin[] = { 0,0 };

  // Creates the sourceImage (but doesn't set the size or allocate memory)
  ImageType::Pointer sourceImage = ImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  // Create a size object native to the sourceImage type
  ImageType::SizeType sourceImageSizeObject;

  // Set the size object to the array defined earlier
  sourceImageSizeObject.SetSize( sourceImageSize );

  // Create a region object native to the sourceImage type
  ImageType::RegionType largestPossibleRegion;

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

  // Loop over all available iterator strategies
  for(int strat = 0; strat < 4; strat++)
    {
    // Initialize the image to hold all 0's
    itk::ImageRegionIterator<ImageType> it =
      itk::ImageRegionIterator<ImageType>(sourceImage, largestPossibleRegion);

    for(it.GoToBegin(); !it.IsAtEnd(); ++it)
      {
      it.Set(0);
      }

    // Create and initialize a spatial function
    typedef itk::SphereSpatialFunction<dim> FunctionType;
    typedef FunctionType::InputType         FunctionPositionType;

    FunctionType::Pointer spatialFunc = FunctionType::New();
    spatialFunc->SetRadius( 1.0 );

    FunctionPositionType center;
    center[0] = 2.5;
    center[1] = 2.5;
    spatialFunc->SetCenter(center);

    // Create and initialize a spatial function iterator
    ImageType::IndexType seedPos;
    const ImageType::IndexValueType pos[] = {2,2};
    seedPos.SetIndex(pos);

    typedef itk::FloodFilledSpatialFunctionConditionalIterator
      <ImageType, FunctionType> ItType;

    ItType sfi = ItType(sourceImage, spatialFunc, seedPos);

    switch(strat){
    case 0:
      {
        sfi.SetOriginInclusionStrategy();
      }
    break;
    case 1:
      {
        sfi.SetCenterInclusionStrategy();
      }
    break;
    case 2:
      {
        sfi.SetCompleteInclusionStrategy();
      }
    break;
    case 3:
      {
        sfi.SetIntersectInclusionStrategy();
      }
    } // end switch inclusion strategy

    // Iterate through the entire image and set interior pixels to 1
    for( sfi.GoToBegin(); !( sfi.IsAtEnd() ); ++sfi)
      {
      sfi.Set(1);
      }

    } // end loop over iterator strategies

  return EXIT_SUCCESS;
}
