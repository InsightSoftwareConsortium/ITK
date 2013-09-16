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
#ifndef __itkBinaryBallStructuringElement_hxx
#define __itkBinaryBallStructuringElement_hxx
#include "itkBinaryBallStructuringElement.h"

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"

namespace itk
{
// Create the structuring element
template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
BinaryBallStructuringElement< TPixel, VDimension, TAllocator >
::CreateStructuringElement()
{
  unsigned int i;

  // Image typedef
  typedef Image< TPixel, VDimension > ImageType;

  // Create an image to hold the ellipsoid
  //
  typename ImageType::Pointer sourceImage = ImageType::New();
  typename ImageType::RegionType region;
  region.SetSize( this->GetSize() );

  sourceImage->SetLargestPossibleRegion(region);
  sourceImage->SetBufferedRegion(region);
  sourceImage->SetRequestedRegion(region);
  sourceImage->Allocate();

  // Set the background to be zero
  //
  ImageRegionIterator< ImageType > it =
    ImageRegionIterator< ImageType >(sourceImage, region);

  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    it.Set(NumericTraits< TPixel >::Zero);
    }

  // Create the ellipsoid
  //

  // Ellipsoid spatial function typedef
  typedef EllipsoidInteriorExteriorSpatialFunction< VDimension > EllipsoidType;

  // Create an ellipsoid spatial function for the source image
  typename EllipsoidType::Pointer spatialFunction = EllipsoidType::New();

  // Define and set the axes lengths for the ellipsoid
  typename EllipsoidType::InputType axes;
  for ( i = 0; i < VDimension; i++ )
    {
    axes[i] = this->GetSize(i);
    }
  spatialFunction->SetAxes(axes);

  // Define and set the center of the ellipsoid in physical space
  typename EllipsoidType::InputType center;
  for ( i = 0; i < VDimension; i++ )
    {
    // put the center of ellipse in the middle of the center pixel
    center[i] = this->GetRadius(i) + 0.5;
    }
  spatialFunction->SetCenter(center);

  // Define the orientations of the ellipsoid axes, for now, we'll use
  // the identify matrix
  typename EllipsoidType::OrientationType orientations;
  orientations.fill(0.0);
  orientations.fill_diagonal(1.0);
  spatialFunction->SetOrientations(orientations);

  typename ImageType::IndexType seed;
  for ( i = 0; i < VDimension; i++ )
    {
    seed[i] = this->GetRadius(i);
    }
  FloodFilledSpatialFunctionConditionalIterator< ImageType, EllipsoidType >
  sfi = FloodFilledSpatialFunctionConditionalIterator< ImageType,
                                                       EllipsoidType >(sourceImage, spatialFunction, seed);
  sfi.SetCenterInclusionStrategy();

  // Iterate through the entire image and set interior pixels to 1
  for (; !sfi.IsAtEnd(); ++sfi )
    {
    sfi.Set(NumericTraits< TPixel >::One);
    }

  // Copy the ellipsoid into the kernel
  //
  Iterator kernel_it;
  for ( it.GoToBegin(), kernel_it = this->Begin(); !it.IsAtEnd(); ++it, ++kernel_it )
    {
    *kernel_it = it.Get();
    }

  // Clean up
  //   ...temporary image should be cleaned up by SmartPointers automatically
}
} // namespace itk

#endif
