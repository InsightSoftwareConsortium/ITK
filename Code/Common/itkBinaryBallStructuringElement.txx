/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryBallStructuringElement.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryBallStructuringElement_txx
#define _itkBinaryBallStructuringElement_txx
#include "itkBinaryBallStructuringElement.h"

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h" 

namespace itk
{

// Create the structuring element
template <class TPixel, unsigned int VDimension, class TAllocator>
void
BinaryBallStructuringElement<TPixel, VDimension, TAllocator>
::CreateStructuringElement()
{
  unsigned int i;
  
  // Image typedef
  typedef itk::Image<TPixel, VDimension> ImageType;

  // Create an image to hold the ellipsoid
  //
  typename ImageType::Pointer sourceImage = ImageType::New();
  typename ImageType::RegionType region;
  region.SetSize( this->GetSize() );

  sourceImage->SetLargestPossibleRegion( region );
  sourceImage->SetBufferedRegion( region );
  sourceImage->SetRequestedRegion( region );
  sourceImage->Allocate();

  // Set the background to be zero
  //
  itk::ImageRegionIterator<ImageType> it =
     itk::ImageRegionIterator<ImageType>(sourceImage, region);

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    it.Set(NumericTraits<TPixel>::Zero);
    }

  
  // Create the ellipsoid
  //

  // Ellipsoid spatial function typedef
  typedef itk::EllipsoidInteriorExteriorSpatialFunction<VDimension>
    EllipsoidType;
  
  // Create an ellipsoid spatial function for the source image
  typename EllipsoidType::Pointer spatialFunction = EllipsoidType::New();

  // Define and set the axes lengths for the ellipsoid
  typename EllipsoidType::InputType axes;
  for (i=0; i < VDimension; i++)
    {
    axes[i] = this->GetSize(i);
    }
  spatialFunction->SetAxes( axes );

  // Define and set the center of the ellipsoid in physical space
  typename EllipsoidType::InputType center;
  for (i=0; i < VDimension; i++)
    {
    // put the center of ellipse in the middle of the center pixel
    center[i] = this->GetRadius(i) + 0.5; 
    }
  spatialFunction->SetCenter( center );

  // Define the orientations of the ellipsoid axes, for now, we'll use
  // the identify matrix
  vnl_matrix<double> orientations(3, 3, vnl_matrix_identity);  
  spatialFunction->SetOrientations(orientations);

  typename ImageType::IndexType seed;
  for (i=0; i < VDimension; i++)
    {
    seed[i] = this->GetRadius(i);
    }
  itk::FloodFilledSpatialFunctionConditionalIterator<ImageType, EllipsoidType> 
    sfi = itk::FloodFilledSpatialFunctionConditionalIterator<ImageType,
     EllipsoidType>(sourceImage, spatialFunction, seed);
  sfi.SetCenterInclusionStrategy();
  
  // Iterate through the entire image and set interior pixels to 1
  for(; !sfi.IsAtEnd(); ++sfi)
    {
     sfi.Set(NumericTraits<TPixel>::One);
    }

  
  // Copy the ellipsoid into the kernel
  //
  Iterator kernel_it;
  for (it.GoToBegin(), kernel_it=this->Begin(); !it.IsAtEnd();
       ++it, ++kernel_it)
    {
    *kernel_it = it.Get();
    }

  // Clean up
  //   ...temporary image should be cleaned up by SmartPointers automatically
  
}

} // namespace itk

#endif
