/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlatStructuringElement.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFlatStructuringElement_txx
#define __itkFlatStructuringElement_txx

#include "itkFlatStructuringElement.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h" 


namespace itk
{


template<unsigned int NDimension>
FlatStructuringElement<NDimension> 
FlatStructuringElement<NDimension>
::Box( RadiusType radius )
{
  Self res = Self();

  res.SetRadius( radius );


  Iterator kernel_it;
  for( kernel_it=res.Begin(); kernel_it != res.End(); ++kernel_it )
    {
    *kernel_it= true;
    }

  return res;
}


template<unsigned int NDimension>
FlatStructuringElement<NDimension> 
FlatStructuringElement<NDimension>
::Ball(RadiusType radius)
{
  Self res = Self();

  res.SetRadius( radius );

  
  // Image typedef
  typedef Image<bool> ImageType;

  // Create an image to hold the ellipsoid
  //
  typename ImageType::Pointer sourceImage = ImageType::New();
  typename ImageType::RegionType region;
  RadiusType size = radius;
  for(unsigned int i=0; i<Dimension; i++ )
    {
    size[i] = 2*size[i] + 1;
    }
  region.SetSize( size );

  sourceImage->SetRegions( region );
  sourceImage->Allocate();

  // Set the background to be zero
  //
  ImageRegionIterator<ImageType> it(sourceImage, region);

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    it.Set(false);
    }

  
  // Create the ellipsoid
  //

  // Ellipsoid spatial function typedef
  typedef EllipsoidInteriorExteriorSpatialFunction<Dimension> EllipsoidType;
  
  // Create an ellipsoid spatial function for the source image
  typename EllipsoidType::Pointer spatialFunction = EllipsoidType::New();

  // Define and set the axes lengths for the ellipsoid
  typename EllipsoidType::InputType axes;
  for (unsigned int i=0; i < Dimension; i++)
    {
    axes[i] = res.GetSize(i);
    }
  spatialFunction->SetAxes( axes );

  // Define and set the center of the ellipsoid in physical space
  typename EllipsoidType::InputType center;
  for (unsigned int i=0; i < Dimension; i++)
    {
    // put the center of ellipse in the middle of the center pixel
    center[i] = res.GetRadius(i) + 0.5; 
    }
  spatialFunction->SetCenter( center );

  // Define the orientations of the ellipsoid axes, for now, we'll use
  // the identify matrix
  typename EllipsoidType::OrientationType orientations;
  orientations.fill( 0.0 );
  orientations.fill_diagonal( 1.0 );
  spatialFunction->SetOrientations(orientations);

  typename ImageType::IndexType seed;
  for (unsigned int i=0; i < Dimension; i++)
    {
    seed[i] = res.GetRadius(i);
    }
  FloodFilledSpatialFunctionConditionalIterator<ImageType, EllipsoidType> 
    sfi = FloodFilledSpatialFunctionConditionalIterator<ImageType,
    EllipsoidType>(sourceImage, spatialFunction, seed);
  sfi.SetCenterInclusionStrategy();
  
  // Iterate through the entire image and set interior pixels to 1
  for(; !sfi.IsAtEnd(); ++sfi)
    {
    sfi.Set(true);
    }

  
  // Copy the ellipsoid into the kernel
  //
  Iterator kernel_it;
  for (it.GoToBegin(), kernel_it=res.Begin();!it.IsAtEnd();++it,++kernel_it)
    {
    *kernel_it = it.Get();
    }

  // Clean up
  //   ...temporary image should be cleaned up by SmartPointers automatically

  return res;
}


}

#endif
