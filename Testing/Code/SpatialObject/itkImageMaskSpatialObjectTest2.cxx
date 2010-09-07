/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkImageMaskSpatialObjectTest2.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


/*
* This is a test file for the itkImageMaskSpatialObject class. 
* The suported pixel types does not include itkRGBPixel, itkRGBAPixel, etc...
* So far it only allows to manage images of simple types like unsigned short,
* unsigned int, or itk::Vector<...>.
*/

/*
 * This test addresses bug
 * http://public.kitware.com/Bug/view.php?id=0006340
 *
 */

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkAffineTransform.h"
#include "itkPoint.h"

#include "itkImageMaskSpatialObject.h"


int itkImageMaskSpatialObjectTest2(int, char* [])
{
  const unsigned int NDimensions = 3;
  int retval=EXIT_SUCCESS;

  typedef double ScalarType;
  typedef itk::ImageMaskSpatialObject<NDimensions> ImageMaskSpatialObject;
  typedef ImageMaskSpatialObject::PixelType  PixelType;
  typedef itk::Image<PixelType,NDimensions>  ImageType;
  typedef itk::Image<PixelType,NDimensions>  ImageType2;
  typedef ImageMaskSpatialObject::BoundingBoxType BoundingBox;
  typedef itk::ImageRegionIterator<ImageType> Iterator;
  typedef itk::ImageRegionIterator<ImageType2> Iterator2;
  typedef itk::Point<ScalarType,NDimensions> Point;

  // Direction was not taken into account in the image spatial object
  // explicitly test using images with directions set.
  // Also explicitly uses nonzero origin, non identity scales
  // just to see

  {
  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{ 50, 50, 50 }};
  ImageType::PointType origin;
  origin[0]=1.5;
  origin[1]=2.1;
  origin[2]=-3;
  image->SetOrigin(origin);

  ImageType::SpacingType spacing;
  spacing[0]=0.5;
  spacing[1]=0.7;
  spacing[2]=1.1;
  image->SetSpacing( spacing );
  ImageType::IndexType index = {{ 0, 0, 0 }};

  // Does it make sense to set the direction for a non-oriented image
  // well it happens frequently enough, so better test it
  ImageType::DirectionType direction;
  direction.Fill(0.0);
  direction[0][1]=-1;
  direction[1][0]=1;
  direction[2][2]=1;
  image->SetDirection(direction);

  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions( region );
  image->Allocate();

  PixelType p = itk::NumericTraits< PixelType >::Zero;

  image->FillBuffer( p );

  ImageType::RegionType insideRegion;
  ImageType::SizeType insideSize   = {{ 30, 30, 30 }};
  ImageType::IndexType insideIndex = {{ 10, 10, 10 }};
  insideRegion.SetSize( insideSize );
  insideRegion.SetIndex( insideIndex );


  Iterator it( image, insideRegion );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( itk::NumericTraits< PixelType >::max() );
    ++it;
    }

  ImageMaskSpatialObject::Pointer maskSO = ImageMaskSpatialObject::New();

  maskSO->SetImage(image);

  maskSO->ComputeObjectToWorldTransform();

  Iterator itr( image, region );
  itr.GoToBegin();

  while( !itr.IsAtEnd() )
    {
    const ImageType::IndexType constIndex =  itr.GetIndex();
    const bool reference = insideRegion.IsInside( constIndex );

    ImageType::PointType point;
    image->TransformIndexToPhysicalPoint( constIndex, point );
    const bool test      = maskSO->IsInside( point );
    if( test != reference )
      {
      std::cerr << "Error in the evaluation of IsInside() " << std::endl;
      std::cerr << "Index failed = " << constIndex << std::endl;
      std::cerr << "Point failed = " << point << std::endl;
      std::cerr << "Image is a: "<<image->GetNameOfClass()<<std::endl;
      std::cerr << "Direction is: "<<std::endl<<image->GetDirection()<<std::endl;
      retval=EXIT_FAILURE;
      break;
      }

    ++itr;
    }

  if(retval==EXIT_SUCCESS) 
    {
    std::cout<<"Test with "<<image->GetNameOfClass()<<" passed."<<std::endl;
    }
  }

  {
  ImageType2::Pointer image = ImageType2::New();
  ImageType2::SizeType size = {{ 50, 50, 50 }};
  ImageType::PointType origin;
  origin[0]=1.5;
  origin[1]=2.1;
  origin[2]=-3;
  image->SetOrigin(origin);

  ImageType::SpacingType spacing;
  spacing[0]=0.5;
  spacing[1]=0.7;
  spacing[2]=1.1;
  image->SetSpacing( spacing );

  ImageType2::IndexType index = {{ 0, 0, 0 }};
  ImageType2::DirectionType direction;
  direction.Fill(0.0);
  direction[0][1]=-1;
  direction[1][0]=1;
  direction[2][2]=1;
  image->SetDirection(direction);

  ImageType::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  image->SetRegions( region );
  image->Allocate();

  PixelType p = itk::NumericTraits< PixelType >::Zero;

  image->FillBuffer( p );

  ImageType::RegionType insideRegion;
  ImageType::SizeType insideSize   = {{ 30, 30, 30 }};
  ImageType::IndexType insideIndex = {{ 10, 10, 10 }};
  insideRegion.SetSize( insideSize );
  insideRegion.SetIndex( insideIndex );


  Iterator2 it( image, insideRegion );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( itk::NumericTraits< PixelType >::max() );
    ++it;
    }

  ImageMaskSpatialObject::Pointer maskSO = ImageMaskSpatialObject::New();

  maskSO->SetImage(image);

  maskSO->ComputeObjectToWorldTransform();

  Iterator2 itr( image, region );
  itr.GoToBegin();

  while( !itr.IsAtEnd() )
    {
    const ImageType::IndexType constIndex =  itr.GetIndex();
    const bool reference = insideRegion.IsInside( constIndex );
    ImageType::PointType point;
    image->TransformIndexToPhysicalPoint( constIndex, point );
    const bool test      = maskSO->IsInside( point );
    if( test != reference )
      {
      std::cerr << "Error in the evaluation of IsInside() " << std::endl;
      std::cerr << "Index failed = " << constIndex << std::endl;
      std::cerr << "Point failed = " << point << std::endl;
      std::cerr << "Image is a: "<<image->GetNameOfClass()<<std::endl;
      std::cerr << "Direction is: "<<std::endl<<image->GetDirection()<<std::endl;
      retval= EXIT_FAILURE;
      break;
      }
      ++itr;
    }

  if(retval==EXIT_SUCCESS)
    {
    std::cout<<"Test with "<<image->GetNameOfClass()<<" passed."<<std::endl;
    }
  }

  return retval;
}

