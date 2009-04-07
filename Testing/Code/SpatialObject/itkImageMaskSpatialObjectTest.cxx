/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMaskSpatialObjectTest.cxx
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


#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkAffineTransform.h"
#include "itkPoint.h"

#include "itkImageMaskSpatialObject.h"


int itkImageMaskSpatialObjectTest(int, char* [])
{
  const unsigned int NDimensions = 3;

  typedef double ScalarType;
  typedef itk::ImageMaskSpatialObject<NDimensions> ImageMaskSpatialObject;
  typedef ImageMaskSpatialObject::PixelType  PixelType;
  typedef ImageMaskSpatialObject::ImageType  ImageType;
  typedef ImageMaskSpatialObject::BoundingBoxType BoundingBox;
  typedef itk::ImageRegionIterator<ImageType> Iterator;
  typedef itk::Point<ScalarType,NDimensions> Point;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{ 50, 50, 50 }};
  ImageType::IndexType index = {{ 0, 0, 0 }};
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
  maskSO->Print(std::cout);
 
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
        return EXIT_FAILURE;
        }
    ++itr;
    }

  maskSO->Print(std::cout);

  return EXIT_SUCCESS;
}
