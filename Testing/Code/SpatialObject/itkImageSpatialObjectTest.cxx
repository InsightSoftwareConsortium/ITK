/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSpatialObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
* This is a test file for the itkImageSpatialObject class. 
* The suported pixel types does not include itkRGBPixel, itkRGBAPixel, etc...
* So far it only allows to manage images of simple types like unsigned short,
* unsigned int, or itk::Vector<...>.
*/


#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkAffineTransform.h"
#include "itkPoint.h"

#include "itkImageSpatialObject.h"


int itkImageSpatialObjectTest(int, char* [])
{
  #define NDimensions 3

  typedef double ScalarType;
  typedef unsigned short Pixel;
  typedef itk::Image<Pixel,NDimensions> Image;
  typedef itk::ImageSpatialObject<NDimensions,Pixel> ImageSpatialObject;
  typedef ImageSpatialObject::BoundingBoxType BoundingBox;
  typedef itk::ImageRegionIterator<Image> Iterator;
  typedef itk::Point<ScalarType,NDimensions> Point;

  Image::Pointer image = Image::New();
  Image::SizeType size = {{ 10, 10, 10 }};
  Image::IndexType index = {{ 0, 0, 0 }};
  Image::RegionType region;
  
  region.SetSize(size);
  region.SetIndex(index);
  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->SetRequestedRegion(region);
  image->Allocate();
  
  Iterator it(image,region);
  Pixel p =0;

  for(; !it.IsAtEnd(); ++it, ++p)
    {
    it.Set(p);
    }
  it.GoToBegin();

  ImageSpatialObject::Pointer imageSO = ImageSpatialObject::New();
 
  imageSO->SetImage(image);
  ImageSpatialObject::TransformType::OffsetType offset;
  offset.Fill(10);

  imageSO->GetTransform()->SetOffset(offset);
  imageSO->ComputeGlobalTransform();

  Point q,r; 
  double returnedValue,expectedValue;
 
  r.Fill(9);
  q.Fill(15);

  std::cout<<"IsInside()...";
  if( imageSO->IsInside(r) || !imageSO->IsInside(q) )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  expectedValue = 555;
  
  try
    {
    imageSO->ValueAt(q,returnedValue);
    }
  catch( itk::ExceptionObject & e )
    {
    throw e;
    }

  std::cout<<"ValueAt()...";
  if( returnedValue != expectedValue )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }
  
  ImageSpatialObject::OutputVectorType derivative,expectedDerivative;
  Pixel expectedPixel;

  imageSO->DerivativeAt(q,1,derivative);
  expectedPixel = 1;
  expectedDerivative[0]=expectedPixel;
  expectedPixel = 10;
  expectedDerivative[1]=expectedPixel;
  expectedPixel = 100;
  expectedDerivative[2]=expectedPixel;
  std::cout<<"DerivativeAt()...";
  if( derivative != expectedDerivative )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  return EXIT_SUCCESS;
}
