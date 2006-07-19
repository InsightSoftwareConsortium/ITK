/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageAdaptorInterpolateImageFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkImage.h"
#include "itkImageAdaptor.h"
#include "itkRGBPixel.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace ImageAdaptorInterpolate {
  
class RedChannelPixelAccessor  
{
public:
  typedef itk::RGBPixel<float>   InternalType;
  typedef               float    ExternalType;

  static ExternalType Get( const InternalType & input ) 
    {
      return static_cast<ExternalType>( input.GetRed() );
    }
};

typedef RedChannelPixelAccessor::InternalType  InputPixelType;
typedef RedChannelPixelAccessor::ExternalType  PixelType;

enum{ ImageDimension = 3 };

typedef itk::Image<InputPixelType,ImageDimension> ImageType;

typedef itk::ImageAdaptor<  ImageType, 
                            RedChannelPixelAccessor > ImageAdaptorType;

typedef double CoordRepType;

typedef itk::LinearInterpolateImageFunction< 
                                  ImageAdaptorType,
                                  CoordRepType  > InterpolatorType;

typedef InterpolatorType::IndexType  IndexType;
typedef InterpolatorType::PointType  PointType;
typedef InterpolatorType::ContinuousIndexType ContinuousIndexType;
typedef InterpolatorType::OutputType OutputType;


/**
 * Test a geometric point. Returns true if test has passed,
 * returns false otherwise
 */
bool TestGeometricPoint(
const InterpolatorType * interp,
const PointType& point,
bool isInside,
OutputType trueValue )
{

  int k;

  std::cout << " Point: " << point;

  bool bvalue = interp->IsInsideBuffer( point );
  std::cout << " Inside: " << bvalue;

  if( bvalue != isInside )
    {
    std::cout << "*** Error: inside should be " << isInside << std::endl;
    return false;
    }

  if( isInside )
    {
    OutputType value = interp->Evaluate( point );
    std::cout << " Value: " << value << std::endl;

    if( vnl_math_abs( value - trueValue ) > 1e-9 )
      {
      std::cout << " *** Error: Value should be: ";
      std::cout << trueValue << std::endl;
      return false;
      }
    }

  std::cout << std::endl;
  return true;

}


/**
 * Test a continuous index. Returns true if test has passed,
 * returns false otherwise
 */
bool TestContinuousIndex(
const InterpolatorType * interp,
const ContinuousIndexType& index,
bool isInside,
OutputType trueValue )
{

  int k;

  std::cout << " Index: " << index;

  bool bvalue = interp->IsInsideBuffer( index );
  std::cout << " Inside: " << bvalue;

  if( bvalue != isInside )
    {
    std::cout << "*** Error: inside should be " << isInside << std::endl;
    return false;
    }

  if( isInside )
    {
    OutputType value = interp->EvaluateAtContinuousIndex( index );
    std::cout << " Value: " << value << std::endl;

    if( vnl_math_abs( value - trueValue ) > 1e-9 )
      {
      std::cout << " *** Error: Value should be: ";
      std::cout << trueValue << std::endl;
      return false;
      }
    }

  std::cout << std::endl;
  return true;

}

} // ImageAdaptorInterpolate namespace

int itkImageAdaptorInterpolateImageFunctionTest(int, char* [] )
{
  int flag = 0;

  std::cout << "Testing image adaptor interpolation: " << std::endl;

  typedef ImageAdaptorInterpolate::ImageType           ImageType;
  typedef ImageAdaptorInterpolate::IndexType           IndexType;
  typedef ImageAdaptorInterpolate::InputPixelType      InputPixelType;
  typedef ImageAdaptorInterpolate::PixelType           PixelType;
  typedef ImageAdaptorInterpolate::PointType           PointType;
  typedef ImageAdaptorInterpolate::OutputType          OutputType;
  typedef ImageAdaptorInterpolate::ContinuousIndexType ContinuousIndexType;

  typedef ImageAdaptorInterpolate::InterpolatorType    InterpolatorType;

  const unsigned int ImageDimension = ImageAdaptorInterpolate::ImageDimension;

  ImageType::SizeType size = { { 20, 40, 80 } };
  double origin [3] = { 0.5,   0.5,   0.5};
  double spacing[3] = { 0.1,   0.05 , 0.025};


  // Create a test image
  ImageType::Pointer image = ImageType::New();
  ImageType::RegionType region;
  region.SetSize( size );

  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->Allocate();
 
  image->SetOrigin( origin );
  image->SetSpacing( spacing );

  // Write in a simple linear pattern
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image, region );

  IndexType index;
  unsigned short value;
  InputPixelType pixel;

  for( ; !iter.IsAtEnd(); ++iter )
    {
    index = iter.GetIndex();
    value = 0;

    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      value += index[j];
      }

    for( unsigned int k = 0; k < ImageDimension; k++ )
      {
      pixel[k] = ( k + 1 ) * value;
      }

    iter.Set( pixel );
    
    }

  // Create the image adaptor
  typedef ImageAdaptorInterpolate::ImageAdaptorType  ImageAdaptorType;
  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
  
  adaptor->SetImage( image );

  // Create the interpolator
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage( adaptor );
  interp->Print( std::cout );

  /* Test evaluation at continuous indices and corresponding
     gemetric points */
  std::cout << "Evaluate at: " << std::endl;
  OutputType output;
  ContinuousIndexType cindex;
  PointType point;
  bool passed;

  // an integer position inside the image
  {
  double darray[3] = {10, 20, 40};
  double temp = 70.0;
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = ImageAdaptorInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) 
    {
    flag = 1;
    }
  
  adaptor->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = ImageAdaptorInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed ) 
    {
    flag = 1;
    }
  
  // position at the image border
  {
  double darray[3] = {0, 20, 40};
  double temp = 60.0;
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = ImageAdaptorInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) 
    {
    flag = 1;
    }

  adaptor->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = ImageAdaptorInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed )
    {
    flag = 1;
    }

  // position near image border
  {
  double epsilon = 1.0e-10;
  double darray[3] = {19 - epsilon, 20, 40};
  double temp = 79.0;
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = ImageAdaptorInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) 
    {
    flag = 1;
    }

  adaptor->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = ImageAdaptorInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed )
    {
    flag = 1;
    }

  // position outside the image
  {
  double darray[3] = {20, 20, 40};
  double temp = 1.0;
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = ImageAdaptorInterpolate::TestContinuousIndex( interp, cindex, false, output );
  }

  if( !passed ) 
    {
    flag = 1;
    }

  adaptor->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = ImageAdaptorInterpolate::TestGeometricPoint( interp, point, false, output );

  if( !passed ) 
    {
    flag = 1;
    }

  // at non-integer position 
  {
  double darray[3] = {5.25, 12.5, 42.0};
  double temp = 59.75;
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = ImageAdaptorInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) 
    {
    flag = 1;
    }

  adaptor->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = ImageAdaptorInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed ) 
    {
    flag = 1;
    }


  /* Return results of test */
  if( flag != 0 ) 
    {
    std::cout << "*** Some test failed" << std::endl;
    return flag;
    }
  else 
    {
    std::cout << "All tests successfully passed" << std::endl;
    return 0;
    }

}

