/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWindowedSincInterpolateImageFunctionTest.cxx
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
#include "itkWindowedSincInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace SincInterpolate {

enum{ ImageDimension = 3 };
enum{ WindowRadius = 2 };

typedef unsigned char        PixelType;
typedef itk::Image<PixelType,ImageDimension> ImageType;
typedef double CoordRepType;

typedef itk::Function::HammingWindowFunction<2>  WindowFunctionType;

typedef itk::ConstantBoundaryCondition< ImageType >  BoundaryConditionType;

typedef itk::WindowedSincInterpolateImageFunction< 
                                          ImageType,2,
                                          WindowFunctionType,
                                          BoundaryConditionType,
                                          CoordRepType           > InterpolatorType;

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

  std::cout << " Point: " << point;

  bool bvalue = interp->IsInsideBuffer( point );
  std::cout << " Inside: " << bvalue << std::endl;

  if( bvalue != isInside )
    {
    std::cout << "*** Error: inside is " << bvalue << " but should be " << isInside << std::endl;
    return false;
    }

  if( isInside )
    {
    OutputType value = interp->Evaluate( point );
    std::cout << " Value: " << value << std::endl;

    if( vnl_math_abs( value - trueValue ) > 1e-9 )
      {
      std::cout << " *** Error: Value is " << value << " but should be: ";
      std::cout << trueValue << std::endl;
      }
    }

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

  std::cout << " Index: " << index;

  bool bvalue = interp->IsInsideBuffer( index );
  std::cout << " Inside: " << bvalue << std::endl;

  if( bvalue != isInside )
    {
    std::cout << "*** Error: inside is " << bvalue << " but should be " << isInside << std::endl;
    return false;
    }

  if( isInside )
    {
    OutputType value = interp->EvaluateAtContinuousIndex( index );
    std::cout << " Value: " << value << std::endl;

    if( vnl_math_abs( value - trueValue ) > 1e-9 )
      {
      std::cout << " *** Error: Value is " << value << " but should be: " << trueValue << std::endl;
      }
    }

  return true;

}

} // SincInterpolate namespace

int itkWindowedSincInterpolateImageFunctionTest(int, char* [] )
{
  int flag = 0;

  std::cout << "Testing vector image interpolation: " << std::endl;

  typedef SincInterpolate::ImageType            ImageType;
  typedef SincInterpolate::IndexType            IndexType;
  typedef SincInterpolate::PixelType            PixelType;
  typedef SincInterpolate::PointType            PointType;
  typedef SincInterpolate::OutputType           OutputType;
  typedef SincInterpolate::ContinuousIndexType  ContinuousIndexType;
  typedef SincInterpolate::CoordRepType         CoordRepType;

  typedef SincInterpolate::InterpolatorType    InterpolatorType;

  const unsigned int ImageDimension = SincInterpolate::ImageDimension;

  ImageType::SizeType size = { { 20, 40, 80 } };
  double origin [3] = { 0.5,   0.5,   0.5};
  double spacing[3] = { 0.1,   0.05 , 0.025};


  // Create a test image
  ImageType::Pointer image = ImageType::New();
  ImageType::RegionType region;
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();
 
  image->SetOrigin( origin );
  image->SetSpacing( spacing );

  // Write in a simple linear pattern
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image, region );

  IndexType index;
  unsigned short value;
  PixelType pixel;

  for( ; !iter.IsAtEnd(); ++iter )
    {
    index = iter.GetIndex();
    value = 0;

    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      value += index[j];
      }

    pixel = value;

    iter.Set( pixel );
    
    }

  // Create the interpolator
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage( image );
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
  CoordRepType darray[3] = {10, 20, 40};
  output = OutputType( 70 );
  cindex = ContinuousIndexType(darray);
  passed = SincInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) flag = 1;
  
  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = SincInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;
  
  // position at the image border
  {
  CoordRepType darray[3] = {0, 20, 40};
  output = OutputType( 60 );
  cindex = ContinuousIndexType(darray);
  passed = SincInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = SincInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;

  // position near image border
  {
  double epsilon = 1.0e-10;
  CoordRepType darray[3] = {19 - epsilon, 20, 40};
  output = OutputType( 79 );
  cindex = ContinuousIndexType(darray);
  passed = SincInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = SincInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;

  // position outside the image
  {
  CoordRepType darray[3] = {20, 20, 40};
  output = OutputType( 1 );
  cindex = ContinuousIndexType(darray);
  passed = SincInterpolate::TestContinuousIndex( interp, cindex, false, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = SincInterpolate::TestGeometricPoint( interp, point, false, output );

  if( !passed ) flag = 1;

  // at non-integer position 
  {
  CoordRepType darray[3] = {5.25, 12.5, 42.0};
  output = OutputType( 59.75 );
  cindex = ContinuousIndexType(darray);
  passed = SincInterpolate::TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) 
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = SincInterpolate::TestGeometricPoint( interp, point, true, output );

  if( !passed ) 
    {
    flag = 1;
    }

  /* Return results of test */
  if (flag != 0) 
    {
    std::cout << "*** Some test failed" << std::endl;
    return flag; 
    }
  
 
  std::cout << "All tests successfully passed" << std::endl;
  return EXIT_SUCCESS;


}

