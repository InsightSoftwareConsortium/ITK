/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>

#include "itkImage.h"
#include "itkSize.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"

#include "vnl/vnl_math.h"


typedef itk::Size<3>                               SizeType;
typedef itk::Image<unsigned short, 3>              ImageType;
typedef double                                     CoordRepType;
typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>  InterpolatorType;
typedef InterpolatorType::IndexType                 IndexType;
typedef InterpolatorType::PointType                 PointType;
typedef InterpolatorType::ContinuousIndexType       ContinuousIndexType;

typedef itk::NearestNeighborInterpolateImageFunction<ImageType,CoordRepType>
  NNInterpolatorType;
namespace
{
  
/* Define the image size and physical coordinates */
SizeType size = {{20, 40, 80}};
double origin [3] = { 0.5L,   0.5L,   0.5L};
double spacing[3] = { 0.1L,   0.05L , 0.025L};

}

/**
 * Test a geometric point. Returns true if test has passed,
 * returns false otherwise
 */
template <class TInterpolator>
bool TestGeometricPoint(
const TInterpolator * interp,
const PointType& point,
bool isInside,
double trueValue )
{

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
    double value = interp->Evaluate( point );
    std::cout << " Value: " << value;

    if( vnl_math_abs( value - trueValue ) > 1e-9 )
      {
      std::cout << "*** Error: value should be " << trueValue << std::endl;
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
template<class TInterpolator>
bool TestContinuousIndex(
const TInterpolator * interp,
const ContinuousIndexType& index,
bool isInside,
double trueValue )
{

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
    double value = interp->EvaluateAtContinuousIndex( index );
    std::cout << " Value: " << value;

    if( vnl_math_abs( value - trueValue ) > 1e-9 )
      {
      std::cout << "*** Error: value should be " << trueValue << std::endl;
      return false;
      }
    }

  std::cout << std::endl;
  return true;

}


int 
itkInterpolateTest(
    int itkNotUsed(argc),
    char * itkNotUsed(argv) [] )
{
    int flag = 0;           /* Did this test program work? */

    std::cout << "Testing image interpolation methods:\n";

    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();
    ImageType::RegionType region;
    region.SetSize(size);
    image->SetLargestPossibleRegion(region);
    image->SetBufferedRegion(region);
    image->Allocate();

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    /* Initialize the image contents */
    IndexType index;
    for (int slice = 0; slice < 80; slice++) {
        index[2] = slice;
        for (int row = 0; row < 40; row++) {
            index[1] = row;
            for (int col = 0; col < 20; col++) {
                index[0] = col;
                image->SetPixel(index, slice+row+col);
            }
        }
    }

    /* Create and initialize the interpolator */
    InterpolatorType::Pointer interp = InterpolatorType::New();
    interp->SetInputImage(image);
    interp->Print( std::cout );

    /* Test evaluation at continuous indices and corresponding
       gemetric points */
    std::cout << "Evaluate at: " << std::endl;
    ContinuousIndexType cindex;
    PointType point;
    bool passed;

    // an integer position inside the image
    double darray1[3] = { 10, 20, 40};
    cindex = ContinuousIndexType(darray1);
    passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 70 );

    if( !passed ) flag = 1;
    
    image->TransformContinuousIndexToPhysicalPoint( cindex, point );
    passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 70 );

    if( !passed ) flag = 1;
    
    // position at the image border
    double darray2[3] = {0, 20, 40};
    cindex = ContinuousIndexType(darray2);
    passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 60 );

    if( !passed ) flag = 1;

    image->TransformContinuousIndexToPhysicalPoint( cindex, point );
    passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 60 );

    if( !passed ) flag = 1;

    // position near image border
    double epsilon = 1.0e-10;
    double darray3[3] = {19 - epsilon, 20, 40};
    cindex = ContinuousIndexType(darray3);
    passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 79 );

    if( !passed ) flag = 1;

    image->TransformContinuousIndexToPhysicalPoint( cindex, point );
    passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 79 );

    if( !passed ) flag = 1;

    // position outside the image
    double darray4[3] = {20, 20, 40};
    cindex = ContinuousIndexType(darray4);
    passed = TestContinuousIndex<InterpolatorType>( interp, cindex, false, 0 );

    if( !passed ) flag = 1;

    image->TransformContinuousIndexToPhysicalPoint( cindex, point );
    passed = TestGeometricPoint<InterpolatorType>( interp, point, false, 0 );

    if( !passed ) flag = 1;

    // at non-integer position 
    double darray5[3] = {5.25, 12.5, 42.0};
    cindex = ContinuousIndexType(darray5);
    passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 59.75 );

    if( !passed ) flag = 1;

    image->TransformContinuousIndexToPhysicalPoint( cindex, point );
    passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 59.75 );

    if( !passed ) flag = 1;

    /* Create and initialize the nearest neigh. interpolator */
    NNInterpolatorType::Pointer nninterp = NNInterpolatorType::New();
    nninterp->SetInputImage(image);
    nninterp->Print( std::cout );

    passed = TestContinuousIndex<NNInterpolatorType>( nninterp, cindex, true,
      60 );

    if( !passed ) flag = 1;



    /* Return results of test */
    if (flag != 0) {
        std::cout << "*** Some test failed" << std::endl;
        return flag; }
    else {
        std::cout << "All tests successfully passed" << std::endl;
        return 0; }

}











