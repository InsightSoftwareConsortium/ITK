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

#include <iostream>

#include "itkMath.h"
#include "itkImage.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNearestNeighborInterpolateImageFunction.h"

typedef itk::Size<3>                              SizeType;
typedef itk::Image<unsigned short, 3>             ImageType;
typedef double                                    CoordRepType;
typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>
                                                  InterpolatorType;
typedef InterpolatorType::IndexType               IndexType;
typedef InterpolatorType::PointType               PointType;
typedef InterpolatorType::ContinuousIndexType     ContinuousIndexType;

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
template <typename TInterpolator>
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

    if( itk::Math::abs( value - trueValue ) > 1e-9 )
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
template<typename TInterpolator>
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

    if( itk::Math::abs( value - trueValue ) > 1e-9 )
      {
      std::cout << "*** Error: value should be " << trueValue << std::endl;
      return false;
      }
    }

  std::cout << std::endl;
  return true;

}


int itkInterpolateTest(int, char *[] )
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
  for (int slice = 0; slice < 80; slice++)
    {
    index[2] = slice;
    for (int row = 0; row < 40; row++)
      {
      index[1] = row;
      for (int col = 0; col < 20; col++)
        {
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
  IndexType mindex;
  PointType point;
  bool passed;

  // an integer position inside the image
  itk::SpacePrecisionType darray1[3] = { 10, 20, 40};
  cindex = ContinuousIndexType(darray1);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 70 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 70 );

  if( !passed )
    {
    flag = 1;
    }

  mindex.CopyWithRound( cindex );
  double expectedValue = mindex[0] + mindex[1] + mindex[2];
  if ( itk::Math::NotAlmostEquals(interp->EvaluateAtIndex( mindex ), expectedValue) )
    {
    std::cout << "Index: " << index;
    std::cout << "Value: " << interp->EvaluateAtIndex(index) << std::endl;
    std::cout << "Error: true value should be 70" << std::endl;
    flag = 1;
    }

  // position at the image border
  itk::SpacePrecisionType darray2[3] = {0, 20, 40};
  cindex = ContinuousIndexType(darray2);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 60 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 60 );

  if( !passed )
    {
    flag = 1;
    }

  // position near image border
  itk::SpacePrecisionType epsilon = 1.0e-10;
  itk::SpacePrecisionType darray3[3] = {19 - epsilon, 20, 40};
  cindex = ContinuousIndexType(darray3);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 79 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 79 );

  if( !passed )
    {
    flag = 1;
    }

  // position outside the image
  itk::SpacePrecisionType darray4[3] = {20, 20, 40};
  cindex = ContinuousIndexType(darray4);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, false, 0 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, false, 0 );

  if( !passed )
    {
    flag = 1;
    }

  // at non-integer position, before half value
  itk::SpacePrecisionType darray5[3] = {5.25, 12.4, 42.0};
  cindex = ContinuousIndexType(darray5);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 59.65 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 59.65 );

  if( !passed )
    {
    flag = 1;
    }

  /* Create and initialize the nearest neigh. interpolator */
  NNInterpolatorType::Pointer nninterp = NNInterpolatorType::New();
  nninterp->SetInputImage(image);
  nninterp->Print( std::cout );

  mindex.CopyWithRound( cindex );
  expectedValue = mindex[0] + mindex[1] + mindex[2];

  passed = TestContinuousIndex<NNInterpolatorType>( nninterp, cindex, true, expectedValue );

  if( !passed )
    {
    flag = 1;
    }

  // at non-integer position, after half value
  itk::SpacePrecisionType darray6[3] = {5.25, 12.6, 42.0};
  cindex = ContinuousIndexType(darray6);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 59.85 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 59.85 );

  if( !passed )
    {
    flag = 1;
    }

  mindex.CopyWithRound( cindex );
  expectedValue = mindex[0] + mindex[1] + mindex[2];

  passed = TestContinuousIndex<NNInterpolatorType>( nninterp, cindex, true, expectedValue );

  if( !passed )
    {
    flag = 1;
    }

  // at non-integer position, at half value with an even base number
  itk::SpacePrecisionType darray7[3] = {5.25, 12.5, 42.0};
  cindex = ContinuousIndexType(darray7);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 59.75 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 59.75 );

  if( !passed )
    {
    flag = 1;
    }

  mindex.CopyWithRound( cindex );
  expectedValue = mindex[0] + mindex[1] + mindex[2];

  passed = TestContinuousIndex<NNInterpolatorType>( nninterp, cindex, true, expectedValue );

  if( !passed )
    {
    flag = 1;
    }

  // at non-integer position, at half value with an odd base number
  itk::SpacePrecisionType darray8[3] = {5.25, 11.5, 42.0};
  cindex = ContinuousIndexType(darray8);
  passed = TestContinuousIndex<InterpolatorType>( interp, cindex, true, 58.75 );

  if( !passed )
    {
    flag = 1;
    }

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint<InterpolatorType>( interp, point, true, 58.75 );

  if( !passed )
    {
    flag = 1;
    }


  mindex.CopyWithRound( cindex );
  expectedValue = mindex[0] + mindex[1] + mindex[2];

  passed = TestContinuousIndex<NNInterpolatorType>( nninterp, cindex, true, expectedValue );

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
  else
    {
    std::cout << "All tests successfully passed" << std::endl;
    }

  return EXIT_SUCCESS;
}
