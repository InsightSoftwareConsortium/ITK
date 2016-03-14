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
#include "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

enum{ VectorDimension = 3 };
enum{ ImageDimension = 3 };
typedef itk::Vector<unsigned short,VectorDimension> PixelType;
typedef itk::Image<PixelType,ImageDimension>        ImageType;
typedef double                                      CoordRepType;

typedef itk::VectorLinearInterpolateNearestNeighborExtrapolateImageFunction<
  ImageType,CoordRepType> InterpolatorType;

typedef InterpolatorType::IndexType           IndexType;
typedef InterpolatorType::PointType           PointType;
typedef InterpolatorType::ContinuousIndexType ContinuousIndexType;
typedef InterpolatorType::OutputType          OutputType;


/**
 * Test a geometric point. Returns true if test has passed,
 * returns false otherwise
 */
bool TestGeometricPoint(
const InterpolatorType * interp,
const PointType& point,
bool itkNotUsed( isInside ),
OutputType trueValue )
{

  std::cout << " Point: " << point;

  bool bvalue = interp->IsInsideBuffer( point );
  std::cout << " Inside: " << bvalue;

  if( bvalue != true )
    {
    std::cout << "*** Error: inside should always be true for VectorLinearInterpolateNearestNeighborExtrapolateImageFunction" << std::endl;
    return false;
    }

  if( bvalue )
    {
    int k;
    OutputType value = interp->Evaluate( point );
    std::cout << " Value: ";
    for( k = 0; k < VectorDimension - 1; k++ )
      {
       std::cout << value[k] << ", ";
      }
    std::cout << value[k] << std::endl;

    for( k = 0; k < VectorDimension; k++ )
      {
      if( itk::Math::abs( value[k] - trueValue[k] ) > 1e-9 )
        {
        break;
        }
      }

    if( k != VectorDimension )
      {
      std::cout << " *** Error: Value should be: ";
      for( k = 0; k < VectorDimension - 1; k++ )
        {
         std::cout << trueValue[k] << ", ";
        }
      std::cout << trueValue[k] << std::endl;
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

  std::cout << " Index: " << index;

  bool bvalue = interp->IsInsideBuffer( index );
  std::cout << " Inside: " << bvalue;

  if( bvalue != true )
    {
    std::cout << "*** Error: inside should always be true for VectorLinearInterpolateNearestNeighborExtrapolateImageFunction" << std::endl;
    return false;
    }

  if( isInside )
    {
    int k;
    OutputType value = interp->EvaluateAtContinuousIndex( index );
    std::cout << " Value: ";
    for( k = 0; k < VectorDimension - 1; k++ )
      {
       std::cout << value[k] << ", ";
      }
    std::cout << value[k] << std::endl;

    for( k = 0; k < VectorDimension; k++ )
      {
      if( itk::Math::abs( value[k] - trueValue[k] ) > 1e-9 )
        {
        break;
        }
      }

    if( k != VectorDimension )
      {
      std::cout << " *** Error: Value should be: ";
      for( k = 0; k < VectorDimension - 1; k++ )
        {
         std::cout << trueValue[k] << ", ";
        }
      std::cout << trueValue[k] << std::endl;
      return false;
      }

    }

  std::cout << std::endl;
  return true;

}

int itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest(int, char* [] )
{
  int flag = 0;

  std::cout << "Testing vector image interpolation: " << std::endl;

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
  PixelType pixel;

  for(; !iter.IsAtEnd(); ++iter)
    {
    index = iter.GetIndex();
    value = 0;

    for( int j = 0; j < ImageDimension; j++ )
      {
      value += index[j];
      }

    for( int k = 0; k < ImageDimension; k++ )
      {
      pixel[k] = ( k + 1 ) * value;
      }

    iter.Set( pixel );

    }

  // Create the interpolator
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage( image );
  interp->Print( std::cout );

  typedef InterpolatorType::Superclass GenericInterpolatorType;
  std::cout << interp->GenericInterpolatorType::GetNameOfClass() << std::endl;
  std::cout << interp->GetNameOfClass() << std::endl;

  /* Test evaluation at continuous indices and corresponding
     geometric points */
  std::cout << "Evaluate at: " << std::endl;
  OutputType output;
  ContinuousIndexType cindex;
  PointType point;
  bool passed;

  // an integer position inside the image
  {
  itk::SpacePrecisionType darray[3] = {10, 20, 40};
  double temp[3] = {70, 140, 210};
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;

  index[0] = 10;
  index[1] = 20;
  index[2] = 40;
  if ( interp->EvaluateAtIndex( index ) != output )
    {
    std::cout << "Index: " << index;
    std::cout << "Value: " << interp->EvaluateAtIndex(index) << std::endl;
    std::cout << "Error: true value should be " << output << std::endl;
    flag = 1;
    }


  // position at the image border
  {
  itk::SpacePrecisionType darray[3] = {0, 20, 40};
  double temp[3] = {60, 120, 180};
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;

  // position near image border
  {
  const itk::SpacePrecisionType epsilon = 1.0e-10;
  const itk::SpacePrecisionType darray[3] = {19 - epsilon, 20, 40};
  const double temp[3] = {79, 158, 237};
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;

  // position outside the image
  {
  const itk::SpacePrecisionType darray[3] = {20, 20, 40};
  const double temp[3] = {79, 158, 237};
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = TestContinuousIndex( interp, cindex, false, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, false, output );

  if( !passed ) flag = 1;

  // at non-integer position
  {
  itk::SpacePrecisionType darray[3] = {5.25, 12.5, 42.0};
  double temp[3] = {59.75, 119.5, 179.25};
  output = OutputType( temp );
  cindex = ContinuousIndexType(darray);
  passed = TestContinuousIndex( interp, cindex, true, output );
  }

  if( !passed ) flag = 1;

  image->TransformContinuousIndexToPhysicalPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;


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
