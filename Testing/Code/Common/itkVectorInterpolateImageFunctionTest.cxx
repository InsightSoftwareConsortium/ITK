/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorInterpolateImageFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkVector.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

enum{ VectorDimension = 3 };
enum{ ImageDimension = 3 };
typedef itk::Vector<unsigned short,VectorDimension> PixelType;
typedef itk::Image<PixelType,ImageDimension> ImageType;
typedef itk::VectorLinearInterpolateImageFunction<ImageType> InterpolatorType;
typedef InterpolatorType::IndexType  IndexType;
typedef InterpolatorType::PointType  PointType;
typedef InterpolatorType::ContinuousIndexType ContinuousIndexType;
typedef InterpolatorType::OutputType OutputType;

ImageType::SizeType size = { { 20, 40, 80 } };
double origin [3] = { 0.5,   0.5,   0.5};
double spacing[3] = { 0.1,   0.05 , 0.025};


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
    std::cout << " Value: ";
    for( k = 0; k < VectorDimension - 1; k++ )
      {
       std::cout << value[k] << ", ";
      } 
    std::cout << value[k] << std::endl;

    for( k = 0; k < VectorDimension; k++ )
      {
      if( vnl_math_abs( value[k] - trueValue[k] ) > 1e-9 )
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
    std::cout << " Value: ";
    for( k = 0; k < VectorDimension - 1; k++ )
      {
       std::cout << value[k] << ", ";
      } 
    std::cout << value[k] << std::endl;

    for( k = 0; k < VectorDimension; k++ )
      {
      if( vnl_math_abs( value[k] - trueValue[k] ) > 1e-9 )
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
      }

		}

  std::cout << std::endl;
  return true;

}

int main()
{
  int flag = 0;

  std::cout << "Testing vector image interpolation: " << std::endl;

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

  for( ; !iter.IsAtEnd(); ++iter )
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

  /* Test evaluation at continuous indices and corresponding
     gemetric points */
  std::cout << "Evaluate at: " << std::endl;
  OutputType output;
  ContinuousIndexType cindex;
  PointType point;
  bool passed;

  // an integer position inside the image
  double darray1[3] = { 10, 20, 40};
  output = 70, 140, 210;
  cindex = ContinuousIndexType(darray1);
  passed = TestContinuousIndex( interp, cindex, true, output );

  if( !passed ) flag = 1;
  
  interp->ConvertContinuousIndexToPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;
  
  // position at the image border
  double darray2[3] = {0, 20, 40};
  output = 60, 120, 180;
  cindex = ContinuousIndexType(darray2);
  passed = TestContinuousIndex( interp, cindex, true, output );

  if( !passed ) flag = 1;

  interp->ConvertContinuousIndexToPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;

  // position near image border
  double darray3[3] = {19, 20, 40};
  output = 79, 158, 237;
  cindex = ContinuousIndexType(darray3);
  passed = TestContinuousIndex( interp, cindex, true, output );

  if( !passed ) flag = 1;

  interp->ConvertContinuousIndexToPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;

  // position outside the image
  double darray4[3] = {20, 20, 40};
  output = 1, 1, 1;
  cindex = ContinuousIndexType(darray4);
  passed = TestContinuousIndex( interp, cindex, false, output );

  if( !passed ) flag = 1;

  interp->ConvertContinuousIndexToPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, false, output );

  if( !passed ) flag = 1;

  // at non-integer position 
  double darray5[3] = {5.25, 12.5, 42.0};
  output = 59.75, 119.5, 179.25;
  cindex = ContinuousIndexType(darray5);
  passed = TestContinuousIndex( interp, cindex, true, output );

  if( !passed ) flag = 1;

  interp->ConvertContinuousIndexToPoint( cindex, point );
  passed = TestGeometricPoint( interp, point, true, output );

  if( !passed ) flag = 1;


  /* Return results of test */
  if (flag != 0) {
      std::cout << "*** Some test failed" << std::endl;
      return flag; }
  else {
      std::cout << "All tests successfully passed" << std::endl;
      return 0; }

}

