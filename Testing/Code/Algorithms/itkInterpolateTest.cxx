/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateTest.cxx
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
#include "itkSize.h"
#include "itkLinearInterpolateImageFunction.h"

#include "vnl/vnl_math.h"


typedef itk::Size<3>                               SizeType;
typedef itk::Image<unsigned short, 3>              ImageType;
typedef itk::LinearInterpolateImageFunction<ImageType>  InterpolatorType;
typedef InterpolatorType::IndexType                 IndexType;
typedef InterpolatorType::PointType                 PointType;
typedef InterpolatorType::ContinuousIndexType       ContinuousIndexType;

/* Define the image size and physical coordinates */
SizeType size = {{20, 40, 80}};
double origin [3] = { 0.5,   0.5,   0.5};
double spacing[3] = { 0.1,   0.05 , 0.025};

/**
 * This function convert points from Image space to
 * geometric space
 */ 
PointType ConvertContinuousIndexToPoint( 
const ContinuousIndexType& index )
{
	PointType point;
  for( int j = 0; j < PointType::PointDimension; j++ )
		{
		point[j] = index[j] * spacing[j] + origin[j];
		}

  return point;
}

/**
 * Test a geometric point. Returns true if test has passed,
 * returns false otherwise
 */
bool TestGeometricPoint(
const InterpolatorType * interp,
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
bool TestContinuousIndex(
const InterpolatorType * interp,
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
main(
    int argc,
    char *argv[])
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


    /* Test evaluation at continuous indices and corresponding
       gemetric points */
    std::cout << "Evaluate at: " << std::endl;
    ContinuousIndexType cindex;
    PointType point;
    bool passed;

    // an integer position inside the image
    double darray1[3] = { 10, 20, 40};
    cindex = ContinuousIndexType(darray1);
    passed = TestContinuousIndex( interp, cindex, true, 70 );

    if( !passed ) flag = 1;
    
    point = ConvertContinuousIndexToPoint( cindex );
    passed = TestGeometricPoint( interp, point, true, 70 );

    if( !passed ) flag = 1;
    
    // position at the image border
    double darray2[3] = {0, 20, 40};
    cindex = ContinuousIndexType(darray2);
    passed = TestContinuousIndex( interp, cindex, true, 60 );

    if( !passed ) flag = 1;

    point = ConvertContinuousIndexToPoint( cindex );
    passed = TestGeometricPoint( interp, point, true, 60 );

    if( !passed ) flag = 1;

    // position near image border
    double darray3[3] = {19, 20, 40};
    cindex = ContinuousIndexType(darray3);
    passed = TestContinuousIndex( interp, cindex, true, 79 );

    if( !passed ) flag = 1;

    point = ConvertContinuousIndexToPoint( cindex );
    passed = TestGeometricPoint( interp, point, true, 79 );

    if( !passed ) flag = 1;

    // position outside the image
    double darray4[3] = {20, 20, 40};
    cindex = ContinuousIndexType(darray4);
    passed = TestContinuousIndex( interp, cindex, false, 0 );

    if( !passed ) flag = 1;

    point = ConvertContinuousIndexToPoint( cindex );
    passed = TestGeometricPoint( interp, point, false, 0 );

    if( !passed ) flag = 1;

    // at non-integer position 
    double darray5[3] = {5.25, 12.5, 42.0};
    cindex = ContinuousIndexType(darray5);
    passed = TestContinuousIndex( interp, cindex, true, 59.75 );

    if( !passed ) flag = 1;

    point = ConvertContinuousIndexToPoint( cindex );
    passed = TestGeometricPoint( interp, point, true, 59.75 );

    if( !passed ) flag = 1;


    /* Return results of test */
    if (flag != 0) {
        std::cout << "*** Some test failed" << std::endl;
        return flag; }
    else {
        std::cout << "All tests successfully passed" << std::endl;
        return 0; }

}











