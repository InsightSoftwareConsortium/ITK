/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMutualInformationMetricTest.cxx
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
#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkImageMapper.h"
#include "itkAffineRegistrationTransform.h"
#include "itkMutualInformationImageToImageMetric.h"

#include <iostream>

/**
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the mutual information value and derivatives
 *  for various shift values in (-10,10).
 *
 */

int main()
{

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  //Allocate Images
  typedef itk::Image<unsigned char,2>           ReferenceType;
  typedef itk::Image<unsigned char,2>           TargetType;
  enum { ImageDimension = ReferenceType::ImageDimension };

  ReferenceType::SizeType size = {{100,100}};
  ReferenceType::IndexType index = {{0,0}};
  ReferenceType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  ReferenceType::Pointer imgReference = ReferenceType::New();
  imgReference->SetLargestPossibleRegion( region );
  imgReference->SetBufferedRegion( region );
  imgReference->SetRequestedRegion( region );
  imgReference->Allocate();

  TargetType::Pointer imgTarget = TargetType::New();
  imgTarget->SetLargestPossibleRegion( region );
  imgTarget->SetBufferedRegion( region );
  imgTarget->SetRequestedRegion( region );
  imgTarget->Allocate();

  // Fill images with a 2D gaussian
  typedef  itk::ImageRegionIterator<ReferenceType>
    ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<TargetType>
    TargetIteratorType;

  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,2>  p;
  itk::Vector<double,2> d;

  // Set the displacement
  itk::Vector<double,2> displacement;
  displacement[0] = 5;
  displacement[1] =	0;

  ReferenceIteratorType ri(imgReference,region);
  TargetIteratorType ti(imgTarget,region);
  ri.Begin();
  while(!ri.IsAtEnd())
  {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
	  d = p-center;
	  d += displacement;
	  const double x = d[0];
	  const double y = d[1];
    ri.Set( (unsigned char) ( 200.0 * exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ri;
  }


  ti.Begin();
  while(!ti.IsAtEnd())
  {
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
	d = p-center;
	const double x = d[0];
	const double y = d[1];
    ti.Set( (unsigned char) ( 200.0 * exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ti;
  }

//-----------------------------------------------------------
// Set up a transformer
//-----------------------------------------------------------
  enum{ ParametersDimension = ImageDimension * ( ImageDimension + 1 ) };
  typedef itk::Vector<double,ParametersDimension> ParametersType;
  typedef itk::AffineRegistrationTransform<
    double, ImageDimension, ParametersType > TransformType;

  TransformType::Pointer transformer = TransformType::New();

//------------------------------------------------------------
// Set up a mapper
//------------------------------------------------------------
  typedef itk::ImageMapper< ReferenceType, TransformType >
     MapperType;

  MapperType::Pointer mapper = MapperType::New();

  // connect the transformer to the mapper
  mapper->SetTransform( transformer );

  // connect the reference image to the mapper
  mapper->SetDomain( imgReference );

//------------------------------------------------------------
// Set up the metric
//------------------------------------------------------------
  typedef itk::MutualInformationImageToImageMetric<
    TargetType, MapperType > MetricType;

  MetricType::Pointer metric = MetricType::New();

  // connect the mapper to the metric
  metric->SetMapper( mapper );

  // connect the target image to the mapper
  metric->SetTarget( imgTarget );

  // set the standard deviations
  metric->SetTargetStandardDeviation( 5.0 );
  metric->SetReferenceStandardDeviation( 5.0 );

  // set the number of samples to use
  metric->SetNumberOfSpatialSamples( 100 );

//------------------------------------------------------------
// Set up a affine transform parameters
//------------------------------------------------------------
  ParametersType parameters;

  // set the parameters to the identity
  ParametersType::Iterator it = parameters.Begin();

     // initialize the linear/matrix part
  for( unsigned int row = 0; row < ImageDimension; row++ )
    {
    for( unsigned int col = 0; col < ImageDimension; col++ )
      {
      *it = 0;
      if( row == col )
        {
        *it = 1;
        }
      ++it;
      }
    }

     // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    *it = 0;
    ++it;
    }


//---------------------------------------------------------
// Print out mutual information values
// for parameters[4] = {-10,10}
//---------------------------------------------------------

  MetricType::MeasureType measure;
  MetricType::DerivativeType derivative;

  printf("%s\t%s\t%s\n", "param[4]", "MI", "dMI/dparam[4]" );

  for( double trans = -10; trans <= 5; trans += 0.5 )
    {
    parameters[4] = trans;
    metric->GetValueAndDerivative( parameters, measure, derivative );

    printf( "%f\t%f\t%f\n", trans, measure,
      derivative[4] );

    // exercise the other functions
    metric->GetValue( parameters );
    metric->GetDerivative( parameters );

    }

  return EXIT_SUCCESS;

}

