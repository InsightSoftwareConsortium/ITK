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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkBSplineResampleImageFunction.h"
#include "itkBSplineDecompositionImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkImage.h"
#include "itkSize.h"
#include "itkRandomImageSource.h"
#include "vnl/vnl_sample.h"

int itkBSplineResampleImageFunctionTest(int, char* [] )
{

  const unsigned int ImageDimension = 2;
  typedef float PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;

  const unsigned int SplineOrder = 3;

  /** Generate a random input image and connect to BSpline decomposition filter */
  typedef ImageType::SpacingType  SpacingType;
  typedef ImageType::PointType    PointType;
  typedef ImageType::SizeType     SizeType;

  SpacingType    spacing;
  PointType      origin;
  SizeType       size;

  spacing.Fill( 2.0 );
  origin.Fill ( 10.0 );
  size.Fill( 32 );

  typedef itk::RandomImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();

  source->SetSize( size );
  source->SetSpacing( spacing );
  source->SetOrigin( origin );

  source->SetMin( 0.0 );
  source->SetMax( 10.0 );

  typedef itk::BSplineDecompositionImageFilter<ImageType,ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter,"filter");

  filter->SetSplineOrder( SplineOrder );
  filter->SetInput( source->GetOutput() );
  filter->Update();

  /** Set up a BSplineResampleImageFunction. */
  typedef itk::BSplineResampleImageFunction<ImageType,double>
    ResampleFunctionType;
  ResampleFunctionType::Pointer resample = ResampleFunctionType::New();

  resample->SetSplineOrder( SplineOrder );
  resample->SetInputImage( filter->GetOutput() );

  /** Set up a BSplineInterpolateImageFunction for comparision. */
  typedef itk::BSplineInterpolateImageFunction<ImageType,double>
    InterpolateFunctionType;
  InterpolateFunctionType::Pointer interpolate = InterpolateFunctionType::New();

  interpolate->SetSplineOrder( SplineOrder );
  interpolate->SetInputImage( source->GetOutput() );

  /** Compare 10 values at random points. */
  ResampleFunctionType::PointType point;
  double minValue = origin[0];
  double maxValue = origin[0] + spacing[0] * static_cast<double>(size[0] - 1);

  for ( unsigned int k = 0; k < 10; k ++ )
    {

    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      point[j] = vnl_sample_uniform( minValue, maxValue );
      }

    double f = resample->Evaluate( point );
    double g = interpolate->Evaluate( point );

    if ( vnl_math_abs( f - g ) > 1e-5 )
      {
      std::cout << "Resample and Interpolated point are different." << std::endl;
      std::cout << " point: " << point << std::endl;
      std::cout << " resample: " << resample->Evaluate( point ) << std::endl;
      std::cout << " interpolate: " << interpolate->Evaluate( point ) << std::endl;
      std::cout << " Test failed. " << std::endl;
      return EXIT_FAILURE;
      }

    }

  return EXIT_SUCCESS;

}
