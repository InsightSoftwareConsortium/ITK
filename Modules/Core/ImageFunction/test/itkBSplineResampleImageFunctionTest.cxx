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
#include "itkFilterWatcher.h"
#include "vnl/vnl_sample.h"
#include "makeRandomImageBsplineInterpolator.h"

/** Note:  This is the same test used for the itkBSplineDecompositionFilter
  *        It is duplicated here because it excercises the itkBSplineResampleImageFunctionTest
  *        and demonstrates its use.
  */

int itkBSplineResampleImageFunctionTest(int, char* [] )
{
  const unsigned int ImageDimension = 2;
  typedef float                                PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::BSplineInterpolateImageFunction<ImageType,double, double> BSplineInterpolatorFunctionType;

  const unsigned int SplineOrder = 3;
  BSplineInterpolatorFunctionType::Pointer interpolator = makeRandomImageInterpolator<BSplineInterpolatorFunctionType>(SplineOrder);
  ImageType::ConstPointer randImage = interpolator->GetInputImage();

  typedef itk::BSplineDecompositionImageFilter<ImageType,ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter,"filter");

  filter->SetSplineOrder( interpolator->GetSplineOrder() );
  filter->SetInput( randImage );
  filter->Update();

  filter->Print( std::cout );

  /** Set up a BSplineResampleImageFunction. */
  typedef itk::BSplineResampleImageFunction<ImageType,double> ResampleFunctionType;
  ResampleFunctionType::Pointer resample = ResampleFunctionType::New();

  resample->SetSplineOrder( interpolator->GetSplineOrder());
  resample->SetInputImage( filter->GetOutput() );

  /** Compare 10 values at random points. */

  ImageType::IndexType Last;
  Last.Fill(0);
  Last[0]=randImage->GetLargestPossibleRegion().GetSize()[0]-1;
  ImageType::PointType LastPhysicalLocation;
  randImage->TransformIndexToPhysicalPoint(Last,LastPhysicalLocation);

  const double minValue = randImage->GetOrigin()[0];
  const double maxValue = LastPhysicalLocation[0];

  for ( unsigned int k = 0; k < 10; k ++ )
    {
    ResampleFunctionType::PointType point;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      point[j] = vnl_sample_uniform( minValue, maxValue );
      }

    const double f = resample->Evaluate( point );
    const double g = interpolator->Evaluate( point );

    if ( itk::Math::abs( f - g ) > 1e-5 )
      {
      std::cout << "Resample and Interpolated point are different." << std::endl;
      std::cout << " point: " << point << std::endl;
      std::cout << " resample: " << resample->Evaluate( point ) << std::endl;
      std::cout << " interpolator: " << interpolator->Evaluate( point ) << std::endl;
      std::cout << " Test failed. " << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
