/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDecompositionImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkBSplineResampleImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkBSplineDecompositionImageFilter.h"
#include "itkImage.h"
#include "itkSize.h"
#include "itkRandomImageSource.h"
#include "vnl/vnl_sample.h"

/** Note:  This is the same test used for the itkBSplineResampleImageFunctionTest
  *        It is duplicated here because it excercises the itkBSplineDecompositionFilter
  *        and demonstrates its use.
  */

int itkBSplineDecompositionImageFilterTest(int, char **)
{

  const unsigned int ImageDimension = 2;
  typedef float PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;

  const unsigned int SplineOrder = 3;

  /** Generate a random input image and connect to BSpline decomposition filter */
  typedef itk::FixedArray<float,ImageDimension> FloatArrayType;
  typedef itk::FixedArray<unsigned long, ImageDimension> SizeType;

  FloatArrayType spacing;
  FloatArrayType origin;
  SizeType       size;

  spacing.Fill( 2.0 );
  origin.Fill ( 10.0 );
  size.Fill( 32 );

  typedef itk::RandomImageSource<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();

  source->SetSize( size.GetDataPointer() );
  source->SetSpacing( spacing.GetDataPointer() );
  source->SetOrigin( origin.GetDataPointer() );

  source->SetMin( 0.0 );
  source->SetMax( 10.0 );

  typedef itk::BSplineDecompositionImageFilter<ImageType,ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetSplineOrder( SplineOrder );
  filter->SetInput( source->GetOutput() );
  filter->Update();

  filter->Print( std::cout );

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

