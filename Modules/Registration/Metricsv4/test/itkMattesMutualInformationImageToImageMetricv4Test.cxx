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

#include "itkMattesMutualInformationImageToImageMetricv4.h"

#include "itkLinearInterpolateImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkTextOutput.h"
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkImageMaskSpatialObject.h"
#include "itkTimeProbe.h"

#include <iostream>

/**
 * This test was copied for v4 metric from itkMattesMutualInformationImageToMetricTest
 */

/**
 * TODO: check this text:
 *
 *  This templated function tests the MattesMutualInformationImageToMetricv4
 *  class using an AfffineTransform and various interpolators.
 *
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the mutual information value and derivatives
 *  for various shift values in (-10,10). Then it checks the numerical
 *  accuracy of computed derivatives by perturbing parameters by
 *  delta = 0.001.
 *
 */
template< typename TImage, typename TInterpolator>
int TestMattesMetricWithAffineTransform(
  TInterpolator * const interpolator, const bool useSampling, const size_t imageSize )
{

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  //Allocate Images
  typedef TImage                                  MovingImageType;
  typedef TImage                                  FixedImageType;
  typedef typename MovingImageType::SizeValueType SizeValueType;

  const unsigned int ImageDimension = MovingImageType::ImageDimension;
  //Image size is scaled to represent sqrt(256^3)
  typename MovingImageType::SizeType size = {{static_cast<SizeValueType>(imageSize),
                                              static_cast<SizeValueType>(imageSize)}};
  typename MovingImageType::IndexType index = {{0,0}};
  typename MovingImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  typename MovingImageType::SpacingType imgSpacing;
  imgSpacing[0] = 3.0;
  imgSpacing[1] = 2.0;

  typename MovingImageType::PointType imgOrigin;
  imgOrigin[0] = 0.001;
  imgOrigin[1] = -0.002;

  typename MovingImageType::Pointer imgMoving = MovingImageType::New();
  imgMoving->SetLargestPossibleRegion( region );
  imgMoving->SetBufferedRegion( region );
  imgMoving->SetRequestedRegion( region );
  imgMoving->Allocate();
  imgMoving->SetSpacing( imgSpacing );
  imgMoving->SetOrigin( imgOrigin );

  typename FixedImageType::Pointer imgFixed = FixedImageType::New();
  imgFixed->SetLargestPossibleRegion( region );
  imgFixed->SetBufferedRegion( region );
  imgFixed->SetRequestedRegion( region );
  imgFixed->Allocate();
  imgFixed->SetSpacing( imgSpacing );
  imgFixed->SetOrigin( imgOrigin );

  // Fill images with a 2D gaussian
  typedef  itk::ImageRegionIterator<MovingImageType> ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<FixedImageType>  TargetIteratorType;

  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;

  itk::Point<double,2>  p;

  // Set the displacement
  itk::Vector<double,2> displacement;
  displacement[0] = 5;
  displacement[1] = 5;

    {
    ReferenceIteratorType ri(imgMoving,region);
    ri.GoToBegin();
    while(!ri.IsAtEnd())
      {
      p[0] = ri.GetIndex()[0];
      p[1] = ri.GetIndex()[1];
      itk::Vector<double,2> d = p-center;
      d += displacement;
      const double x = d[0];
      const double y = d[1];
      ri.Set( (unsigned char) ( 200.0 * std::exp( - ( x*x + y*y )/(s*s) ) ) );
      ++ri;
      }
    }
    {
    TargetIteratorType ti(imgFixed,region);
    ti.GoToBegin();
    while(!ti.IsAtEnd())
      {
      p[0] = ti.GetIndex()[0];
      p[1] = ti.GetIndex()[1];
      itk::Vector<double,2> d = p-center;
      const double x = d[0];
      const double y = d[1];
      ti.Set( (unsigned char) ( 200.0 * std::exp( - ( x*x + y*y )/(s*s) ) ) );
      ++ti;
      }
    }

  //Setup a fixed image mask for the image
  typename MovingImageType::Pointer imgMovingMask = MovingImageType::New();
  imgMovingMask->CopyInformation(imgMoving);
  imgMovingMask->SetRegions(region);
  imgMovingMask->Allocate(true); // initialize buffer to zero

  typename FixedImageType::Pointer imgFixedMask   = FixedImageType::New();
  imgFixedMask->CopyInformation(imgFixed);
  imgFixedMask->SetRegions(region);
  imgFixedMask->Allocate(true); // initialize buffer to zero

    {
      {
      //Set up a mask that only has every third voxel listed is used in fixed image region
      int count=0;
      ReferenceIteratorType ri1(imgMovingMask,region);
      ri1.GoToBegin();
      while(!ri1.IsAtEnd()) //Set all moving mask voxels to 1
        {
        if(count%3 == 0)
          {
          ri1.Set(1);
          }
        ++ri1;
        ++count;
        }
      }

      {
      //Set up a mask that only has every other voxel listed is used in fixed image region
      int count=0;
      TargetIteratorType ti1(imgFixedMask,region);
      ti1.GoToBegin();
      while(!ti1.IsAtEnd())//Set a subset of fixed mask voxels to 1, so that requested number can be made more than possible number
        {
        if(count%2 == 0)
          {
          ti1.Set(1);
          }
        ++ti1;
        ++count;
        }
      }
    }

//-----------------------------------------------------------
// Set up a transformer
//-----------------------------------------------------------
  typedef itk::AffineTransform< double, ImageDimension > TransformType;
  typedef typename TransformType::ParametersType         ParametersType;

  typename TransformType::Pointer transformer = TransformType::New();

//------------------------------------------------------------
// Set up the metric
//------------------------------------------------------------
  typedef itk::MattesMutualInformationImageToImageMetricv4<
    FixedImageType, MovingImageType > MetricType;

  typename MetricType::Pointer metric = MetricType::New();

  // Sanity check before metric is run, these should be ITK_NULLPTR;
  if( metric->GetJointPDFDerivatives().IsNotNull() )
    {
    return EXIT_FAILURE;
    }
  if( metric->GetJointPDF().IsNotNull() )
    {
    return EXIT_FAILURE;
    }

  // connect the interpolator
  metric->SetMovingInterpolator( interpolator );

  // connect the transform
  metric->SetMovingTransform( transformer );

  // connect the images to the metric
  metric->SetFixedImage( imgFixed );
  metric->SetMovingImage( imgMoving );

  // set the number of histogram bins
  metric->SetNumberOfHistogramBins( 50 );

  // this test doesn't pass when using gradient image filters,
  // presumably because of different deriviative scaling created
  // by the filter output. The derivative results match those
  // from when using the central difference calculator, but are
  // scaled by a factor of 3.
  metric->SetUseFixedImageGradientFilter(false);
  metric->SetUseMovingImageGradientFilter(false);
  //
  //-------------------------------------------------------
  // exercise misc member functions
  //-------------------------------------------------------
  std::cout << "Name of class: " << metric->GetNameOfClass() << std::endl;
  std::cout << "No. of histogram bin used = " << metric->GetNumberOfHistogramBins() << std::endl;
  if( metric->GetJointPDF().IsNotNull() )
    {
    std::cout << "JointPDF image info: " << metric->GetJointPDF() << std::endl;
    }
  if( metric->GetJointPDFDerivatives().IsNotNull() )
    {
    std::cout << "JointPDFDerivative image info: " << metric->GetJointPDFDerivatives() << std::endl;
    }
  std::cout << "GetNumberOfThreadsUsed: " << metric->GetNumberOfThreadsUsed() << std::endl;
  metric->Print(std::cout);

  // Now start the algorithmc testing

  std::cout << "useSampling: " << useSampling << std::endl;
  if( useSampling )
    {
    typedef typename MetricType::FixedSampledPointSetType PointSetType;
    typedef typename PointSetType::PointType              PointType;
    typename PointSetType::Pointer                        pset(PointSetType::New());
    unsigned int ind=0;
    unsigned int ct=0;
    itk::ImageRegionIteratorWithIndex<FixedImageType> It(imgFixed, imgFixed->GetLargestPossibleRegion() );
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      // take every N^th point
      if ( ct % 5 == 0  )
        {
        PointType pt;
        imgFixed->TransformIndexToPhysicalPoint( It.GetIndex(), pt);
        pset->SetPoint(ind, pt);
        ind++;
        }
        ct++;
      }
    std::cout << "Setting point set with " << ind << " points of " << imgFixed->GetLargestPossibleRegion().GetNumberOfPixels() << " total " << std::endl;
    metric->SetFixedSampledPointSet( pset );
    metric->SetUseFixedSampledPointSet( true );
    }

  // initialize the metric before use
  metric->Initialize();

//------------------------------------------------------------
// Set up a affine transform parameters
//------------------------------------------------------------
  transformer->SetIdentity();
  const unsigned int numberOfParameters = transformer->GetNumberOfParameters();
  ParametersType parameters = transformer->GetParameters();

//---------------------------------------------------------
// Print out mutual information values
// for parameters[4] = {-10,10} (arbitrary choice)
//---------------------------------------------------------

  typename MetricType::MeasureType metricValueWithDerivative;
  typename MetricType::MeasureType metricValueOnly;
  typename MetricType::DerivativeType derivative( numberOfParameters );


  bool testFailed = false;

  itk::TimeProbe timerGetValueAndDerivative;
  itk::TimeProbe timerGetValue;

  std::cout << "param[4]\tMI\tMI2\tdMI/dparam[4]" << std::endl;
  for( double trans = -10; trans <= 10; trans += 0.5 )
    {
    parameters[4] = trans;
    transformer->SetParameters( parameters );
    timerGetValueAndDerivative.Start();
    metric->GetValueAndDerivative( metricValueWithDerivative, derivative );
    timerGetValueAndDerivative.Stop();
    timerGetValue.Start();
    metricValueOnly = metric->GetValue();
    timerGetValue.Stop();

    std::cout << "OffsetParam: " << trans << "\tvalueWithDerivative: " << metricValueWithDerivative << "\tvalueOnly: " <<
      metricValueOnly << "\tderivative[4]: " << derivative[4];
    // Make sure the metric value calculation is
    // consistent
    if( ! itk::Math::FloatAlmostEqual( metricValueWithDerivative, metricValueOnly, 8 ) )
      {
      std::cout << "\t[FAILED]: metricValueWithDerivative values do not match: ("
                << metricValueWithDerivative << " - " << metricValueOnly <<  ") = "
                << (metricValueWithDerivative - metricValueOnly ) << std::endl;
      testFailed = true;
      }
    else
      {
      std::cout << "\t[PASSED]" << std::endl;
      }
    }
  std::cerr << "GetValueAndDerivative took " << timerGetValueAndDerivative.GetMean() << " seconds.\n";
  std::cerr << "GetValue took " << timerGetValue.GetMean() << " seconds.\n";

  std::cout << "NumberOfValidPoints: " << metric->GetNumberOfValidPoints() << " of " << metric->GetVirtualRegion().GetNumberOfPixels() << std::endl;

//---------------------------------------------------------
// Check output gradients for numerical accuracy
//---------------------------------------------------------
  parameters[4] = 0;
  transformer->SetParameters( parameters );
  metric->Initialize();
  metric->GetValueAndDerivative( metricValueWithDerivative, derivative );

  ParametersType parameters1Plus( numberOfParameters );
  ParametersType parameters2Plus( numberOfParameters );
  ParametersType parameters1Minus( numberOfParameters );
  ParametersType parameters2Minus( numberOfParameters );

  const double delta = 0.00001;

  const double tolerance = (useSampling) ? static_cast<double>(0.075) : static_cast<double>(0.014);
  for( unsigned int perturbParamIndex = 0; perturbParamIndex < numberOfParameters; ++perturbParamIndex )
    {
    //copy the parameters and perturb the current one.
    for( unsigned int j = 0; j < numberOfParameters; ++j )
      {
      if( j == perturbParamIndex )
        {
        parameters1Plus[j] = parameters[perturbParamIndex] + delta;      //positive perturbation
        parameters2Plus[j] = parameters[perturbParamIndex] + 2.0*delta;  //positive perturbation
        parameters1Minus[j] = parameters[perturbParamIndex] - delta;     //negative perturbation
        parameters2Minus[j] = parameters[perturbParamIndex] - 2.0*delta; //negative perturbation
        }
      else
        {
        parameters1Plus[j] = parameters[j];
        parameters1Minus[j] = parameters[j];
        parameters2Plus[j] = parameters[j];
        parameters2Minus[j] = parameters[j];
        }
      }

    transformer->SetParameters( parameters1Plus );
    const typename MetricType::MeasureType measure1Plus = metric->GetValue();

    transformer->SetParameters( parameters1Minus );
    const typename MetricType::MeasureType measure1Minus = metric->GetValue();
    //Compute a first-order accuracy first order derivative
    const double firstOrderApproxDerivative = -1.0 * ( measure1Plus - measure1Minus ) / ( 2.0 * delta );
    //Compute a second-order accuracy first order derivative
    transformer->SetParameters( parameters2Plus );
    const typename MetricType::MeasureType measure2Plus = metric->GetValue();

    transformer->SetParameters( parameters2Minus );
    const typename MetricType::MeasureType measure2Minus = metric->GetValue();

    //Computing second order derivative to get a handle on
    //stability of estimates from the metric
    const double secondOrderApproxDerivative = -1.0 * (
      (-1.0/12.0)*measure2Plus + (+1.0/12.0)*measure2Minus +
      (+2.0/3.0)*measure1Plus + (-2.0/3.0)*measure1Minus ) / ( delta );
    const double ratio = derivative[perturbParamIndex]/secondOrderApproxDerivative;

    std::cout << "perturbParamIndex: " << perturbParamIndex
      << "\tparameters[: " << parameters[perturbParamIndex]
      << "]\tmetric->GetDerivative[" << derivative[perturbParamIndex]
      << "]\n\t\tsecondOrderApproxDerivative[" << secondOrderApproxDerivative
      << "]\tfirstOrderApproxDerivative[" << firstOrderApproxDerivative
      << "]\tratio: " << ratio;

    const double evalDiff = itk::Math::abs( ratio - 1.0 );
    if ( evalDiff > tolerance )
      {
      std::cout << "\t[FAILED] computed derivative differ from central difference by (" << evalDiff << " > " << tolerance << ")." << std::endl;
      testFailed = true;
      }
    else
      {
      std::cout << "\t[PASSED]" << std::endl;
      }
    }

  if( testFailed )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}

/**
 * Test entry point.
 */
int itkMattesMutualInformationImageToImageMetricv4Test(int, char *[] )
{

  const size_t imageSize = 100; //NOTE 100 is very small

  //typedef itk::Image<unsigned char,2> ImageType;
  typedef itk::Image<double,2> ImageType;

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // Test metric with a linear interpolator
  typedef itk::LinearInterpolateImageFunction< ImageType, double > LinearInterpolatorType;

  LinearInterpolatorType::Pointer linearInterpolator = LinearInterpolatorType::New();

  std::cout << "Test metric with a linear interpolator." << std::endl;
  bool useSampling = false;
  int failed = true;
  failed = TestMattesMetricWithAffineTransform<ImageType,LinearInterpolatorType>( linearInterpolator, useSampling, imageSize );
  if ( failed )
    {
    std::cout << "Test failed when using all the pixels instead of sampling" << std::endl;
    return EXIT_FAILURE;
    }
  useSampling = true;
  failed = TestMattesMetricWithAffineTransform<ImageType,LinearInterpolatorType>( linearInterpolator, useSampling, imageSize );
  if ( failed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
  // Test metric with a BSpline interpolator
  typedef itk::BSplineInterpolateImageFunction< ImageType, double > BSplineInterpolatorType;

  BSplineInterpolatorType::Pointer bSplineInterpolator = BSplineInterpolatorType::New();

  bSplineInterpolator->SetSplineOrder( 3 );
  useSampling = false;
  std::cout << "Test metric with a BSpline interpolator and no sampling." << std::endl;
  failed = TestMattesMetricWithAffineTransform<ImageType,BSplineInterpolatorType>( bSplineInterpolator, useSampling, imageSize );
  if ( failed )
    {
    std::cout << "Test failed when using all the pixels instead of sampling" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
