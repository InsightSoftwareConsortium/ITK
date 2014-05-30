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
  TInterpolator * interpolator, bool useSampling )
{

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  //Allocate Images
  typedef TImage           MovingImageType;
  typedef TImage           FixedImageType;
  enum { ImageDimension = MovingImageType::ImageDimension };

  typename MovingImageType::SizeType size = {{100,100}};
  typename MovingImageType::IndexType index = {{0,0}};
  typename MovingImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  typename MovingImageType::SpacingType imgSpacing;
  imgSpacing[0] = 3.0;
  imgSpacing[1] = 2.0;

  typename MovingImageType::PointType imgOrigin;
  imgOrigin[0] = 0.0;
  imgOrigin[1] = 0.0;

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
  typedef  itk::ImageRegionIterator<MovingImageType>
    ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<FixedImageType>
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
  displacement[1] = 5;

  ReferenceIteratorType ri(imgMoving,region);
  TargetIteratorType ti(imgFixed,region);
  ri.GoToBegin();
  while(!ri.IsAtEnd())
    {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
    d = p-center;
    d += displacement;
    const double x = d[0];
    const double y = d[1];
    ri.Set( (unsigned char) ( 200.0 * std::exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ri;
    }

  ti.GoToBegin();
  while(!ti.IsAtEnd())
    {
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
    d = p-center;
    const double x = d[0];
    const double y = d[1];
    ti.Set( (unsigned char) ( 200.0 * std::exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ti;
    }

  //Setup a fixed image mask for the image
  typename MovingImageType::Pointer imgMovingMask = MovingImageType::New();
  imgMovingMask->CopyInformation(imgMoving);
  imgMovingMask->SetRegions(region);
  imgMovingMask->Allocate(true); // initialize
                                                        // buffer to zero

  typename FixedImageType::Pointer imgFixedMask   = FixedImageType::New();
  imgFixedMask->CopyInformation(imgFixed);
  imgFixedMask->SetRegions(region);
  imgFixedMask->Allocate(true); // initialize
                                                       // buffer to zero

  int NumberFixedImageMaskVoxels=0;
    {//Set up a mask that only has every 10th voxel listed is used in fixed image region
    //This should result in only about 588 samples
      {
      ReferenceIteratorType ri1(imgMovingMask,region);
      ri1.GoToBegin();
      while(!ri1.IsAtEnd()) //Set all moving mask voxels to 1
        {
        ri1.Set(1);
        ++ri1;
        }
      }

      {
      int count=0;
      TargetIteratorType ti1(imgFixedMask,region);
      ti1.GoToBegin();
      while(!ti1.IsAtEnd())//Set a subset of fixed mask voxels to 1, so that requested number can be made more than possible number
        {
        if(count%17 == 0)
          {
          ti1.Set(1);
          ++NumberFixedImageMaskVoxels;
          }
        count++;
        ++ti1;
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

  // Sanity check before metric is run, these should be NULL;
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

  std::cout << "useSampling: " << useSampling << std::endl;
  if( useSampling )
    {
    typedef typename MetricType::FixedSampledPointSetType PointSetType;
    typedef typename PointSetType::PointType              PointType;
    typename PointSetType::Pointer                        pset(PointSetType::New());
    unsigned int ind=0,ct=0;
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
  unsigned int numberOfParameters = transformer->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );

  // set the parameters to the identity
  unsigned long count = 0;

     // initialize the linear/matrix part
  for( unsigned int row = 0; row < ImageDimension; row++ )
    {
    for( unsigned int col = 0; col < ImageDimension; col++ )
      {
      parameters[count] = 0;
      if( row == col )
        {
        parameters[count] = 1;
        }
      ++count;
      }
    }

     // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    parameters[count] = 0;
    ++count;
    }

  transformer->SetParameters( parameters );

//---------------------------------------------------------
// Print out mutual information values
// for parameters[4] = {-10,10} (arbitrary choice)
//---------------------------------------------------------

  typename MetricType::MeasureType measure, measure2;
  typename MetricType::DerivativeType derivative( numberOfParameters );

  std::cout << "param[4]\tMI\tMI2\tdMI/dparam[4]" << std::endl;

  bool testFailed = false;

  for( double trans = -10; trans <= 10; trans += 0.5 )
    {
    parameters[4] = trans;
    transformer->SetParameters( parameters );
    metric->GetValueAndDerivative( measure, derivative );
    measure2 = metric->GetValue();

    std::cout << trans << "\t" << measure << "\t" <<
      measure2 << "\t" << derivative[4] <<std::endl;

    // Make sure the metric value calculation is
    // consistent
    if( measure != measure2 )
      {
      std::cerr << "Error: measure values do not match: "
                << measure << ", " << measure2 << std::endl;
      testFailed = true;
      }
    }

  std::cout << "NumberOfValidPoints: " << metric->GetNumberOfValidPoints() << " of " << metric->GetVirtualRegion().GetNumberOfPixels() << std::endl;

//---------------------------------------------------------
// Check output gradients for numerical accuracy
//---------------------------------------------------------
  parameters[4] = 0;
  transformer->SetParameters( parameters );
  metric->Initialize();
  metric->GetValueAndDerivative( measure, derivative );

  ParametersType parametersPlus( numberOfParameters );
  ParametersType parametersMinus( numberOfParameters );
  typename MetricType::MeasureType measurePlus;
  typename MetricType::MeasureType measureMinus;

  double delta = 0.001;

  for( unsigned int i = 0; i < numberOfParameters; ++i )
    {
    //copy the parameters and perturb the current one.
    for( unsigned int j = 0; j < numberOfParameters; ++j )
      {
      if( j == i )
        {
        parametersPlus[j] = parameters[i] + delta;    //positive perturbation
        parametersMinus[j] = parameters[i] - delta;  //negative perturbation
        }
      else
        {
        parametersPlus[j] = parameters[j];
        parametersMinus[j] = parameters[j];
        }
      }

    transformer->SetParameters( parametersPlus );
    measurePlus = metric->GetValue();

    transformer->SetParameters( parametersMinus );
    measureMinus = metric->GetValue();

    double approxDerivative = -1 * ( measurePlus - measureMinus ) / ( 2 * delta );
    double ratio = derivative[i]/approxDerivative;

    std::cout << i << "\t";
    std::cout << parameters[i] << "\t";
    std::cout << derivative[i] << "\t";
    std::cout << approxDerivative << "\t";
    std::cout << ratio << "\t";
    std::cout << std::endl;

    double tolerance = static_cast<double>(0.014);
    if( useSampling )
      {
      tolerance = static_cast<double>(0.075);
      }
    if ( vnl_math_abs( ratio - 1.0 ) > tolerance )
      {
      std::cout << "computed derivative differ from central difference." << std::endl;
      testFailed = true;
      }

    }

  std::cout << "GetNumberOfThreadsUsed: " << metric->GetNumberOfThreadsUsed() << std::endl;

  if( testFailed )
    {
    return EXIT_FAILURE;
    }

//-------------------------------------------------------
// exercise misc member functions
//-------------------------------------------------------
  std::cout << "Name of class: " <<
    metric->GetNameOfClass() << std::endl;
  std::cout << "No. of histogram bin used = " <<
    metric->GetNumberOfHistogramBins() << std::endl;
  if( metric->GetJointPDF().IsNotNull() )
    {
    std::cout << "JointPDF image info: " <<
      metric->GetJointPDF() << std::endl;
    }
  if( metric->GetJointPDFDerivatives().IsNotNull() )
    {
    std::cout << "JointPDFDerivative image info: " <<
      metric->GetJointPDFDerivatives() << std::endl;
    }

  metric->Print(std::cout);

  return EXIT_SUCCESS;
}

/**
 * Test entry point.
 */
int itkMattesMutualInformationImageToImageMetricv4Test(int, char *[] )
{
  int failed;

  //typedef itk::Image<unsigned char,2> ImageType;
  typedef itk::Image<double,2> ImageType;

  bool useSampling = false;

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // Test metric with a linear interpolator
  typedef itk::LinearInterpolateImageFunction< ImageType, double >
    LinearInterpolatorType;

  LinearInterpolatorType::Pointer linearInterpolator = LinearInterpolatorType::New();

  std::cout << "Test metric with a linear interpolator." << std::endl;
  failed = TestMattesMetricWithAffineTransform<ImageType,LinearInterpolatorType>( linearInterpolator, useSampling );

  if ( failed )
    {
    std::cout << "Test failed when using all the pixels instead of sampling" << std::endl;
    return EXIT_FAILURE;
    }

  useSampling = true;
  failed = TestMattesMetricWithAffineTransform<ImageType,LinearInterpolatorType>( linearInterpolator, useSampling );

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
  std::cout << "Test metric with a BSpline interpolator." << std::endl;
  failed = TestMattesMetricWithAffineTransform<ImageType,BSplineInterpolatorType>( bSplineInterpolator, useSampling );

  if ( failed )
    {
    std::cout << "Test failed when using all the pixels instead of sampling" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
