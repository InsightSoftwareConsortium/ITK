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

#include "itkMattesMutualInformationImageToImageMetric.h"

#include "itkLinearInterpolateImageFunction.h"
#include "itkTextOutput.h"
#include "itkBSplineTransform.h"
#include "itkImageMaskSpatialObject.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

/**
 *  This templated function test the MattesMutualInformationImageToMetric
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
 *
 */
template< typename TImage, typename TInterpolator>
int TestMattesMetricWithAffineTransform(
  TInterpolator * interpolator, bool useSampling,
  bool useExplicitJointPDFDerivatives, bool useCachingBSplineWeights )
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
  typedef itk::MattesMutualInformationImageToImageMetric<
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
  metric->SetInterpolator( interpolator );

  // connect the transform
  metric->SetTransform( transformer );

  // connect the images to the metric
  metric->SetFixedImage( imgFixed );
  metric->SetMovingImage( imgMoving );

  // set the number of histogram bins
  metric->SetNumberOfHistogramBins( 50 );

  metric->SetUseExplicitPDFDerivatives( useExplicitJointPDFDerivatives );
  metric->SetUseCachingOfBSplineWeights( useCachingBSplineWeights );
  metric->ReinitializeSeed(121212);

  if( useSampling )
    {
    // set the number of samples to use
    //metric->SetNumberOfSpatialSamples( 500 );
      {
      // NOTE: This number of spatial samples is to be larger than possible
      // and it will be truncated to the size of the image.

      // convert mask image to mask
      typedef itk::ImageMaskSpatialObject<ImageDimension> ImageMaskSpatialObjectType;
      typename ImageMaskSpatialObjectType::Pointer soMovingMask  = ImageMaskSpatialObjectType::New();
      soMovingMask->SetImage( imgMovingMask );
      soMovingMask->ComputeObjectToWorldTransform();
      typename ImageMaskSpatialObjectType::Pointer soFixedMask  = ImageMaskSpatialObjectType::New();
      soFixedMask->SetImage( imgFixedMask );
      soFixedMask->ComputeObjectToWorldTransform();

      metric->SetMovingImageMask(soMovingMask);
      metric->SetFixedImageMask(soFixedMask);

      // Make the mask const to enhance code coverage
      typename ImageMaskSpatialObjectType::ConstPointer soMovingConstMask = soMovingMask.GetPointer();
      typename ImageMaskSpatialObjectType::ConstPointer soFixedConstMask  = soFixedMask.GetPointer();
      metric->SetMovingImageMask(soMovingConstMask);
      metric->SetFixedImageMask(soFixedConstMask);

      //metric->SetNumberOfSpatialSamples( static_cast<unsigned long int>(NumberFixedImageMaskVoxels*.2) );
      metric->SetNumberOfSpatialSamples( static_cast<unsigned long int>(NumberFixedImageMaskVoxels*2) );
      }
    }
  else
    {
    metric->UseAllPixelsOn();
    }

  // set the region over which to compute metric
  index.Fill(2);
  size.Fill(96);
  region.SetSize( size );
  region.SetIndex( index );
  metric->SetFixedImageRegion( region );

  // initialize the metric before use
  metric->DebugOn();
  metric->Initialize();
  metric->DebugOff();

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

//---------------------------------------------------------
// Print out mutual information values
// for parameters[4] = {-10,10} (arbitrary choice)
//---------------------------------------------------------

  typename MetricType::MeasureType measure, measure2;
  typename MetricType::DerivativeType derivative( numberOfParameters );

  std::cout << "param[4]\tMI\tMI2\tdMI/dparam[4]" << std::endl;

  for( double trans = -10; trans <= 5; trans += 0.5 )
    {
    parameters[4] = trans;
    metric->GetValueAndDerivative( parameters, measure, derivative );
    measure2 = metric->GetValue( parameters );

    std::cout << trans << "\t" << measure << "\t" <<
      measure2 << "\t" << derivative[4] <<std::endl;

    // exercise the other functions
    metric->GetDerivative( parameters, derivative );

    }

//---------------------------------------------------------
// Check output gradients for numerical accuracy
//---------------------------------------------------------
  parameters[4] = 0;
  metric->GetValueAndDerivative( parameters, measure, derivative );

  ParametersType parametersPlus( numberOfParameters );
  ParametersType parametersMinus( numberOfParameters );
  typename MetricType::MeasureType measurePlus;
  typename MetricType::MeasureType measureMinus;

  double delta = 0.001;

  bool testFailed = false;

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

    measurePlus = metric->GetValue( parametersPlus );
    measureMinus = metric->GetValue( parametersMinus );

    double approxDerivative = ( measurePlus - measureMinus ) / ( 2 * delta );
    double ratio = derivative[i]/approxDerivative;

    std::cout << i << "\t";
    std::cout << parameters[i] << "\t";
    std::cout << derivative[i] << "\t";
    std::cout << approxDerivative << "\t";
    std::cout << ratio << "\t";
    std::cout << std::endl;

    if ( itk::Math::abs( ratio - 1.0 ) > 0.012 )
      {
      std::cout << "computed derivative differ from central difference." << std::endl;
      testFailed = true;
      }

    }

  if( testFailed )
    {
    return EXIT_FAILURE;
    }

//-------------------------------------------------------
// exercise misc member functions
//-------------------------------------------------------
  std::cout << "Name of class: " <<
    metric->GetNameOfClass() << std::endl;
  std::cout << "No. of samples used = " <<
    metric->GetNumberOfSpatialSamples() << std::endl;
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
 *  This templated function test the MattesMutualInformationImageToMetric
 *  class using an BSplineTransform and various interpolators.
 *
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the mutual information value and derivatives
 *  for various shift values in (-10,10).
 *
 *
 */
template< typename TImage, typename TInterpolator>
int TestMattesMetricWithBSplineTransform(
  TInterpolator * interpolator, bool useSampling,
  bool useExplicitJointPDFDerivatives, bool useCachingBSplineWeights )
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
  imgSpacing[0] = 1.5;
  imgSpacing[1] = 1.5;

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

//-----------------------------------------------------------
// Set up a transformer
//-----------------------------------------------------------
  typedef itk::BSplineTransform< double, ImageDimension, 3 > TransformType;
  typedef typename TransformType::ParametersType             ParametersType;
  typename TransformType::PhysicalDimensionsType dimensions;
  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    dimensions[dim] = imgFixed->GetSpacing()[dim] *
      ( imgFixed->GetLargestPossibleRegion().GetSize()[dim] - 1 );
    }
  typename TransformType::MeshSizeType meshSize;
  meshSize.Fill( 4 );

  typename TransformType::Pointer transformer = TransformType::New();

  transformer->SetTransformDomainPhysicalDimensions( dimensions );
  transformer->SetTransformDomainOrigin( imgFixed->GetOrigin() );
  transformer->SetTransformDomainDirection( imgFixed->GetDirection() );
  transformer->SetTransformDomainMeshSize( meshSize );

  transformer->Print( std::cout );

//------------------------------------------------------------
// Set up the metric
//------------------------------------------------------------
  typedef itk::MattesMutualInformationImageToImageMetric<
    FixedImageType, MovingImageType > MetricType;

  typename MetricType::Pointer metric = MetricType::New();

  // connect the interpolator
  metric->SetInterpolator( interpolator );

  // connect the transform
  metric->SetTransform( transformer );

  // connect the images to the metric
  metric->SetFixedImage( imgFixed );
  metric->SetMovingImage( imgMoving );

  // set the number of histogram bins
  metric->SetNumberOfHistogramBins( 50 );

  metric->SetUseExplicitPDFDerivatives( useExplicitJointPDFDerivatives );
  metric->SetUseCachingOfBSplineWeights( useCachingBSplineWeights );

  if( useSampling )
    {
    // set the number of samples to use
    metric->SetNumberOfSpatialSamples( 500 );
    }
  else
    {
    metric->UseAllPixelsOn();
    }

  // set the region over which to compute metric
  metric->SetFixedImageRegion( imgFixed->GetBufferedRegion() );

  // initialize the metric before use
  metric->Initialize();

//------------------------------------------------------------
// Set up a B-spline deformable transform parameters
//------------------------------------------------------------
  unsigned int numberOfParameters = transformer->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );
  parameters.Fill( 0.0 );

//---------------------------------------------------------
// Print out mutual information values
// for parameters between {-10,10} (arbitrary choice)
//---------------------------------------------------------

  typename MetricType::MeasureType measure, measure2;
  typename MetricType::DerivativeType derivative( numberOfParameters );
  unsigned int q = numberOfParameters / 4;

  std::cout << "q = " << q << std::endl;
  std::cout << "param[q]\tMI\tMI2\tdMI/dparam[q]" << std::endl;

  for( double trans = -10; trans <= 5; trans += 0.5 )
    {
   // parameters[q] = trans;
    parameters.Fill( trans );
    metric->GetValueAndDerivative( parameters, measure, derivative );
    measure2 = metric->GetValue( parameters );

    std::cout << trans << "\t" << measure << "\t" <<
      measure2 << "\t" << derivative[q] <<std::endl;

    // exercise the other functions
    metric->GetDerivative( parameters, derivative );

    }

//---------------------------------------------------------
// Check output gradients for numerical accuracy
//---------------------------------------------------------
  parameters.Fill( 4.5 * imgSpacing[0] );
  metric->GetValueAndDerivative( parameters, measure, derivative );

  ParametersType parametersPlus( numberOfParameters );
  ParametersType parametersMinus( numberOfParameters );
  typename MetricType::MeasureType measurePlus;
  typename MetricType::MeasureType measureMinus;

  double delta = 0.1 * imgSpacing[0];

  bool testFailed = false;

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

    measurePlus = metric->GetValue( parametersPlus );
    const unsigned long numberPlusSamples= metric->GetNumberOfMovingImageSamples();
    measureMinus = metric->GetValue( parametersMinus );
    const unsigned long numberMinusSamples= metric->GetNumberOfMovingImageSamples();

    //Test was failing due to some +-perturbed points viewed as
    //"inside", while -=perturbed points were outside, and
    //vice-versa. This logic ensures that such points are excluded
    //from the test.
    if ( numberPlusSamples!= numberMinusSamples)
      {
      continue;
      }
    double approxDerivative = ( measurePlus - measureMinus ) / ( 2 * delta );
    double ratio = derivative[i]/approxDerivative;

    std::cout << i << "\t";
    std::cout << parameters[i] << "\t";
    std::cout << derivative[i] << "\t";
    std::cout << approxDerivative << "\t";
    std::cout << ratio << "\t";
    std::cout << std::endl;

    if ( itk::Math::abs( ratio - 1.0 ) > 0.05 && itk::Math::abs( derivative[i] ) > 1e-4 )
      {
      std::cout << "computed derivative differ from central difference." << std::endl;
      testFailed = true;
      }

    }

  if( testFailed )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}

int itkMattesMutualInformationImageToImageMetricTest(int argc, char * argv [] )
{

  bool useExplicitJointPDFDerivatives = true;
  bool useCachingBSplineWeights = true;

  if( argc > 1 )
    {
    useExplicitJointPDFDerivatives = atoi( argv[1] );
    }

  if( argc > 2 )
    {
    useCachingBSplineWeights = atoi( argv[2] );
    }

  int failed;
  typedef itk::Image<unsigned char,2> ImageType;

  bool useSampling = true;

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // Test metric with a linear interpolator
  typedef itk::LinearInterpolateImageFunction< ImageType, double >
    LinearInterpolatorType;

  LinearInterpolatorType::Pointer linearInterpolator
    = LinearInterpolatorType::New();

  failed = TestMattesMetricWithAffineTransform<ImageType,LinearInterpolatorType>(
    linearInterpolator, useSampling, useExplicitJointPDFDerivatives, useCachingBSplineWeights );

  if ( failed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  useSampling = false;
  failed = TestMattesMetricWithAffineTransform<ImageType,LinearInterpolatorType>(
    linearInterpolator, useSampling, useExplicitJointPDFDerivatives, useCachingBSplineWeights );

  if ( failed )
    {
    std::cout << "Test failed when using all the pixels instead of sampling" << std::endl;
    return EXIT_FAILURE;
    }

  // Test metric with a BSpline interpolator
  typedef itk::BSplineInterpolateImageFunction< ImageType, double >
    BSplineInterpolatorType;

  BSplineInterpolatorType::Pointer bSplineInterpolator
    = BSplineInterpolatorType::New();

  bSplineInterpolator->SetSplineOrder( 3 );

  useSampling = true;
  failed = TestMattesMetricWithAffineTransform<ImageType,BSplineInterpolatorType>(
    bSplineInterpolator, useSampling, useExplicitJointPDFDerivatives, useCachingBSplineWeights );

  if ( failed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  useSampling = false;
  failed = TestMattesMetricWithAffineTransform<ImageType,BSplineInterpolatorType>(
    bSplineInterpolator, useSampling, useExplicitJointPDFDerivatives, useCachingBSplineWeights );

  if ( failed )
    {
    std::cout << "Test failed when using all the pixels instead of sampling" << std::endl;
    return EXIT_FAILURE;
    }

  // Test metric with BSpline deformable transform
  useSampling = true;
  failed = TestMattesMetricWithBSplineTransform<
    ImageType,BSplineInterpolatorType>( bSplineInterpolator, useSampling,
        useExplicitJointPDFDerivatives, useCachingBSplineWeights );

  if ( failed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Test metric with BSpline deformable transform and using all the pixels
  //
  // We know this test particular combination is not working yet,
  // but we left the test here in order to help with the debugging.
  //
  /*
  std::cout << "Test metric with BSpline deformable transform and using all the pixels" << std::endl;
  useSampling = false;
  failed = TestMattesMetricWithBSplineTransform<
    ImageType,BSplineInterpolatorType>( bSplineInterpolator, useSampling );

  if ( failed )
    {
    std::cout << "Test failed when using all the pixels instead of sampling" << std::endl;
    return EXIT_FAILURE;
    }
  */

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
