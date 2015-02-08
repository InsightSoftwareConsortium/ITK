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

#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkGaussianImageSource.h"
#include "itkPointSet.h"

#include <iostream>
#include "itkStdStreamStateSave.h"

/**
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the NormalizedCorrelation value and derivatives
 *  for various shift values in (-10,10).
 *
 */

int itkNormalizedCorrelationPointSetToImageMetricTest(int, char* [] )
{

// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
  itk::StdStreamStateSave coutState(std::cout);

//------------------------------------------------------------
// Create two simple images
//------------------------------------------------------------

  const unsigned int ImageDimension = 2;

  typedef double                   PixelType;

  typedef double                   CoordinateRepresentationType;

  //Allocate Images
  typedef itk::Image<PixelType,ImageDimension>         MovingImageType;
  typedef itk::Image<PixelType,ImageDimension>         FixedImageType;

  // Declare Gaussian Sources
  typedef itk::GaussianImageSource< MovingImageType >  MovingImageSourceType;
  typedef itk::GaussianImageSource< FixedImageType  >  FixedImageSourceType;

  // Note: the following declarations are classical arrays
  FixedImageType::SizeValueType fixedImageSize[]     = {  100,  100 };
  MovingImageType::SizeValueType movingImageSize[]    = {  100,  100 };

  FixedImageType::SpacingValueType fixedImageSpacing[]  = { 1.0f, 1.0f };
  MovingImageType::SpacingValueType movingImageSpacing[] = { 1.0f, 1.0f };

  FixedImageType::PointValueType fixedImageOrigin[]   = { 0.0f, 0.0f };
  MovingImageType::PointValueType movingImageOrigin[]  = { 0.0f, 0.0f };

  MovingImageSourceType::Pointer movingImageSource = MovingImageSourceType::New();
  FixedImageSourceType::Pointer  fixedImageSource  = FixedImageSourceType::New();

  fixedImageSource->SetSize(    fixedImageSize    );
  fixedImageSource->SetOrigin(  fixedImageOrigin  );
  fixedImageSource->SetSpacing( fixedImageSpacing );
  fixedImageSource->SetNormalized( false );
  fixedImageSource->SetScale( 250.0f );

  movingImageSource->SetSize(    movingImageSize    );
  movingImageSource->SetOrigin(  movingImageOrigin  );
  movingImageSource->SetSpacing( movingImageSpacing );
  movingImageSource->SetNormalized( false );
  movingImageSource->SetScale( 250.0f );

  movingImageSource->Update();  // Force the filter to run
  fixedImageSource->Update();   // Force the filter to run

  MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
  FixedImageType::Pointer  fixedImage  = fixedImageSource->GetOutput();

//-----------------------------------------------------------
// Create the point set and load it with data by sampling
// the fixed image
//-----------------------------------------------------------
  typedef itk::PointSet< float, 2 >   FixedPointSetType;
  FixedPointSetType::Pointer fixedPointSet = FixedPointSetType::New();

  const unsigned int numberOfPoints = 100;

  fixedPointSet->SetPointData( FixedPointSetType::PointDataContainer::New() );

  fixedPointSet->GetPoints()->Reserve( numberOfPoints );
  fixedPointSet->GetPointData()->Reserve( numberOfPoints );

  itk::ImageRegionIterator< FixedImageType > it( fixedImage,
                                            fixedImage->GetBufferedRegion() );

  const unsigned int skip =
      fixedImage->GetBufferedRegion().GetNumberOfPixels() / numberOfPoints;

  unsigned int counter = 0;

  FixedPointSetType::PointIdentifier pointId = 0;
  FixedPointSetType::PointType  point;

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    if( counter==0 )
      {
      fixedImage->TransformIndexToPhysicalPoint( it.GetIndex(), point );
      fixedPointSet->SetPoint( pointId, point );
      fixedPointSet->SetPointData( pointId, it.Get() );
      ++pointId;
      if( pointId == numberOfPoints )
        {
        break;
        }
      counter = skip;
      }
    --counter;
    ++it;
    }


//-----------------------------------------------------------
// Set up  the Metric
//-----------------------------------------------------------
  typedef itk::NormalizedCorrelationPointSetToImageMetric<
                                       FixedPointSetType,
                                       MovingImageType >
                                                    MetricType;

  typedef MetricType::TransformType                 TransformBaseType;
  typedef TransformBaseType::ParametersType         ParametersType;

  MetricType::Pointer  metric = MetricType::New();


//-----------------------------------------------------------
// Plug the Images into the metric
//-----------------------------------------------------------
  metric->SetFixedPointSet( fixedPointSet );
  metric->SetMovingImage( movingImage );

//-----------------------------------------------------------
// Set up a Transform
//-----------------------------------------------------------

  typedef itk::TranslationTransform<
                        CoordinateRepresentationType,
                        ImageDimension >         TransformType;

  TransformType::Pointer transform = TransformType::New();

  metric->SetTransform( transform.GetPointer() );


//------------------------------------------------------------
// Set up an Interpolator
//------------------------------------------------------------
  typedef itk::LinearInterpolateImageFunction<
                    MovingImageType,
                    double > InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  interpolator->SetInputImage( movingImage.GetPointer() );

  metric->SetInterpolator( interpolator.GetPointer() );


  std::cout << metric << std::endl;


//------------------------------------------------------------
// This call is mandatory before start querying the Metric
// This method do all the necessary connections between the
// internal components: Interpolator, Transform and Images
//------------------------------------------------------------
  try {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Metric initialization failed" << std::endl;
    std::cout << "Reason " << e.GetDescription() << std::endl;

    return EXIT_FAILURE;
    }


//------------------------------------------------------------
// Set up transform parameters
//------------------------------------------------------------
  ParametersType parameters( transform->GetNumberOfParameters() );

  // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    parameters[k]= 0.0f;
    }


//---------------------------------------------------------
// Print out metric values
// for parameters[1] = {-10,10}  (arbitrary choice...)
//---------------------------------------------------------

  MetricType::MeasureType     measure;
  MetricType::DerivativeType  derivative;

  std::cout << "param[1]   Metric    d(Metric)/d(param[1] " << std::endl;

  metric->SubtractMeanOn();

  parameters[1] = -10.2;
  metric->GetValueAndDerivative( parameters, measure, derivative );
  double oldValue = measure;

  for( double trans = -10; trans <= 5; trans += 0.2 )
    {
    parameters[1] = trans;
    metric->GetValueAndDerivative( parameters, measure, derivative );

    std::cout.width(5);
    std::cout.precision(5);
    std::cout << trans;
    std::cout.width(15);
    std::cout.precision(5);
    std::cout << measure;
    std::cout.width(15);
    std::cout.precision(5);
    std::cout << derivative[1];

    // compare against finite difference approx.
    std::cout.width(15);
    std::cout.precision(5);
    std::cout << (measure-oldValue)/0.2;
    oldValue = measure;

    // exercise the other functions
    metric->GetValue( parameters );
    metric->GetDerivative( parameters, derivative );

    std::cout << std::endl;

    }

//-------------------------------------------------------
// exercise misc member functions
//-------------------------------------------------------
  std::cout << "Check case when Target is ITK_NULLPTR" << std::endl;
  metric->SetFixedPointSet( ITK_NULLPTR );
  try
    {
    std::cout << "Value = " << metric->GetValue( parameters );
    std::cout << "If you are reading this message the Metric " << std::endl;
    std::cout << "is NOT managing exceptions correctly    " << std::endl;

    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception received (as expected) "    << std::endl;
    std::cout << "Description : " << e.GetDescription() << std::endl;
    std::cout << "Location    : " << e.GetLocation()    << std::endl;
    std::cout << "Test for exception throwing... PASSED ! " << std::endl;
    }

  try
    {
    metric->GetValueAndDerivative( parameters, measure, derivative );
    std::cout << "Value = " << measure << std::endl;
    std::cout << "If you are reading this message the Metric " << std::endl;
    std::cout << "is NOT managing exceptions correctly    " << std::endl;

    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception received (as expected) "    << std::endl;
    std::cout << "Description : " << e.GetDescription() << std::endl;
    std::cout << "Location    : " << e.GetLocation()    << std::endl;
    std::cout << "Test for exception throwing... PASSED ! "  << std::endl;
    }

  return EXIT_SUCCESS;

}
