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
#ifndef itkOptImageToImageMetricsTest_h
#define itkOptImageToImageMetricsTest_h

#include "itkTimeProbe.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk {

template <typename FixedImageType,
          typename MovingImageType,
          typename InterpolatorType,
          typename TransformType,
          typename MetricType,
          typename MetricInitializerType >
class OptImageToImageMetricsTest
{
public:

  OptImageToImageMetricsTest() {}

  int RunTest( FixedImageType* fixed,
               MovingImageType* moving,
               InterpolatorType* interpolator,
               TransformType* transform,
               MetricType* metric,
               MetricInitializerType metricInitializer)
    {
    typedef typename MetricType::ParametersType ParametersType;

    std::cout << "-------------------------------------------------------------------" << std::endl;
    std::cout << "Testing" << std::endl;
    std::cout << "\tMetric       : " << metric->GetNameOfClass() << std::endl;
    std::cout << "\tInterpolator : " << interpolator->GetNameOfClass() << std::endl;
    std::cout << "\tTransform    : " << transform->GetNameOfClass() << std::endl;
    std::cout << "-------------------------------------------------------------------" << std::endl;
    std::cout << std::endl;

    int result = EXIT_SUCCESS;

    // connect the interpolator
    metric->SetInterpolator( interpolator );

    // connect the transform
    metric->SetTransform( transform );

    // connect the images to the metric
    metric->SetFixedImage( fixed );
    metric->SetMovingImage( moving );

    // call custom initialization for the metric
    metricInitializer.Initialize();

    // Always use the same seed value.
    // All instances are the same since MersenneTwisterRandomVariateGenerator
    // uses a singleton pattern.
    itk::Statistics::MersenneTwisterRandomVariateGenerator::GetInstance()->SetSeed( 42 );

    // initialize the metric
    // Samples are drawn here in metric->Initialize(),
    // so we seed the random number generator
    // immediately before this call.
    metric->Initialize();

    // Set the transform to identity
    transform->SetIdentity();

    // Get the transform parameters for identity.
    ParametersType parameters = transform->GetParameters();

    typename MetricType::MeasureType value;
    typename MetricType::DerivativeType derivative;

    // Try GetValue and GetDerivative...
    value = metric->GetValue( parameters );
    metric->GetDerivative( parameters, derivative );

    // Make a time probe
    itk::TimeProbe timeProbe;

    // Walk around the parameter value at parameterIdx
    for (unsigned int parameterIdx = 0; parameterIdx < parameters.GetSize(); parameterIdx++)
      {
      std::cout << "Param[" << parameterIdx << "]\tValue\tDerivative " << std::endl;
      double startVal = parameters[parameterIdx];
      // endVal is 10% beyond startVal.
      double endVal = 1.10 * startVal;
      // If startVal is 0, endVal needs to be fixed up.
      if( itk::Math::abs(endVal - 0.0) < 1e-8 )
        {
        endVal = startVal + 1.0;
        }
      double incr = (endVal - startVal) / 10.0;

      for( double pval = startVal; pval <= endVal; pval += incr )
        {
        parameters[parameterIdx] = pval;

        timeProbe.Start();
        metric->GetValueAndDerivative( parameters, value, derivative );
        timeProbe.Stop();

        std::cout << pval << "\t" << value << "\t" << derivative << std::endl;
        }
      }

    std::cout << std::endl;
    std::cout << "Mean time for GetValueAndDerivative : " << timeProbe.GetMean() << std::endl;
    std::cout << std::endl;
    std::cout << "------------------------------Done---------------------------------" << std::endl;

    return result;
    }

};

template <typename FixedImageType, typename MovingImageType>
class MeanSquaresMetricInitializer
{
public:
  typedef itk::MeanSquaresImageToImageMetric< FixedImageType,
                                              MovingImageType> MetricType;


  MeanSquaresMetricInitializer(MetricType* metric)
    {
    m_Metric = metric;
    }

  void Initialize()
    {
    // Do stuff on m_Metric
    m_Metric->UseAllPixelsOn();
    }

protected:
  MetricType* m_Metric;

};

template <typename FixedImageType, typename MovingImageType>
class MattesMIMetricInitializer
{
public:
  typedef itk::MattesMutualInformationImageToImageMetric< FixedImageType,
                                                          MovingImageType> MetricType;


  MattesMIMetricInitializer(MetricType* metric)
    {
    m_Metric = metric;
    }

  void Initialize()
    {
    // Do stuff on m_Metric
    m_Metric->SetNumberOfHistogramBins( 50 );
    m_Metric->SetNumberOfSpatialSamples( 5000 );
    }

protected:
  MetricType* m_Metric;

};

template <typename FixedImageType, typename MovingImageType>
class MIMetricInitializer
{
public:
  typedef itk::MutualInformationImageToImageMetric< FixedImageType,
                                                    MovingImageType> MetricType;


  MIMetricInitializer(MetricType* metric)
    {
    m_Metric = metric;
    }

  void Initialize()
    {
    // Do stuff on m_Metric
    m_Metric->SetNumberOfSpatialSamples( 400 );
    }

protected:
  MetricType* m_Metric;

};

template < typename InterpolatorType,
            typename TransformType,
            typename FixedImageReaderType,
            typename MovingImageReaderType >
void BasicTest( FixedImageReaderType* fixedImageReader,
                MovingImageReaderType* movingImageReader,
                InterpolatorType* interpolator,
                TransformType* transform
                )
{
  typedef typename FixedImageReaderType::OutputImageType    FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType   MovingImageType;

  fixedImageReader->Update();
  movingImageReader->Update();

  typename FixedImageType::Pointer fixed = fixedImageReader->GetOutput();
  typename MovingImageType::Pointer moving = movingImageReader->GetOutput();

  // Mean squares
  typedef itk::MeanSquaresImageToImageMetric< FixedImageType, MovingImageType > MetricType;
  typename MetricType::Pointer msMetric = MetricType::New();
  MeanSquaresMetricInitializer< FixedImageType, MovingImageType > msMetricInitializer( msMetric );

  TestAMetric( fixedImageReader, movingImageReader, interpolator, transform, msMetric.GetPointer(), msMetricInitializer );

  // Mattes MI
  typedef itk::MattesMutualInformationImageToImageMetric< FixedImageType, MovingImageType > MattesMetricType;
  typename MattesMetricType::Pointer mattesMetric = MattesMetricType::New();
  MattesMIMetricInitializer< FixedImageType, MovingImageType > mattesMetricInitializer( mattesMetric );

  TestAMetric( fixedImageReader, movingImageReader, interpolator, transform, mattesMetric.GetPointer(), mattesMetricInitializer );
}

template <typename FixedImageReaderType,
          typename MovingImageReaderType,
          typename InterpolatorType,
          typename TransformType,
          typename MetricType,
          typename MetricInitializerType>
void TestAMetric(FixedImageReaderType* fixedImageReader,
                 MovingImageReaderType* movingImageReader,
                 InterpolatorType* interpolator,
                 TransformType* transform,
                 MetricType* metric,
                 MetricInitializerType metricInitializer)
{
  typedef typename FixedImageReaderType::OutputImageType    FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType   MovingImageType;

  metric->SetFixedImageRegion( fixedImageReader->GetOutput()->GetBufferedRegion() );

  OptImageToImageMetricsTest<  FixedImageType,
                          MovingImageType,
                          InterpolatorType,
                          TransformType,
                          MetricType,
                          MetricInitializerType > testMetric;

  testMetric.RunTest( fixedImageReader->GetOutput(), movingImageReader->GetOutput(), interpolator, transform, metric, metricInitializer );
}

template <typename FixedImageReaderType, typename MovingImageReaderType>
void AffineLinearTest( FixedImageReaderType* fixedImageReader,
                       MovingImageReaderType* movingImageReader)
{
  typedef typename MovingImageReaderType::OutputImageType MovingImageType;
  typedef itk::LinearInterpolateImageFunction< MovingImageType, double > InterpolatorType;
  typedef itk::AffineTransform<double, 2> TransformType;

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  TransformType::Pointer transform = TransformType::New();

  BasicTest(fixedImageReader,
            movingImageReader,
            interpolator.GetPointer(),
            transform.GetPointer());

}

template <typename FixedImageReaderType, typename MovingImageReaderType>
void RigidLinearTest( FixedImageReaderType* fixedImageReader,
                       MovingImageReaderType* movingImageReader)
{
  typedef typename MovingImageReaderType::OutputImageType MovingImageType;

  typedef itk::LinearInterpolateImageFunction< MovingImageType, double > InterpolatorType;
  typedef itk::Rigid2DTransform<double> TransformType;

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  TransformType::Pointer transform = TransformType::New();

  BasicTest(fixedImageReader,
            movingImageReader,
            interpolator.GetPointer(),
            transform.GetPointer());
}

template <typename FixedImageReaderType, typename MovingImageReaderType>
void TranslationLinearTest( FixedImageReaderType* fixedImageReader,
                            MovingImageReaderType* movingImageReader)
{
  typedef typename MovingImageReaderType::OutputImageType MovingImageType;

  typedef itk::LinearInterpolateImageFunction< MovingImageType, double > InterpolatorType;
  typedef itk::TranslationTransform<double, 2> TransformType;

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  TransformType::Pointer transform = TransformType::New();

  BasicTest(fixedImageReader,
            movingImageReader,
            interpolator.GetPointer(),
            transform.GetPointer());
}


template <typename FixedImageReaderType, typename MovingImageReaderType>
void DoDebugTest( FixedImageReaderType* fixedImageReader,
                  MovingImageReaderType* movingImageReader)
{
  typedef typename MovingImageReaderType::OutputImageType MovingImageType;

  typedef itk::LinearInterpolateImageFunction< MovingImageType, double > InterpolatorType;
  typedef itk::Rigid2DTransform<double> TransformType;

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  TransformType::Pointer transform = TransformType::New();

  typedef typename FixedImageReaderType::OutputImageType      FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType     MovingImageType;

  fixedImageReader->Update();
  movingImageReader->Update();

  typename FixedImageType::Pointer fixed = fixedImageReader->GetOutput();
  typename MovingImageType::Pointer moving = movingImageReader->GetOutput();

  // Mean squares
  typedef itk::MeanSquaresImageToImageMetric< FixedImageType, MovingImageType > MetricType;
  typename MetricType::Pointer metric = MetricType::New();
  MeanSquaresMetricInitializer< FixedImageType, MovingImageType > metricInitializer( metric );

  metric->SetFixedImageRegion( fixedImageReader->GetOutput()->GetBufferedRegion() );

  typedef typename MetricType::ParametersType ParametersType;

  std::cout << "-------------------------------------------------------------------" << std::endl;
  std::cout << "Testing" << std::endl;
  std::cout << "\tMetric       : " << metric->GetNameOfClass() << std::endl;
  std::cout << "\tInterpolator : " << interpolator->GetNameOfClass() << std::endl;
  std::cout << "\tTransform    : " << transform->GetNameOfClass() << std::endl;
  std::cout << "-------------------------------------------------------------------" << std::endl;
  std::cout << std::endl;

  // connect the interpolator
  metric->SetInterpolator( interpolator );

  // connect the transform
  metric->SetTransform( transform );

  // connect the images to the metric
  metric->SetFixedImage( fixed );
  metric->SetMovingImage( moving );

  // call custom initialization for the metric
  metricInitializer.Initialize();

  // initialize the metric
  metric->Initialize();

  // Set the transform to identity
  transform->SetIdentity();

  // Get the transform parameters for identity.
  ParametersType parameters = transform->GetParameters();

  typename MetricType::MeasureType value;
  typename MetricType::DerivativeType derivative;

  parameters[0] = 0.1;

  metric->GetValueAndDerivative( parameters,
                                 value,
                                 derivative );

  //metric->StopDebug();

  // Force the test to end here so the debug file
  // ends at the right place.
  exit(EXIT_SUCCESS);
}

} // end namespace itk

#endif
