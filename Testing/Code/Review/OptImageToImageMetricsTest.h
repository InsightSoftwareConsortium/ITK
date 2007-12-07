
#ifndef OptImageToImageMetricsTest_h
#define OptImageToImageMetricsTest_h

#include "itkTimeProbe.h"


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

  std::cout << "===================================================================" << std::endl;
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

  // initialize the metric
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
  for (int parameterIdx = 0; parameterIdx < parameters.GetSize(); parameterIdx++)
    {
    std::cout << "Param[" << parameterIdx << "]\tValue\tDerivative " << std::endl; //[" << parameterIdx << "]" << std::endl;
    double startVal = parameters[parameterIdx];
    // endVal is 10% beyond startVal.
    double endVal = 1.10 * startVal;
    // If startVal is 0, endVal needs to be fixed up.
    if (fabs(endVal - 0.0) < 1e-8)
      {
      endVal = startVal + 1.0;
      }
    double incr = (endVal - startVal) / 10.0;

    for (double pval = startVal; pval <= endVal; pval += incr)
      {
      parameters[parameterIdx] = pval;

      timeProbe.Start();
      metric->GetValueAndDerivative( parameters,
                                     value, 
                                     derivative );
      timeProbe.Stop();

      std::cout << pval << "\t" << value << "\t" << derivative << std::endl;
      }
    }

  std::cout << std::endl;
  std::cout << "Mean time for GetValueAndDerivative : " << timeProbe.GetMeanTime() << std::endl;
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
#ifdef ITK_USE_OPTIMIZED_REGISTRATION_METHODS
    m_Metric->UseAllPixelsOn();
#endif 
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

template < class InterpolatorType, 
            class TransformType,
            class FixedImageReaderType,
            class MovingImageReaderType >
void BasicTest( FixedImageReaderType* fixedImageReader, 
                MovingImageReaderType* movingImageReader,
                InterpolatorType* interpolator,
                TransformType* transform
                )
{
  typedef typename FixedImageReaderType::OutputImageType FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType MovingImageType;

  fixedImageReader->Update();
  movingImageReader->Update();

  typename FixedImageType::Pointer fixed = fixedImageReader->GetOutput();
  typename MovingImageType::Pointer moving = movingImageReader->GetOutput();

  // Mean squares 
  typedef itk::MeanSquaresImageToImageMetric< FixedImageType, MovingImageType > MetricType;
  typedef MeanSquaresMetricInitializer< FixedImageType, MovingImageType > MetricInitializerType;
  typename MetricType::Pointer msMetric = MetricType::New();
  MeanSquaresMetricInitializer< FixedImageType, MovingImageType > msMetricInitializer( msMetric );

  TestAMetric( fixedImageReader, movingImageReader, interpolator, transform, msMetric.GetPointer(), msMetricInitializer );
 
  // Mattes MI
  typedef itk::MattesMutualInformationImageToImageMetric< FixedImageType, MovingImageType > MattesMetricType;
  typedef MattesMIMetricInitializer< FixedImageType, MovingImageType > MattesMetricInitializerType;
  typename MattesMetricType::Pointer mattesMetric = MattesMetricType::New();
  MattesMIMetricInitializer< FixedImageType, MovingImageType > mattesMetricInitializer( mattesMetric );

  TestAMetric( fixedImageReader, movingImageReader, interpolator, transform, mattesMetric.GetPointer(), mattesMetricInitializer );

  // MI
  typedef itk::MutualInformationImageToImageMetric< FixedImageType, MovingImageType > MIMetricType;
  typedef MIMetricInitializer< FixedImageType, MovingImageType > MIMetricInitializerType;
  typename MIMetricType::Pointer miMetric = MIMetricType::New();
  MIMetricInitializer< FixedImageType, MovingImageType> miMetricInitializer( miMetric );
  
  TestAMetric( fixedImageReader, movingImageReader, interpolator, transform, miMetric.GetPointer(), miMetricInitializer );
}

template <class FixedImageReaderType,
          class MovingImageReaderType,
          class InterpolatorType,
          class TransformType,
          class MetricType,
          class MetricInitializerType>
void TestAMetric(FixedImageReaderType* fixedImageReader,
                 MovingImageReaderType* movingImageReader,
                 InterpolatorType* interpolator,
                 TransformType* transform,
                 MetricType* metric,
                 MetricInitializerType metricInitializer)
{
  typedef typename FixedImageReaderType::OutputImageType FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType MovingImageType;

  metric->SetFixedImageRegion( fixedImageReader->GetOutput()->GetBufferedRegion() );

  OptImageToImageMetricsTest<  FixedImageType,
                          MovingImageType,
                          InterpolatorType,
                          TransformType,
                          MetricType,
                          MetricInitializerType > testMIMetric;

  int test3 = testMIMetric.RunTest( fixedImageReader->GetOutput(), movingImageReader->GetOutput(), interpolator, transform, metric, metricInitializer );
}

template <class FixedImageReaderType, class MovingImageReaderType>
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

template <class FixedImageReaderType, class MovingImageReaderType>
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

template <class FixedImageReaderType, class MovingImageReaderType>
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


#endif 

