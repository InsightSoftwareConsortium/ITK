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
#ifndef __itkOptImageToImageMetricsTest2_h
#define __itkOptImageToImageMetricsTest2_h

#include "itkTimeProbe.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkImageToImageMetric.h"
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

  void RunTest( FixedImageType* fixed,
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

    //int result = EXIT_SUCCESS;

    // connect the interpolator
    metric->SetInterpolator( interpolator );

    // connect the transform
    metric->SetTransform( transform );

    // connect the images to the metric
    metric->SetFixedImage( fixed );
    metric->SetMovingImage( moving );

    metric->GetThreader()->SetGlobalDefaultNumberOfThreads(4);
    metric->GetThreader()->SetGlobalMaximumNumberOfThreads(4);

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

    //Verify that Initialize has properly called
    //MultiThreadingInitialize() and a series of CreateAnother();

    typedef typename MetricType::TransformPointer TransformPointer;
    const TransformPointer *transformPtr= metric->GetThreaderTransform();
    if ((transformPtr==static_cast<const TransformPointer *>(NULL))||
        (transformPtr[0].IsNull()))
      {
      exit(EXIT_FAILURE);
      }

    const TransformType *firstBSpline=
       static_cast<TransformType*>(transformPtr[0].GetPointer());
    typedef typename TransformType::BulkTransformPointer BulkTransformPointer;
    BulkTransformPointer firstBulkTransform = firstBSpline->GetBulkTransform();

    // The bulk transform may be IdentityTransform, which has no parameters
    if (firstBulkTransform->GetNumberOfParameters() == 0)
      {
      return;
      }

    ParametersType firstBulkParameters =  firstBulkTransform->GetParameters();
    double firstBulkEntry = firstBulkParameters[0];

    for ( ThreadIdType i=0; i<metric->GetThreader()->GetNumberOfThreads()-1; i++)
      {
        //Verify that BSpline transform pointer is being copied
        if (transformPtr[i].IsNull())
          {
          exit(EXIT_FAILURE);
          }

        //Verify that bulk transform matrix in BSpline pointer is being copied
        const TransformType *loopBSpline=
           static_cast<TransformType*>(transformPtr[i].GetPointer());
        BulkTransformPointer loopBulkTransform = loopBSpline->GetBulkTransform();

        // The bulk transform may be IdentityTransform, which has no parameters
        if (loopBulkTransform->GetNumberOfParameters() == 0)
          {
          return;
          }
        ParametersType loopBulkParameters =  loopBulkTransform->GetParameters();
        double loopBulkEntry = loopBulkParameters[0];
        double entryComparisonTolerance = 0.001;

        if (fabs(loopBulkEntry - firstBulkEntry)>entryComparisonTolerance)
          {
          exit(EXIT_FAILURE);
          }
      }
    }
    //Other registration functionality tested in
    //OptImageToImageTest.cxx... skip the rest

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
  typedef typename FixedImageReaderType::OutputImageType    FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType   MovingImageType;

  fixedImageReader->Update();
  movingImageReader->Update();

  typename FixedImageType::Pointer fixed = fixedImageReader->GetOutput();
  typename MovingImageType::Pointer moving = movingImageReader->GetOutput();

  // Mean squares
  typedef itk::MeanSquaresImageToImageMetric< FixedImageType, MovingImageType > MetricType;
  typedef MeanSquaresMetricInitializer< FixedImageType, MovingImageType > MetricInitializerType;
  typename MetricType::Pointer msMetric = MetricType::New();
  MeanSquaresMetricInitializer< FixedImageType, MovingImageType > msMetricInitializer( msMetric );

  TestAMetric( fixedImageReader, movingImageReader, interpolator,
                             transform, msMetric.GetPointer(), msMetricInitializer );
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
  typedef typename FixedImageReaderType::OutputImageType    FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType   MovingImageType;

  metric->SetFixedImageRegion( fixedImageReader->GetOutput()->GetBufferedRegion() );

  OptImageToImageMetricsTest<  FixedImageType,
                          MovingImageType,
                          InterpolatorType,
                          TransformType,
                          MetricType,
                          MetricInitializerType > testMetric;

  testMetric.RunTest( fixedImageReader->GetOutput(), movingImageReader->GetOutput(),
                      interpolator, transform, metric, metricInitializer );
}

template <class FixedImageReaderType, class MovingImageReaderType>
void BSplineLinearTest( FixedImageReaderType* fixedImageReader,
                       MovingImageReaderType* movingImageReader)
{
  typedef typename MovingImageReaderType::OutputImageType MovingImageType;
  typedef itk::LinearInterpolateImageFunction< MovingImageType, double > InterpolatorType;

  typedef typename FixedImageReaderType::OutputImageType    FixedImageType;
  typedef typename MovingImageReaderType::OutputImageType   MovingImageType;

  fixedImageReader->Update();
  movingImageReader->Update();

  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();
  typename FixedImageType::SpacingType   fixedSpacing    = fixedImage->GetSpacing();
  typename FixedImageType::PointType     fixedOrigin     = fixedImage->GetOrigin();
  typename FixedImageType::DirectionType fixedDirection  = fixedImage->GetDirection();
  typename FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  typename FixedImageType::SizeType   fixedSize =  fixedRegion.GetSize();

  const unsigned int SpaceDimension = 2;
  const unsigned int VSplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineDeformableTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            VSplineOrder >     TransformType;

  typename TransformType::Pointer bsplineTransform = TransformType::New();

  typedef typename TransformType::RegionType RegionType;
  RegionType bsplineRegion;
  typename RegionType::SizeType   size;

  const unsigned int numberOfGridNodesOutsideTheImageSupport = VSplineOrder;

  const unsigned int numberOfGridNodesInsideTheImageSupport = 5;

  const unsigned int numberOfGridNodes =
                        numberOfGridNodesInsideTheImageSupport +
                        numberOfGridNodesOutsideTheImageSupport;

  const unsigned int numberOfGridCells =
                        numberOfGridNodesInsideTheImageSupport - 1;

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  size.Fill( numberOfGridNodes );
  bsplineRegion.SetSize( size );
  typedef typename TransformType::SpacingType SpacingType;
  SpacingType spacing;

  typedef typename TransformType::OriginType OriginType;
  OriginType origin;

  spacing[0] = fixedSpacing[0] * fixedSize[0]  / numberOfGridCells;
  spacing[1] = fixedSpacing[1] * fixedSize[1]  / numberOfGridCells;

  const unsigned int orderShift = VSplineOrder / 2;

  origin[0] = fixedOrigin[0] - orderShift * spacing[0] - fixedSpacing[0] / 2.0;
  origin[1] = fixedOrigin[1] - orderShift * spacing[1] - fixedSpacing[1] / 2.0;

  bsplineTransform->SetGridSpacing( spacing );
  bsplineTransform->SetGridOrigin( origin );
  bsplineTransform->SetGridRegion( bsplineRegion );
  bsplineTransform->SetGridDirection( fixedImage->GetDirection() );

  typedef typename TransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters = bsplineTransform->GetNumberOfParameters();


  ParametersType parameters( numberOfParameters );

  BasicTest(fixedImageReader, movingImageReader, interpolator.GetPointer(),
            bsplineTransform.GetPointer());

}

} // end namespace itk

#endif
