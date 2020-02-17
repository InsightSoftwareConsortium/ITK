/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkOptImageToImageMetricsTest2_h
#define itkOptImageToImageMetricsTest2_h

#include "itkTimeProbe.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkImageToImageMetric.h"
#include "itkBSplineTransform.h"

namespace itk
{

template <typename FixedImageType,
          typename MovingImageType,
          typename InterpolatorType,
          typename TransformType,
          typename MetricType,
          typename MetricInitializerType>
class OptImageToImageMetricsTest
{
public:
  OptImageToImageMetricsTest() = default;

  void
  RunTest(FixedImageType *      fixed,
          MovingImageType *     moving,
          InterpolatorType *    interpolator,
          TransformType *       transform,
          MetricType *          metric,
          MetricInitializerType metricInitializer)
  {
    std::cout << "-------------------------------------------------------------------" << std::endl;
    std::cout << "Testing" << std::endl;
    std::cout << "\tMetric       : " << metric->GetNameOfClass() << std::endl;
    std::cout << "\tInterpolator : " << interpolator->GetNameOfClass() << std::endl;
    std::cout << "\tTransform    : " << transform->GetNameOfClass() << std::endl;
    std::cout << "-------------------------------------------------------------------" << std::endl;
    std::cout << std::endl;

    // int result = EXIT_SUCCESS;

    // connect the interpolator
    metric->SetInterpolator(interpolator);

    // connect the transform
    metric->SetTransform(transform);

    // connect the images to the metric
    metric->SetFixedImage(fixed);
    metric->SetMovingImage(moving);

    metric->GetThreader()->SetGlobalDefaultNumberOfThreads(4);
    metric->GetThreader()->SetGlobalMaximumNumberOfThreads(4);

    // call custom initialization for the metric
    metricInitializer.Initialize();

    // Always use the same seed value.
    // All instances are the same since MersenneTwisterRandomVariateGenerator
    // uses a singleton pattern.
    itk::Statistics::MersenneTwisterRandomVariateGenerator::GetInstance()->SetSeed(42);

    // initialize the metric
    // Samples are drawn here in metric->Initialize(),
    // so we seed the random number generator
    // immediately before this call.
    metric->Initialize();

    // Verify that Initialize has properly called
    // MultiThreadingInitialize() and a series of CreateAnother();

    using TransformPointer = typename MetricType::TransformPointer;
    const TransformPointer * transformPtr = metric->GetThreaderTransform();
    if ((transformPtr == static_cast<const TransformPointer *>(nullptr)) || (transformPtr[0].IsNull()))
    {
      exit(EXIT_FAILURE);
    }
  }
  // Other registration functionality tested in
  // OptImageToImageTest.cxx... skip the rest
};

template <typename FixedImageType, typename MovingImageType>
class MeanSquaresMetricInitializer
{
public:
  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;


  MeanSquaresMetricInitializer(MetricType * metric) { m_Metric = metric; }

  void
  Initialize()
  {
    // Do stuff on m_Metric
    m_Metric->UseAllPixelsOn();
  }

protected:
  MetricType * m_Metric;
};


template <typename InterpolatorType,
          typename TransformType,
          typename FixedImageReaderType,
          typename MovingImageReaderType>
void
BasicTest(FixedImageReaderType *  fixedImageReader,
          MovingImageReaderType * movingImageReader,
          InterpolatorType *      interpolator,
          TransformType *         transform)
{
  using FixedImageType = typename FixedImageReaderType::OutputImageType;
  using MovingImageType = typename MovingImageReaderType::OutputImageType;

  fixedImageReader->Update();
  movingImageReader->Update();

  typename FixedImageType::Pointer  fixed = fixedImageReader->GetOutput();
  typename MovingImageType::Pointer moving = movingImageReader->GetOutput();

  // Mean squares
  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;
  typename MetricType::Pointer                                  msMetric = MetricType::New();
  MeanSquaresMetricInitializer<FixedImageType, MovingImageType> msMetricInitializer(msMetric);

  TestAMetric(fixedImageReader, movingImageReader, interpolator, transform, msMetric.GetPointer(), msMetricInitializer);
}

template <typename FixedImageReaderType,
          typename MovingImageReaderType,
          typename InterpolatorType,
          typename TransformType,
          typename MetricType,
          typename MetricInitializerType>
void
TestAMetric(FixedImageReaderType *  fixedImageReader,
            MovingImageReaderType * movingImageReader,
            InterpolatorType *      interpolator,
            TransformType *         transform,
            MetricType *            metric,
            MetricInitializerType   metricInitializer)
{
  using FixedImageType = typename FixedImageReaderType::OutputImageType;
  using MovingImageType = typename MovingImageReaderType::OutputImageType;

  metric->SetFixedImageRegion(fixedImageReader->GetOutput()->GetBufferedRegion());

  OptImageToImageMetricsTest<FixedImageType,
                             MovingImageType,
                             InterpolatorType,
                             TransformType,
                             MetricType,
                             MetricInitializerType>
    testMetric;

  testMetric.RunTest(
    fixedImageReader->GetOutput(), movingImageReader->GetOutput(), interpolator, transform, metric, metricInitializer);
}

template <typename FixedImageReaderType, typename MovingImageReaderType>
void
BSplineLinearTest(FixedImageReaderType * fixedImageReader, MovingImageReaderType * movingImageReader)
{
  using MovingImageType = typename MovingImageReaderType::OutputImageType;
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  using FixedImageType = typename FixedImageReaderType::OutputImageType;
  using MovingImageType = typename MovingImageReaderType::OutputImageType;

  fixedImageReader->Update();
  movingImageReader->Update();

  typename FixedImageType::Pointer       fixedImage = fixedImageReader->GetOutput();
  typename MovingImageType::Pointer      movingImage = movingImageReader->GetOutput();
  typename FixedImageType::SpacingType   fixedSpacing = fixedImage->GetSpacing();
  typename FixedImageType::PointType     fixedOrigin = fixedImage->GetOrigin();
  typename FixedImageType::DirectionType fixedDirection = fixedImage->GetDirection();
  typename FixedImageType::RegionType    fixedRegion = fixedImage->GetBufferedRegion();
  typename FixedImageType::SizeType      fixedSize = fixedRegion.GetSize();

  constexpr unsigned int SpaceDimension = 2;
  constexpr unsigned int VSplineOrder = 3;
  using CoordinateRepType = double;

  using TransformType = itk::BSplineTransform<CoordinateRepType, SpaceDimension, VSplineOrder>;

  typename TransformType::Pointer bsplineTransform = TransformType::New();

  typename TransformType::MeshSizeType           meshSize;
  typename TransformType::PhysicalDimensionsType physicalDimensions;
  for (unsigned int d = 0; d < SpaceDimension; d++)
  {
    physicalDimensions[d] = fixedSpacing[d] * static_cast<CoordinateRepType>(fixedSize[d] - 1);
    meshSize[d] = 4;
  }
  bsplineTransform->SetTransformDomainOrigin(fixedOrigin);
  bsplineTransform->SetTransformDomainDirection(fixedDirection);
  bsplineTransform->SetTransformDomainPhysicalDimensions(physicalDimensions);
  bsplineTransform->SetTransformDomainMeshSize(meshSize);

  using ParametersType = typename TransformType::ParametersType;

  const unsigned int numberOfParameters = bsplineTransform->GetNumberOfParameters();

  ParametersType parameters(numberOfParameters);

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();

  BasicTest(fixedImageReader, movingImageReader, interpolator.GetPointer(), bsplineTransform.GetPointer());
}

} // end namespace itk

#endif
