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

#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.h"
#include "itkSphereSignedDistanceFunction.h"
#include "itkAmoebaOptimizer.h"

#include "itkCastImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"

#include "itkFastMarchingImageFilter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"
#include "itkTestingMacros.h"

/** \class ShowIterationObject
 * This class is used to support callbacks
 * on the segmentation filter in this test.
 */
namespace
{
template <typename TFilter>
class ShowIterationObject
{
public:
  ShowIterationObject(TFilter * filter) { m_Filter = filter; }
  void
  ShowIteration()
  {
    std::cout << m_Filter->GetElapsedIterations() << ": ";
    std::cout << m_Filter->GetCurrentParameters() << " ";
    std::cout << m_Filter->GetRMSChange() << std::endl;
  }

  typename TFilter::Pointer m_Filter;
};
} // namespace

int
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest(int, char *[])
{
  // Typedefs of components
  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;
  using InternalPixelType = float;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using InternalImageType = itk::Image<InternalPixelType, ImageDimension>;

  using FilterType = itk::GeodesicActiveContourShapePriorLevelSetImageFilter<InternalImageType, InternalImageType>;

  using ShapeFunctionType = itk::SphereSignedDistanceFunction<double, ImageDimension>;

  using CostFunctionType = itk::ShapePriorMAPCostFunction<InternalImageType, InternalPixelType>;

  using OptimizerType = itk::AmoebaOptimizer;
  using ParametersType = FilterType::ParametersType;

  FilterType::Pointer        filter = FilterType::New();
  ShapeFunctionType::Pointer shape = ShapeFunctionType::New();
  CostFunctionType::Pointer  costFunction = CostFunctionType::New();
  OptimizerType::Pointer     optimizer = OptimizerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    filter, GeodesicActiveContourShapePriorLevelSetImageFilter, ShapePriorSegmentationLevelSetImageFilter);

  ImageType::SizeType imageSize;
  imageSize[0] = 128;
  imageSize[1] = 128;

  ImageType::RegionType imageRegion;
  imageRegion.SetSize(imageSize);

  //
  // Create an input image.
  // A light circle with a rectangle through it on a dark background.
  // The circle is centered at {50,57} with radius 30.
  // The rectangle starts at {10,50} with size {80,10}.
  //
  // The true shape is just the circle.
  //
  PixelType background = 0;
  PixelType foreground = 190;

  // Fill in the background
  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetRegions(imageRegion);
  inputImage->Allocate();
  inputImage->FillBuffer(background);

  ImageType::Pointer trueShape = ImageType::New();
  trueShape->SetRegions(imageRegion);
  trueShape->Allocate();
  trueShape->FillBuffer(background);

  // Draw in the rectangle
  ImageType::IndexType rectStart;
  rectStart[0] = 10;
  rectStart[1] = 50;
  ImageType::SizeType rectSize;
  rectSize[0] = 80;
  rectSize[1] = 10;
  ImageType::RegionType rectRegion;
  rectRegion.SetIndex(rectStart);
  rectRegion.SetSize(rectSize);

  using Iterator = itk::ImageRegionIterator<ImageType>;
  Iterator it(inputImage, rectRegion);
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    it.Set(foreground);
    ++it;
  }

  // Draw in the circle
  shape->Initialize();
  ParametersType trueParameters(shape->GetNumberOfParameters());
  trueParameters[0] = 30.0;
  trueParameters[1] = 50.0;
  trueParameters[2] = 57.0;
  shape->SetParameters(trueParameters);

  it = Iterator(inputImage, imageRegion);
  it.GoToBegin();

  Iterator it2(trueShape, imageRegion);
  it2.GoToBegin();

  while (!it.IsAtEnd())
  {
    ImageType::IndexType         index = it.GetIndex();
    ShapeFunctionType::PointType point;
    inputImage->TransformIndexToPhysicalPoint(index, point);
    if (shape->Evaluate(point) <= 0.0)
    {
      it.Set(foreground);
      it2.Set(foreground);
    }
    ++it;
    ++it2;
  }

  //
  // Create an edge potential map.
  // First compute the image gradient magnitude using a derivative of gaussian filter.
  // Then apply a sigmoid function to the gradient magnitude.
  //
  using CastFilterType = itk::CastImageFilter<ImageType, InternalImageType>;
  CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput(inputImage);

  using GradientImageType = itk::GradientMagnitudeRecursiveGaussianImageFilter<InternalImageType, InternalImageType>;

  GradientImageType::Pointer gradMagnitude = GradientImageType::New();
  gradMagnitude->SetInput(caster->GetOutput());
  gradMagnitude->SetSigma(1.0);

  using SigmoidFilterType = itk::SigmoidImageFilter<InternalImageType, InternalImageType>;
  SigmoidFilterType::Pointer sigmoid = SigmoidFilterType::New();
  sigmoid->SetOutputMinimum(0.0);
  sigmoid->SetOutputMaximum(1.0);
  sigmoid->SetAlpha(-0.4);
  sigmoid->SetBeta(2.5);
  sigmoid->SetInput(gradMagnitude->GetOutput());

  //
  // Create an initial level.
  // Use fast marching to create an signed distance from a seed point.
  //
  using FastMarchingFilterType = itk::FastMarchingImageFilter<InternalImageType>;
  FastMarchingFilterType::Pointer fastMarching = FastMarchingFilterType::New();

  using NodeContainer = FastMarchingFilterType::NodeContainer;
  using NodeType = FastMarchingFilterType::NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();

  // Choose an initial contour that is within the shape to be segmented
  // The initial contour is a circle centered at {47,47} with radius 10.0
  InternalImageType::IndexType seedPosition;
  seedPosition[0] = 47;
  seedPosition[1] = 47;

  NodeType node;
  node.SetValue(-10.0);
  node.SetIndex(seedPosition);

  seeds->Initialize();
  seeds->InsertElement(0, node);

  fastMarching->SetTrialPoints(seeds);
  fastMarching->SetSpeedConstant(1.0);
  fastMarching->SetOutputSize(imageSize);

  //
  // Set up the components of the shape prior segmentation filter.
  //

  // Set up the shape function
  shape->Initialize();

  // Set up the cost function
  CostFunctionType::ArrayType mean(shape->GetNumberOfShapeParameters());
  CostFunctionType::ArrayType stddev(shape->GetNumberOfShapeParameters());

  // Assume the sphere radius has a mean value of 25 and std dev of 3
  mean[0] = 25.0;
  stddev[0] = 3.0;

  costFunction->SetShapeParameterMeans(mean);
  costFunction->SetShapeParameterStandardDeviations(stddev);

  CostFunctionType::WeightsType weights;
  weights.Fill(1.0);
  weights[1] = 10.0;
  costFunction->SetWeights(weights);

  // Set up the optimizer
  optimizer->SetFunctionConvergenceTolerance(0.1);
  optimizer->SetParametersConvergenceTolerance(0.5);
  optimizer->SetMaximumNumberOfIterations(50);

  // Set up the initial parameters
  ParametersType parameters(shape->GetNumberOfParameters());

  parameters[0] = mean[0]; // mean radius
  parameters[1] = 64;      // center of the image
  parameters[2] = 64;      // center of the image

  // Set up the scaling between the level set terms
  filter->SetPropagationScaling(0.5);
  filter->SetAdvectionScaling(1.00);
  filter->SetCurvatureScaling(1.00);
  filter->SetShapePriorScaling(0.1);

  // Hook up components to the filter
  filter->SetInput(fastMarching->GetOutput());   // initial level set
  filter->SetFeatureImage(sigmoid->GetOutput()); // edge potential map
  filter->SetShapeFunction(shape);
  filter->SetCostFunction(costFunction);
  filter->SetOptimizer(optimizer);
  filter->SetInitialParameters(parameters);

  filter->SetNumberOfLayers(4);
  filter->SetMaximumRMSError(0.01);
  filter->SetNumberOfIterations(400);

  //
  // Connect an observer to the filter
  //
  using WatcherType = ShowIterationObject<FilterType>;
  WatcherType                                    iterationWatcher(filter);
  itk::SimpleMemberCommand<WatcherType>::Pointer command = itk::SimpleMemberCommand<WatcherType>::New();
  command->SetCallbackFunction(&iterationWatcher, &WatcherType::ShowIteration);
  filter->AddObserver(itk::IterationEvent(), command);

  //
  // Threshold the output level set to display the final contour.
  //
  using ThresholdFilterType = itk::BinaryThresholdImageFilter<InternalImageType, ImageType>;
  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  thresholder->SetInput(filter->GetOutput());
  thresholder->SetLowerThreshold(-1e+10);
  thresholder->SetUpperThreshold(0.0);
  thresholder->SetOutsideValue(0);
  thresholder->SetInsideValue(255);

  //
  // Compute overlap between the true shape and the segmented shape.
  //
  using OverlapCalculatorType = itk::SimilarityIndexImageFilter<ImageType, ImageType>;
  OverlapCalculatorType::Pointer overlap = OverlapCalculatorType::New();

  overlap->SetInput1(trueShape);
  overlap->SetInput2(thresholder->GetOutput());

  // Run the pipeline
  try
  {
    overlap->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    std::cout << "Caught unexpected exception." << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
  }

  // Print useful information from the shape detection filter
  std::cout << "Max. no. iterations: " << filter->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << filter->GetMaximumRMSError() << std::endl;
  std::cout << "No. elpased iterations: " << filter->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << filter->GetRMSChange() << std::endl;
  std::cout << "Overlap: " << overlap->GetSimilarityIndex() << std::endl;


  // Uncomment to write out images
  /*
    using WriterType = itk::ImageFileWriter< ImageType >;
    WriterType::Pointer writer = WriterType::New();

    using RescaleFilterType = itk::RescaleIntensityImageFilter< InternalImageType,
      ImageType >;
    RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

    writer->SetFileName( "inputImage.png" );
    writer->SetInput( inputImage );
    writer->Update();

    rescaler->SetInput( gradMagnitude->GetOutput() );
    rescaler->SetOutputMinimum( 0 );
    rescaler->SetOutputMaximum( 255 );
    writer->SetFileName( "gradMagnitude.png" );
    writer->SetInput( rescaler->GetOutput() );
    writer->Update();

    rescaler->SetInput( sigmoid->GetOutput() );
    writer->SetFileName( "edgePotential.png" );
    writer->Update();

    writer->SetInput( thresholder->GetOutput() );
    writer->SetFileName( "outputLevelSet.png" );
    writer->Update();

    thresholder->SetInput( fastMarching->GetOutput() );
    writer->SetInput( thresholder->GetOutput() );
    writer->SetFileName( "initialLevelSet.png" );
    writer->Update();
  */

  // Check if overlap is above threshold
  if (overlap->GetSimilarityIndex() > 0.93)
  {
    std::cout << "Overlap exceed threshold." << std::endl;
  }
  else
  {
    std::cout << "Overlap below threshold." << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise other methods for coverage
  filter->Print(std::cout);
  filter->GetSegmentationFunction()->Print(std::cout);

  using GenericFilterType = FilterType::Superclass;
  std::cout << filter->GenericFilterType::GetNameOfClass() << std::endl;

  std::cout << "ShapeFunction: ";
  std::cout << filter->GetShapeFunction() << std::endl;
  std::cout << "CostFunction: ";
  std::cout << filter->GetCostFunction() << std::endl;
  std::cout << "Optimizer: ";
  std::cout << filter->GetOptimizer() << std::endl;
  std::cout << "InitialParameters: ";
  std::cout << filter->GetInitialParameters() << std::endl;
  std::cout << "ShapePriorSegmentationFunction: ";
  std::cout << filter->GetShapePriorSegmentationFunction() << std::endl;

  // Repeat Update for zero propagation weight
  filter->SetPropagationScaling(0.0);
  filter->SetShapePriorScaling(1.1);
  filter->SetNumberOfIterations(5);
  filter->Update();

  //
  // Exercise error handling testing.
  //
  bool pass;

#define TEST_INITIALIZATION_ERROR(ComponentName, badComponent, goodComponent)                                          \
  filter->Set##ComponentName(badComponent);                                                                            \
  try                                                                                                                  \
  {                                                                                                                    \
    pass = false;                                                                                                      \
    filter->Update();                                                                                                  \
  }                                                                                                                    \
  catch (const itk::ExceptionObject & err)                                                                             \
  {                                                                                                                    \
    std::cout << "Caught expected ExceptionObject" << std::endl;                                                       \
    std::cout << err << std::endl;                                                                                     \
    pass = true;                                                                                                       \
    filter->ResetPipeline();                                                                                           \
  }                                                                                                                    \
  filter->Set##ComponentName(goodComponent);                                                                           \
                                                                                                                       \
  if (!pass)                                                                                                           \
  {                                                                                                                    \
    std::cout << "Test failed." << std::endl;                                                                          \
    return EXIT_FAILURE;                                                                                               \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT

  TEST_INITIALIZATION_ERROR(ShapeFunction, nullptr, shape);
  TEST_INITIALIZATION_ERROR(CostFunction, nullptr, costFunction);
  TEST_INITIALIZATION_ERROR(Optimizer, nullptr, optimizer);

  CostFunctionType::ArrayType badParameters(shape->GetNumberOfShapeParameters() - 1);
  badParameters.Fill(2.0);

  TEST_INITIALIZATION_ERROR(InitialParameters, badParameters, parameters);

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
