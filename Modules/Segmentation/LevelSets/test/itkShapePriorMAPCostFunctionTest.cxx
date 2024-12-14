/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkShapePriorMAPCostFunction.h"
#include "itkSphereSignedDistanceFunction.h"
#include "itkAmoebaOptimizer.h"

#include "itkImageRegionIteratorWithIndex.h"

/**
 * This module tests the ShapePriorMAPCostFunction class.
 *
 * The active region nodes are generated from the shape signed distance
 * function with parameters perturbed from the mean.
 *
 * Starting from the mean parameters, the cost function is optimized
 * using the Amoeba optimizers. The output parameters are compared with
 * the parameters used to generate the active region.
 *
 * The test fails if the output parameters are not within a tolerance
 * of the original parameters.
 *
 */
int
itkShapePriorMAPCostFunctionTest(int, char *[])
{

  using PixelType = float;
  constexpr unsigned int Dimension = 2;
  using ImageType = itk::Image<PixelType, Dimension>;

  /**
   * Set up the shape signed distance function
   */
  using ShapeFunctionType = itk::SphereSignedDistanceFunction<double, Dimension>;
  auto shape = ShapeFunctionType::New();

  /**
   * Set up a statistical model of the shape parameters.
   */
  using CostFunctionType = itk::ShapePriorMAPCostFunction<ImageType, PixelType>;
  using NodeType = CostFunctionType::NodeType;
  using NodeContainerType = CostFunctionType::NodeContainerType;
  CostFunctionType::ParametersType mean(shape->GetNumberOfParameters());
  CostFunctionType::ParametersType stddev(shape->GetNumberOfParameters());

  mean[0] = 10.0;
  mean[1] = 50.0;
  mean[2] = 50.0;

  stddev[0] = 0.1;
  stddev[1] = 2.5;
  stddev[2] = 2.5;


  /**
   * Set the shape parameters to be perturbation of the mean
   */
  ShapeFunctionType::ParametersType parameters(shape->GetNumberOfParameters());
  parameters[0] = mean[0] - 0.1;
  parameters[1] = mean[1] - 4.0;
  parameters[2] = mean[2] - 6.0;
  shape->SetParameters(parameters);


  /**
   * Create an input level set and active region container
   */
  auto                  size = ImageType::SizeType::Filled(128);
  ImageType::RegionType region;
  region.SetSize(size);

  auto input = ImageType::New();
  input->SetRegions(region);
  input->Allocate();

  auto activeRegion = NodeContainerType::New();


  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;
  Iterator iter(input, region);
  iter.GoToBegin();

  unsigned int    counter = 0;
  const PixelType activeRegionThreshold = 3.0;

  while (!iter.IsAtEnd())
  {
    ImageType::IndexType         index;
    ShapeFunctionType::PointType point;
    index = iter.GetIndex();
    input->TransformIndexToPhysicalPoint(index, point);

    const float value = shape->Evaluate(point);
    iter.Set(value);

    if (itk::Math::abs(value) < activeRegionThreshold)
    {
      NodeType node;
      node.SetIndex(index);
      node.SetValue(value);
      activeRegion->InsertElement(counter++, node);
    }

    ++iter;
  }

  std::cout << "No. nodes: " << activeRegion->Size() << '\n';

  /**
   * Create a dummy edge potential image.
   */
  auto edgeMap = ImageType::New();
  edgeMap->SetRegions(region);
  edgeMap->Allocate();
  edgeMap->FillBuffer(1.0);


  /**
   * Set up the cost function
   */
  auto costFunction = CostFunctionType::New();

  costFunction->SetShapeFunction(shape);
  costFunction->SetActiveRegion(activeRegion);
  costFunction->SetFeatureImage(edgeMap);

  CostFunctionType::ParametersType shapeMean(shape->GetNumberOfShapeParameters());
  CostFunctionType::ParametersType shapeStdDev(shape->GetNumberOfShapeParameters());
  shapeMean[0] = mean[0];
  shapeStdDev[0] = stddev[0];

  costFunction->SetShapeParameterMeans(shapeMean);
  costFunction->SetShapeParameterStandardDeviations(shapeStdDev);

  auto weights = itk::MakeFilled<CostFunctionType::WeightsType>(1.5);

  costFunction->SetWeights(weights);


  // Initialize cost function before use
  try
  {
    costFunction->Initialize();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << '\n';
    return EXIT_FAILURE;
  }

  // exercise Print method
  costFunction->Print(std::cout);

  // exercise Get methods
  std::cout << "ShapeParameterMeans: ";
  std::cout << costFunction->GetShapeParameterMeans() << '\n';
  std::cout << "ShapeStandardDeviations: ";
  std::cout << costFunction->GetShapeParameterStandardDeviations() << '\n';
  std::cout << "Weights: ";
  std::cout << costFunction->GetWeights() << '\n';
  std::cout << "ShapeFunction: ";
  std::cout << costFunction->GetShapeFunction() << '\n';
  std::cout << "ActiveRegion: ";
  std::cout << costFunction->GetActiveRegion() << '\n';
  std::cout << "FeatureImage: ";
  std::cout << costFunction->GetFeatureImage() << '\n';

  using GenericCostFunctionType = CostFunctionType::Superclass;
  std::cout << costFunction->GenericCostFunctionType::GetNameOfClass() << '\n';

  /**
   * Attempt to plug the cost function into an optimizer
   */
  using OptimizerType = itk::AmoebaOptimizer;
  auto optimizer = OptimizerType::New();

  optimizer->SetCostFunction(costFunction);
  optimizer->SetInitialPosition(mean);

  optimizer->SetFunctionConvergenceTolerance(0.01);
  optimizer->SetParametersConvergenceTolerance(0.05);

  try
  {
    optimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << '\n';
    return EXIT_FAILURE;
  }

  std::cout << "Target parameters: " << parameters << '\n';
  std::cout << "Final parameters: " << optimizer->GetCurrentPosition() << '\n';

  for (unsigned int j = 0; j < costFunction->GetNumberOfParameters(); ++j)
  {
    if (itk::Math::abs(parameters[j] - optimizer->GetCurrentPosition()[j]) > 0.5)
    {
      std::cout << "Final parameters not within tolerance. " << '\n';
      return EXIT_FAILURE;
    }
  }

  // exercise error testing

  bool pass;

#define TEST_INITIALIZATION_ERROR(ComponentName, badComponent, goodComponent) \
  costFunction->Set##ComponentName(badComponent);                             \
  try                                                                         \
  {                                                                           \
    pass = false;                                                             \
    costFunction->Initialize();                                               \
  }                                                                           \
  catch (const itk::ExceptionObject & err)                                    \
  {                                                                           \
    std::cout << "Caught expected ExceptionObject" << '\n';                   \
    std::cout << err << '\n';                                                 \
    pass = true;                                                              \
  }                                                                           \
  costFunction->Set##ComponentName(goodComponent);                            \
                                                                              \
  if (!pass)                                                                  \
  {                                                                           \
    std::cout << "Test failed." << '\n';                                      \
    return EXIT_FAILURE;                                                      \
  }                                                                           \
  ITK_MACROEND_NOOP_STATEMENT

  TEST_INITIALIZATION_ERROR(ShapeFunction, nullptr, shape);
  TEST_INITIALIZATION_ERROR(ActiveRegion, nullptr, activeRegion);
  TEST_INITIALIZATION_ERROR(FeatureImage, nullptr, edgeMap);

  CostFunctionType::ParametersType badParameters(shape->GetNumberOfShapeParameters() - 1);
  badParameters.Fill(2.0);

  TEST_INITIALIZATION_ERROR(ShapeParameterMeans, badParameters, shapeMean);
  TEST_INITIALIZATION_ERROR(ShapeParameterStandardDeviations, badParameters, shapeStdDev);

  std::cout << "Test passed." << '\n';
  return EXIT_SUCCESS;
}
