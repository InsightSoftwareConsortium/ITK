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

#include "itkBSplineTransform.h"
#include "itkBSplineTransformParametersAdaptor.h"

int
itkBSplineTransformParametersAdaptorTest(int, char *[])
{
  constexpr unsigned int SpaceDimension = 3;
  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;
  using TransformType = itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;

  /**
   * Define the transformation domain
   */

  using OriginType = TransformType::OriginType;
  auto origin = itk::MakeFilled<OriginType>(5.0);

  using PhysicalDimensionsType = TransformType::PhysicalDimensionsType;
  auto dimensions = itk::MakeFilled<PhysicalDimensionsType>(100);

  using MeshSizeType = TransformType::MeshSizeType;
  auto meshSize = MeshSizeType::Filled(10);

  using DirectionType = TransformType::DirectionType;
  DirectionType direction;
  direction.SetIdentity();

  /**
   * Instantiate a transform
   */
  auto transform = TransformType::New();
  transform->SetTransformDomainOrigin(origin);
  transform->SetTransformDomainPhysicalDimensions(dimensions);
  transform->SetTransformDomainMeshSize(meshSize);
  transform->SetTransformDomainDirection(direction);

  /**
   * Allocate memory for the parameters
   */
  using ParametersType = TransformType::ParametersType;
  const unsigned long numberOfParameters = transform->GetNumberOfParameters();
  ParametersType      parameters(numberOfParameters);
  parameters.Fill(ParametersType::ValueType{});

  /**
   * Set the parameters in the transform
   */
  transform->SetParameters(parameters);

  using CoefficientImageType = TransformType::ImageType;
  auto index = CoefficientImageType::IndexType::Filled(5);
  transform->GetCoefficientImages()[0]->SetPixel(index, 5.0);

  auto point = itk::MakeFilled<TransformType::InputPointType>(50.0);

  const TransformType::OutputPointType outputPointBeforeAdapt = transform->TransformPoint(point);


  /**
   * Instantiate the adaptor
   *   we keep the transform domain definition the same except we increase
   *   the mesh size which increases the number of control points in the grid.
   */

  TransformType::MeshSizeType requiredMeshSize;
  for (unsigned int d = 0; d < SpaceDimension; ++d)
  {
    requiredMeshSize[d] = (d + 1) * meshSize[d];
  }

  const TransformType::SizeType gridSizeBefore =
    transform->GetCoefficientImages()[0]->GetLargestPossibleRegion().GetSize();

  using AdaptorType = itk::BSplineTransformParametersAdaptor<TransformType>;
  auto adaptor = AdaptorType::New();
  adaptor->SetTransform(transform);
  adaptor->SetRequiredTransformDomainMeshSize(requiredMeshSize);
  adaptor->SetRequiredTransformDomainOrigin(transform->GetTransformDomainOrigin());
  adaptor->SetRequiredTransformDomainDirection(transform->GetTransformDomainDirection());
  adaptor->SetRequiredTransformDomainPhysicalDimensions(transform->GetTransformDomainPhysicalDimensions());
  try
  {
    adaptor->AdaptTransformParameters();
  }
  catch (...)
  {
    std::cerr << "Error in adapting transform." << '\n';
    return EXIT_FAILURE;
  }

  const ParametersType fixedParameters = adaptor->GetRequiredFixedParameters();
  std::cout << "Fixed parameters: " << fixedParameters << '\n';
  adaptor->SetRequiredFixedParameters(fixedParameters);

  if (adaptor->GetRequiredTransformDomainMeshSize() != transform->GetTransformDomainMeshSize())
  {
    std::cerr << "required transform domain mesh size conversion is incorrect." << '\n';
    return EXIT_FAILURE;
  }
  if (adaptor->GetRequiredTransformDomainOrigin() != transform->GetTransformDomainOrigin())
  {
    std::cerr << "required transform domain origin conversion is incorrect." << '\n';
    return EXIT_FAILURE;
  }
  if (adaptor->GetRequiredTransformDomainDirection() != transform->GetTransformDomainDirection())
  {
    std::cerr << "required transform domain direction conversion is incorrect." << '\n';
    return EXIT_FAILURE;
  }
  if (adaptor->GetRequiredTransformDomainPhysicalDimensions() != transform->GetTransformDomainPhysicalDimensions())
  {
    std::cerr << "required transform domain physical dimensions conversion is incorrect." << '\n';
    return EXIT_FAILURE;
  }

  const TransformType::SizeType gridSizeAfter =
    transform->GetCoefficientImages()[0]->GetLargestPossibleRegion().GetSize();

  const TransformType::OutputPointType outputPointAfterAdapt = transform->TransformPoint(point);

  std::cout << "Grid size before: " << gridSizeBefore << '\n';
  std::cout << "Grid size after: " << gridSizeAfter << '\n';
  std::cout << point << " to (before) " << outputPointBeforeAdapt << '\n';
  std::cout << point << " to (after) " << outputPointAfterAdapt << '\n';

  if (outputPointBeforeAdapt.EuclideanDistanceTo(outputPointAfterAdapt) > 1e-6)
  {
    std::cerr << "output points don't match up before and after adapt call." << '\n';
    return EXIT_FAILURE;
  }

  adaptor->Print(std::cout, 5);

  return EXIT_SUCCESS;
}
