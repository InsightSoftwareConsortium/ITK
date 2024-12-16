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

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkTestingMacros.h"

int
itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptorTest(int, char *[])
{
  constexpr unsigned int SpaceDimension = 3;
  using CoordinateRepType = double;
  using TransformType = itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<CoordinateRepType, SpaceDimension>;

  /**
   * Define the transformation domain
   */
  using PointType = TransformType::PointType;
  auto origin = itk::MakeFilled<PointType>(-5.0);

  using SizeType = TransformType::SizeType;
  auto size = SizeType::Filled(65);

  using SpacingType = TransformType::SpacingType;
  auto spacing = itk::MakeFilled<SpacingType>(1.2);

  using DirectionType = TransformType::DirectionType;
  DirectionType direction;
  direction.SetIdentity();

  using DisplacementFieldType = TransformType::DisplacementFieldType;
  auto displacementField = DisplacementFieldType::New();
  displacementField->SetOrigin(origin);
  displacementField->SetSpacing(spacing);
  displacementField->SetRegions(size);
  displacementField->SetDirection(direction);
  displacementField->Allocate();

  constexpr TransformType::OutputVectorType zeroVector{};
  displacementField->FillBuffer(zeroVector);


  auto nonzeroVector = itk::MakeFilled<TransformType::OutputVectorType>(10.3);

  auto index = DisplacementFieldType::IndexType::Filled(40);
  displacementField->SetPixel(index, nonzeroVector);

  /**
   * Instantiate a transform
   */
  std::cout << "Initialize transform." << std::endl;

  auto transform = TransformType::New();
  transform->SetDisplacementField(displacementField);

  auto                                 point = itk::MakeFilled<TransformType::InputPointType>(50.0);
  const TransformType::OutputPointType outputPointBeforeAdapt = transform->TransformPoint(point);

  const SpacingType spacingBefore = transform->GetDisplacementField()->GetSpacing();
  const SizeType    sizeBefore = transform->GetDisplacementField()->GetLargestPossibleRegion().GetSize();

  /**
   * Instantiate the adaptor
   *   we keep the transform domain definition the same except we increase
   *   the size and decrease the spacing.
   */

  std::cout << "Instantiate adaptor." << std::endl;

  auto     requiredSpacing = itk::MakeFilled<SpacingType>(0.6);
  SizeType requiredSize;
  for (unsigned int d = 0; d < SpaceDimension; ++d)
  {
    requiredSize[d] = static_cast<SizeType::SizeValueType>((spacing[d] * (size[d] - 1) / requiredSpacing[d]) + 1);
  }

  using AdaptorType = itk::BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TransformType>;
  auto adaptor = AdaptorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(adaptor,
                                    BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor,
                                    DisplacementFieldTransformParametersAdaptor);


  adaptor->SetTransform(transform);
  adaptor->SetRequiredSize(requiredSize);
  adaptor->SetRequiredSpacing(requiredSpacing);
  adaptor->SetRequiredOrigin(displacementField->GetOrigin());
  adaptor->SetRequiredDirection(displacementField->GetDirection());

  AdaptorType::ArrayType updateMeshSize;
  AdaptorType::ArrayType totalMeshSize;

  updateMeshSize.Fill(10);
  totalMeshSize.Fill(0);

  adaptor->SetMeshSizeForTheUpdateField(updateMeshSize);
  adaptor->SetMeshSizeForTheTotalField(totalMeshSize);
  try
  {
    adaptor->AdaptTransformParameters();
  }
  catch (...)
  {
    std::cerr << "Error in adapting transform." << std::endl;
    return EXIT_FAILURE;
  }


  const SpacingType spacingAfter = transform->GetDisplacementField()->GetSpacing();
  const SizeType    sizeAfter = transform->GetDisplacementField()->GetLargestPossibleRegion().GetSize();

  std::cout << "Spacing: " << spacingBefore << "(before), " << spacingAfter << "(after)." << std::endl;
  std::cout << "Size: " << sizeBefore << "(before), " << sizeAfter << "(after)." << std::endl;

  const TransformType::ParametersType fixedParameters = adaptor->GetRequiredFixedParameters();
  std::cout << "Fixed parameters: " << fixedParameters << std::endl;
  adaptor->SetRequiredFixedParameters(fixedParameters);

  if (adaptor->GetRequiredSize() != transform->GetDisplacementField()->GetLargestPossibleRegion().GetSize())
  {
    std::cerr << "required size conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }
  if (adaptor->GetRequiredSpacing() != transform->GetDisplacementField()->GetSpacing())
  {
    std::cerr << "required spacing conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }
  if (adaptor->GetRequiredOrigin() != transform->GetDisplacementField()->GetOrigin())
  {
    std::cerr << "required origin conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }
  if (adaptor->GetRequiredDirection() != transform->GetDisplacementField()->GetDirection())
  {
    std::cerr << "required direction conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }
  if (adaptor->GetNumberOfControlPointsForTheUpdateField() != transform->GetNumberOfControlPointsForTheUpdateField())
  {
    std::cerr << "update field mesh conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }
  if (adaptor->GetNumberOfControlPointsForTheTotalField() != transform->GetNumberOfControlPointsForTheTotalField())
  {
    std::cerr << "total field mesh conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }

  const TransformType::OutputPointType outputPointAfterAdapt = transform->TransformPoint(point);
  std::cout << point << " to (before) " << outputPointBeforeAdapt << std::endl;
  std::cout << point << " to (after) " << outputPointAfterAdapt << std::endl;

  if (outputPointBeforeAdapt.EuclideanDistanceTo(outputPointAfterAdapt) > 1e-6)
  {
    std::cerr << "output points don't match up before and after adapt call." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
