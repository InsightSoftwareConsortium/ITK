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

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkMath.h"

int
itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptorTest(int, char *[])
{
  constexpr unsigned int SpaceDimension = 3;
  using CoordinateRepType = double;
  using TransformType = itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<CoordinateRepType, SpaceDimension>;

  /**
   * Define the transformation domain
   */
  using PointType = TransformType::PointType;
  PointType origin;
  origin.Fill(-5.0);

  using SizeType = TransformType::SizeType;
  SizeType size;
  size.Fill(65);

  using SpacingType = TransformType::SpacingType;
  SpacingType spacing;
  spacing.Fill(1.2);

  using DirectionType = TransformType::DirectionType;
  DirectionType direction;
  direction.SetIdentity();

  using DisplacementFieldType = TransformType::DisplacementFieldType;
  DisplacementFieldType::Pointer displacementField = DisplacementFieldType::New();
  displacementField->SetOrigin(origin);
  displacementField->SetSpacing(spacing);
  displacementField->SetRegions(size);
  displacementField->SetDirection(direction);
  displacementField->Allocate();

  TransformType::OutputVectorType zeroVector;
  zeroVector.Fill(0);
  displacementField->FillBuffer(zeroVector);


  TransformType::OutputVectorType nonzeroVector;
  nonzeroVector.Fill(10.3);

  DisplacementFieldType::IndexType index;
  index.Fill(40);
  displacementField->SetPixel(index, nonzeroVector);

  /**
   * Instantiate a transform
   */
  std::cout << "Initialize transform." << std::endl;

  TransformType::Pointer transform = TransformType::New();
  transform->SetDisplacementField(displacementField);

  TransformType::InputPointType point;
  point.Fill(50.0);
  TransformType::OutputPointType outputPointBeforeAdapt = transform->TransformPoint(point);

  SpacingType spacingBefore = transform->GetDisplacementField()->GetSpacing();
  SizeType    sizeBefore = transform->GetDisplacementField()->GetLargestPossibleRegion().GetSize();

  /**
   * Instantiate the adaptor
   *   we keep the transform domain definition the same except we increase
   *   the size and decrease the spacing.
   */

  std::cout << "Instantiate adaptor." << std::endl;

  SpacingType requiredSpacing;
  requiredSpacing.Fill(0.6);
  SizeType requiredSize;
  for (unsigned int d = 0; d < SpaceDimension; d++)
  {
    requiredSize[d] = static_cast<SizeType::SizeValueType>((spacing[d] * (size[d] - 1) / requiredSpacing[d]) + 1);
  }

  using AdaptorType = itk::GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TransformType>;
  AdaptorType::Pointer adaptor = AdaptorType::New();
  adaptor->SetTransform(transform);
  adaptor->SetRequiredSize(requiredSize);
  adaptor->SetRequiredSpacing(requiredSpacing);
  adaptor->SetRequiredOrigin(displacementField->GetOrigin());
  adaptor->SetRequiredDirection(displacementField->GetDirection());
  adaptor->SetGaussianSmoothingVarianceForTheUpdateField(1.0);
  adaptor->SetGaussianSmoothingVarianceForTheTotalField(0.2);
  try
  {
    adaptor->AdaptTransformParameters();
  }
  catch (...)
  {
    std::cerr << "Error in adapting transform." << std::endl;
    return EXIT_FAILURE;
  }


  SpacingType spacingAfter = transform->GetDisplacementField()->GetSpacing();
  SizeType    sizeAfter = transform->GetDisplacementField()->GetLargestPossibleRegion().GetSize();

  std::cout << "Spacing: " << spacingBefore << "(before), " << spacingAfter << "(after)." << std::endl;
  std::cout << "Size: " << sizeBefore << "(before), " << sizeAfter << "(after)." << std::endl;

  TransformType::ParametersType fixedParameters = adaptor->GetRequiredFixedParameters();
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
  if (itk::Math::NotAlmostEquals(adaptor->GetGaussianSmoothingVarianceForTheUpdateField(),
                                 transform->GetGaussianSmoothingVarianceForTheUpdateField()))
  {
    std::cerr << "update field variance conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }
  if (itk::Math::NotAlmostEquals(adaptor->GetGaussianSmoothingVarianceForTheTotalField(),
                                 transform->GetGaussianSmoothingVarianceForTheTotalField()))
  {
    std::cerr << "total field variance conversion is incorrect." << std::endl;
    return EXIT_FAILURE;
  }

  TransformType::OutputPointType outputPointAfterAdapt = transform->TransformPoint(point);
  std::cout << point << " to (before) " << outputPointBeforeAdapt << std::endl;
  std::cout << point << " to (after) " << outputPointAfterAdapt << std::endl;

  if (outputPointBeforeAdapt.EuclideanDistanceTo(outputPointAfterAdapt) > 1e-6)
  {
    std::cerr << "output points don't match up before and after adapt call." << std::endl;
    return EXIT_FAILURE;
  }

  adaptor->Print(std::cout, 5);

  return EXIT_SUCCESS;
}
