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

#include "itkTimeVaryingVelocityFieldTransform.h"
#include "itkTestingMacros.h"


int
itkTimeVaryingVelocityFieldTransformTest(int, char *[])
{
  constexpr unsigned int ComponentDimension = 3;
  constexpr unsigned int VelocityFieldDimension = 4;

  using ComponentType = double;

  using VectorType = itk::Vector<ComponentType, ComponentDimension>;
  using DisplacementFieldType = itk::Image<VectorType, ComponentDimension>;
  using TimeVaryingVelocityFieldType = itk::Image<VectorType, VelocityFieldDimension>;

  TimeVaryingVelocityFieldType::PointType origin;
  origin.Fill(0.0);

  TimeVaryingVelocityFieldType::SpacingType spacing;
  spacing.Fill(2.0);

  TimeVaryingVelocityFieldType::SizeType size;
  size.Fill(25);

  VectorType displacement1;
  displacement1.Fill(0.1);

  TimeVaryingVelocityFieldType::Pointer timeVaryingVelocityField = TimeVaryingVelocityFieldType::New();

  timeVaryingVelocityField->SetOrigin(origin);
  timeVaryingVelocityField->SetSpacing(spacing);
  timeVaryingVelocityField->SetRegions(size);
  timeVaryingVelocityField->Allocate();
  timeVaryingVelocityField->FillBuffer(displacement1);

  using IntegratorType =
    itk::TimeVaryingVelocityFieldIntegrationImageFilter<TimeVaryingVelocityFieldType, DisplacementFieldType>;

  IntegratorType::Pointer integrator = IntegratorType::New();
  integrator->SetInput(timeVaryingVelocityField);
  integrator->SetLowerTimeBound(0.3);
  integrator->SetUpperTimeBound(0.75);
  integrator->SetNumberOfIntegrationSteps(10);
  integrator->Update();

  integrator->Print(std::cout, 3);

  DisplacementFieldType::IndexType index;
  index.Fill(0);
  VectorType displacementPixel;

  // This integration should result in a constant image of value
  // 0.75 * 0.1 - 0.3 * 0.1 = 0.045 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * displacementField = integrator->GetOutput();

  displacementPixel = displacementField->GetPixel(index);

  std::cout << "Estimated forward displacement vector: " << displacementPixel << std::endl;
  if (itk::Math::abs(displacementPixel[0] - 0.045) > 0.0001)
  {
    std::cerr << "Failed to produce the correct forward integration." << std::endl;
    return EXIT_FAILURE;
  }

  IntegratorType::Pointer inverseIntegrator = IntegratorType::New();
  inverseIntegrator->SetInput(timeVaryingVelocityField);
  inverseIntegrator->SetLowerTimeBound(1.0);
  inverseIntegrator->SetUpperTimeBound(0.0);
  inverseIntegrator->SetNumberOfIntegrationSteps(10);
  inverseIntegrator->Update();

  // This integration should result in a constant image of value
  // -( 0.1 * 1.0 - ( 0.1 * 0.0 ) ) = -0.1 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * inverseField = inverseIntegrator->GetOutput();
  displacementPixel = inverseField->GetPixel(index);
  if (itk::Math::abs(displacementPixel[0] + 0.101852) > 0.01)
  {
    std::cerr << "Failed to produce the correct inverse integration." << std::endl;
    return EXIT_FAILURE;
  }

  // Now test the transform

  using TransformType = itk::TimeVaryingVelocityFieldTransform<ComponentType, ComponentDimension>;
  TransformType::Pointer transform = TransformType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(transform, TimeVaryingVelocityFieldTransform, VelocityFieldTransform);

  transform->SetLowerTimeBound(0.0);
  transform->SetUpperTimeBound(1.0);

  ITK_TRY_EXPECT_EXCEPTION(transform->IntegrateVelocityField());


  transform->SetVelocityField(timeVaryingVelocityField);
  ITK_TEST_SET_GET_VALUE(timeVaryingVelocityField, transform->GetModifiableTimeVaryingVelocityField());
  ITK_TEST_SET_GET_VALUE(timeVaryingVelocityField, transform->GetTimeVaryingVelocityField());

  ITK_TRY_EXPECT_NO_EXCEPTION(transform->IntegrateVelocityField());


  // Now Clone the Transform and test transform again
  TransformType::Pointer clone = transform->Clone();

  TransformType::InputPointType point;
  point.Fill(1.3);

  using OutputPointType = TransformType::OutputPointType;
  OutputPointType transformedPoint = transform->TransformPoint(point);
  OutputPointType cloneTransformedPoint = clone->TransformPoint(point);

  VectorType displacement;
  displacement.Fill(0.1);

  point += displacement;

  if (point.EuclideanDistanceTo(transformedPoint) > 0.01)
  {
    std::cerr << "Failed to produce the expected transformed point." << std::endl;
    return EXIT_FAILURE;
  }
  if (point.EuclideanDistanceTo(cloneTransformedPoint) > 0.01)
  {
    std::cerr << "Cloned transform failed to produce the expected transformed point." << std::endl;
    return EXIT_FAILURE;
  }
  point -= displacement;

  TransformType::InputPointType point2;
  point2.CastFrom(transformedPoint);

  TransformType::InputPointType clonePoint2;
  clonePoint2.CastFrom(cloneTransformedPoint);

  using InverseTransformBasePointer = TransformType::InverseTransformBasePointer;
  InverseTransformBasePointer inverseTransform = transform->GetInverseTransform();
  InverseTransformBasePointer cloneInverseTransform = clone->GetInverseTransform();

  transformedPoint = inverseTransform->TransformPoint(point2);
  cloneTransformedPoint = cloneInverseTransform->TransformPoint(clonePoint2);

  if (point.EuclideanDistanceTo(transformedPoint) > 0.01)
  {
    std::cerr << "Failed to produce the expected inverse transformed point." << std::endl;
    return EXIT_FAILURE;
  }
  if (point.EuclideanDistanceTo(cloneTransformedPoint) > 0.01)
  {
    std::cerr << "Cloned transform failed to produce the expected inverse transformed point." << std::endl;
    return EXIT_FAILURE;
  }

  transform->Print(std::cout, 3);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
