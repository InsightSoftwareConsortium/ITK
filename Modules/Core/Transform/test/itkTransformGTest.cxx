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

// First include the header file to be tested:
#include "itkTransform.h"

#include <gtest/gtest.h>

namespace
{
template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
class DerivedTransform : public itk::Transform<TParametersValueType, VInputDimension, VOutputDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DerivedTransform);

  using Self = DerivedTransform;
  using Superclass = itk::Transform<TParametersValueType, VInputDimension, VOutputDimension>;
  using Pointer = itk::SmartPointer<Self>;

  itkNewMacro(Self);

private:
  DerivedTransform() = default;

  using typename Superclass::FixedParametersType;
  using typename Superclass::InputPointType;
  using typename Superclass::JacobianType;
  using typename Superclass::OutputPointType;
  using typename Superclass::ParametersType;

  static constexpr const char * exceptionMessage =
    "This member function is not meant to be called during unit testing!";

  void
  SetParameters(const ParametersType &) override
  {
    itkExceptionMacro(<< exceptionMessage);
  }

  void
  SetFixedParameters(const FixedParametersType &) override
  {
    itkExceptionMacro(<< exceptionMessage);
  }

  OutputPointType
  TransformPoint(const InputPointType &) const override
  {
    itkExceptionMacro(<< exceptionMessage);
  }

  void
  ComputeJacobianWithRespectToParameters(const InputPointType &, JacobianType &) const override
  {
    itkExceptionMacro(<< exceptionMessage);
  }
};
} // namespace


// Tests that a transform derived from itk::Transform has empty Parameters and FixedParameters by default.
TEST(Transform, HasEmptyParametersAndFixedParametersByDefault)
{
  const auto expectEmpty = [](const auto & transform) {
    EXPECT_TRUE(transform.GetParameters().empty());
    EXPECT_TRUE(transform.GetFixedParameters().empty());
  };

  expectEmpty(*(DerivedTransform<float, 2, 2>::New()));
  expectEmpty(*(DerivedTransform<double, 3, 4>::New()));
}
