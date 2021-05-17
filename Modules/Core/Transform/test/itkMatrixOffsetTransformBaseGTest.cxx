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

// First include the header file to be tested:
#include "itkMatrixOffsetTransformBase.h"

#include <gtest/gtest.h>


namespace
{

template <typename TParametersValueType, unsigned NDimensions>
void
Check_New_MatrixOffsetTransformBase()
{
  const auto transformBase = itk::MatrixOffsetTransformBase<TParametersValueType, NDimensions, NDimensions>::New();

  EXPECT_TRUE(transformBase->GetMatrix().GetVnlMatrix().is_identity());

  const auto zeroFilledFixedArray = itk::FixedArray<double, NDimensions>::Filled(0.0);

  EXPECT_EQ(zeroFilledFixedArray, transformBase->GetOffset());
  EXPECT_EQ(zeroFilledFixedArray, transformBase->GetCenter());
  EXPECT_EQ(zeroFilledFixedArray, transformBase->GetTranslation());
}


template <unsigned int NDimensions>
void
Assert_SetFixedParameters_throws_when_size_is_less_than_NDimensions()
{
  for (unsigned int size{}; size < NDimensions; ++size)
  {
    using TransformBaseType = itk::MatrixOffsetTransformBase<double, NDimensions, NDimensions>;
    using FixedParametersType = typename TransformBaseType::FixedParametersType;

    const auto                transformBase = TransformBaseType::New();
    const FixedParametersType fixedParameters(size);
    ASSERT_THROW(transformBase->SetFixedParameters(fixedParameters), itk::ExceptionObject);
  }
}

} // namespace


// Checks the object created by MatrixOffsetTransformBase::New().
TEST(MatrixOffsetTransformBase, CheckNew)
{
  Check_New_MatrixOffsetTransformBase<float, 2>();
  Check_New_MatrixOffsetTransformBase<double, 3>();
}


TEST(MatrixOffsetTransformBase, SetFixedParametersThrowsWhenSizeIsLessThanNDimensions)
{
  Assert_SetFixedParameters_throws_when_size_is_less_than_NDimensions<2>();
  Assert_SetFixedParameters_throws_when_size_is_less_than_NDimensions<3>();
  Assert_SetFixedParameters_throws_when_size_is_less_than_NDimensions<4>();
}
