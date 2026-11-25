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
#include "itkTransformIOBase.h"

#include <string>

#include <gtest/gtest.h>

TEST(TransformIOBaseGTest, CorrectTransformPrecisionTypeThrowsWithoutPrecisionToken)
{
  std::string nameForFloat{ "UnknownTransform_3_3" };
  EXPECT_THROW(itk::TransformIOBaseTemplate<float>::CorrectTransformPrecisionType(nameForFloat), itk::ExceptionObject);

  std::string nameForDouble{ "UnknownTransform_3_3" };
  EXPECT_THROW(itk::TransformIOBaseTemplate<double>::CorrectTransformPrecisionType(nameForDouble),
               itk::ExceptionObject);
}

TEST(TransformIOBaseGTest, CorrectTransformPrecisionTypeCorrectsMismatchedPrecision)
{
  std::string doubleName{ "MatrixOffsetTransformBase_double_3_3" };
  itk::TransformIOBaseTemplate<float>::CorrectTransformPrecisionType(doubleName);
  EXPECT_EQ(doubleName, "MatrixOffsetTransformBase_float_3_3");

  std::string floatName{ "MatrixOffsetTransformBase_float_3_3" };
  itk::TransformIOBaseTemplate<double>::CorrectTransformPrecisionType(floatName);
  EXPECT_EQ(floatName, "MatrixOffsetTransformBase_double_3_3");
}
