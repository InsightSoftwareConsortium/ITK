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
#include "itkEuler3DTransform.h"

#include <gtest/gtest.h>


TEST(Euler3DTransform, SetFixedParametersThrowsWhenSizeIsLessThanInputSpaceDimension)
{
  using TransformType = itk::Euler3DTransform<>;

  for (unsigned int size{}; size < TransformType::InputSpaceDimension; ++size)
  {
    const auto                               transform = TransformType::New();
    const TransformType::FixedParametersType fixedParameters(size);
    ASSERT_THROW(transform->SetFixedParameters(fixedParameters), itk::ExceptionObject);
  }
}
