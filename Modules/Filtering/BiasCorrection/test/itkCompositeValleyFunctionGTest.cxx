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

#include "itkCompositeValleyFunction.h"
#include "itkMath.h"
#include <gtest/gtest.h>

TEST(CompositeValleyFunction, BoundsAndInterval)
{
  itk::Array<double> means(2);
  itk::Array<double> sigmas(2);

  means[0] = 0.0;
  means[1] = 100.0;
  sigmas[0] = 20.0;
  sigmas[1] = 20.0;

  itk::CompositeValleyFunction function(means, sigmas);

  EXPECT_DOUBLE_EQ(function.GetUpperBound(), 280.0);
  EXPECT_DOUBLE_EQ(function.GetLowerBound(), -180.0);

  const double interval1 = function.GetInterval();
  const double interval2 = (function.GetUpperBound() - function.GetLowerBound()) / (1000000.0 - 1.0);
  EXPECT_NEAR(interval1, interval2, itk::NumericTraits<double>::epsilon());
}

TEST(CompositeValleyFunction, EvaluateMatchesCallOperator)
{
  itk::Array<double> means(2);
  itk::Array<double> sigmas(2);

  means[0] = 0.0;
  means[1] = 100.0;
  sigmas[0] = 20.0;
  sigmas[1] = 20.0;

  itk::CompositeValleyFunction function(means, sigmas);

  const long   numberOfSamples = function.GetNumberOfSamples();
  const double measure = function.GetLowerBound() + function.GetInterval() * numberOfSamples * 0.5;
  const double value1 = function(measure);
  const double value2 = function.Evaluate(measure);

  EXPECT_NEAR(value1, value2, itk::NumericTraits<double>::epsilon());
}
