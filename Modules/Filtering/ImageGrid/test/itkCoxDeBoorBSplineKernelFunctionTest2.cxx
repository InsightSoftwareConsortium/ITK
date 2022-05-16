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
#include "itkCoxDeBoorBSplineKernelFunction.h"

/**
 * In this test, we check to see that the derivative is calculated
 * correctly for spline orders 2 through 10
 */
int
itkCoxDeBoorBSplineKernelFunctionTest2(int, char *[])
{
  using KernelType = itk::CoxDeBoorBSplineKernelFunction<3>;
  auto kernel = KernelType::New();

  auto kernelOrderMinus1 = KernelType::New();

  for (unsigned int order = 2; order <= 10; ++order)
  {
    kernel->SetSplineOrder(order);
    kernelOrderMinus1->SetSplineOrder(order - 1);

    for (double t = 0.0; t < static_cast<double>(0.5 * (order + 1)); t += 0.1)
    {
      KernelType::RealType derivative = kernel->EvaluateDerivative(t);
      if (itk::Math::abs(derivative - (kernelOrderMinus1->Evaluate(t + 0.5) - kernelOrderMinus1->Evaluate(t - 0.5))) >
          1e-10)
      {
        return EXIT_FAILURE;
      }

      // try calculating the nth derivative
      for (unsigned int d = 2; d < order - 1; ++d)
      {
        try
        {
          kernel->EvaluateNthDerivative(t, d);
        }
        catch (...)
        {
          return EXIT_FAILURE;
        }
      }
    }
  }

  return EXIT_SUCCESS;
}
