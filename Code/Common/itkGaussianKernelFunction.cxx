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
#include "itkGaussianKernelFunction.h"
#include "vnl/vnl_math.h"

namespace itk
{
/**
 * Initialize static const m_Factor
 */
const double GaussianKernelFunction:: m_Factor =
  1.0 / vcl_sqrt(2.0 * vnl_math::pi);

GaussianKernelFunction::GaussianKernelFunction()
{}

GaussianKernelFunction::~GaussianKernelFunction()
{}
} // namespace itk
