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
#define ITK_TEMPLATE_EXPLICIT_ObjectToObjectOptimizerBaseTemplate
#include "itkObjectToObjectOptimizerBase.h"

namespace itk
{

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKOptimizersv4_EXPORT ObjectToObjectOptimizerBaseTemplate<double>;
template class ITKOptimizersv4_EXPORT ObjectToObjectOptimizerBaseTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
