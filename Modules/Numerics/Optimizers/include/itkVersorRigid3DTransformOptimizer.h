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
#ifndef itkVersorRigid3DTransformOptimizer_h
#define itkVersorRigid3DTransformOptimizer_h

#include "itkVersorTransformOptimizer.h"

// At some point in the distant future, remove support for VersorRigid3DTransformOptimizer
// #if defined(ITK_FUTURE_LEGACY_REMOVE)
// #warning "itkVersorRigid3DTransformOptimizer is identical to itkVersorTransformOptimizer, please replace"
// #else
namespace itk
{
using VersorRigid3DTransformOptimizer = VersorTransformOptimizer;
} // end namespace itk
// #endif

#endif
