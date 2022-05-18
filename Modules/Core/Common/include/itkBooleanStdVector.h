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
#ifndef itkBooleanStdVector_h
#define itkBooleanStdVector_h

#include "itkBoolean.h"
#include <vector>

namespace itk
{

/** The type alias `BooleanStdVectorType` provides an alternative to `std::vector<bool>`. `std::vector<bool>` is not
 * thread safe due to the possibility of multiple bits being packed together in the same memory location.
 * BooleanStdVectorType does not have such a space optimization. BooleanStdVectorType is semantically like
 * `std::vector<bool>`, but unlike `std::vector<bool>`, it does "avoid data races when the contents of the contained
 * object in different elements in the same container [...] are modified concurrently", according to the C++ Standard,
 * section [container.requirements.dataraces], "Container data races".
 */
using BooleanStdVectorType = std::vector<Boolean>;

} // namespace itk

#endif
