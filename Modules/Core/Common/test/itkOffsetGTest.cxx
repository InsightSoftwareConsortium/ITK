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
#include "itkOffset.h"
#include "itkRangeGTestUtilities.h"


static_assert(itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::Offset<>>() &&
                itk::RangeGTestUtilities::CheckConstexprBeginAndEndOfContainer<itk::Offset<1>>(),
              "Check constexpr begin() and end() of Offset.");
