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
#ifndef itkTestDriverIncludeRequiredFactories_h
#define itkTestDriverIncludeRequiredFactories_h

// WARNING: This header is part of ITK's internal test infrastructure.
// External projects should NOT include this header or copy its registration
// pattern. Instead, link against the factory meta-module targets:
//
//   target_link_libraries(MyTestDriver PRIVATE ITK::ITKFFTImageFilterInit)
//
// See the ITK 6 Migration Guide section "Factory Registration in External
// Project Test Drivers" for the recommended approach.

#include "itkTestDriverInclude.h"

#ifdef __EMSCRIPTEN__
#  include <emscripten.h>
#endif

void
RegisterRequiredFactories();

void
RegisterRequiredIOFactories();

void
RegisterRequiredFFTFactories();

void
ProcessArgumentsAndRegisterRequiredFactories(int * argc, ArgumentStringType * argv);

#endif
