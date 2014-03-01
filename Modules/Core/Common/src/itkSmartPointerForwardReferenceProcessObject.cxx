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
#include "itkSmartPointerForwardReference.h"
/** The inclusion of itkSmartPointerForwardReference.hxx is needed here
 * because this is one of the very few cases where
 * itkSmartPointerForwardReference.h does not include
 * itkSmartPointerForwardReference.hxx
 *
 * Ensure that the implicitly instantiated methods and operators are
 * exported for the linker.
 */
#if __GNUC__ >= 4
#pragma GCC visibility push(default)
#endif
#include "itkSmartPointerForwardReference.hxx"
#if __GNUC__ >= 4
#pragma GCC visibility pop
#endif


#include "itkProcessObject.h"

// Manual instantiation is necessary to prevent link errors
template class ITKCommon_EXPORT itk::SmartPointerForwardReference< itk::ProcessObject >;
