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

#include "itkImageBase.h"

namespace itk
{
#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKCommon_EXPORT ImageBase< 1u >;
template class ITKCommon_EXPORT ImageBase< 2u >;
template class ITKCommon_EXPORT ImageBase< 3u >;
template class ITKCommon_EXPORT ImageBase< 4u >;
template class ITKCommon_EXPORT ImageBase< 5u >;
template class ITKCommon_EXPORT ImageBase< 6u >;
template class ITKCommon_EXPORT ImageBase< 7u >;
template class ITKCommon_EXPORT ImageBase< 8u >;
template class ITKCommon_EXPORT ImageBase< 9u >;
template class ITKCommon_EXPORT ImageBase< 10u >;

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif
}  // end namespace itk
