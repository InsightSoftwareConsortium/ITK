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
#ifndef itkIsSame_h
#define itkIsSame_h

#include "itkMetaProgrammingLibrary.h"

namespace itk
{
/// \cond HIDE_META_PROGRAMMING
namespace mpl
{

/** Tells whether two types are identical.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template<typename, typename> struct IsSame : public FalseType { };

/// \cond SPECIALIZATION_IMPLEMENTATION
template<typename T> struct IsSame<T, T> : public TrueType {};
/// \endcond

} // end namespace itk::mpl

// itk::IsSame have move to itk::mpl
// Expect them to be deprecated.
using mpl::IsSame;

/// \endcond

} // end namespace itk

#endif //itkIsSame_h
