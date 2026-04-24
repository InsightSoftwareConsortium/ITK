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
#ifndef itkImageRegionIterator_h
#define itkImageRegionIterator_h

#include "itkImageRegionConstIterator.h"

namespace itk
{
/** \class ImageRegionIterator
 * \brief Writable region iterator.
 *
 * SMOKE-TEST: this is now an alias template over ImageRegionIteratorBase<TImage,false>.
 * Set() and non-const Value() are defined on the base, SFINAE-gated so that only
 * the VIsConst==false specialization exposes them.
 *
 * NOTE (parent-class ripple): the Superclass ImageConstIterator<TImage> is NOT
 * templated on const-ness in this worktree. The const_cast on m_Buffer therefore
 * still appears inside Set()/Value(). Completing unit 1 (template ImageIterator
 * on VIsConst) propagates the non-const pointer type into the base, at which
 * point the cast can be fully removed.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
using ImageRegionIterator = ImageRegionIteratorBase<TImage, /*VIsConst=*/false>;

// Deduction guide for class template argument deduction (CTAD).
// (Inherits the base template's guide; nothing extra needed here.)

} // end namespace itk

#endif
