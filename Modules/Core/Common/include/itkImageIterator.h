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
#ifndef itkImageIterator_h
#define itkImageIterator_h

#include "itkImageConstIterator.h"

namespace itk
{
/** \class ImageIterator
 * \brief A multi-dimensional iterator templated over image type.
 *
 * ImageIterator is the mutable flavor of the iterator pair. It is defined
 * as an alias template over `ImageIteratorBase<TImage, /*VIsConst=*\/false>`,
 * so it shares the same implementation as `ImageConstIterator` with pointer
 * types that track the const-ness of the traversed image. This removes the
 * `const_cast`s that previously appeared in `Set()`, `Value()`, and
 * `GetImage()` on the non-const flavor.
 *
 * Public API is preserved: default ctor, `(TImage*, RegionType)` ctor,
 * copy ctor, assignment, `Set()`, `Value()`, `Get()`, `GetImage()`, and
 * conversion from `ImageConstIterator<TImage>` (via the SFINAE-gated
 * converting constructor in the base) are unchanged from a caller's view.
 *
 * \sa ImageConstIterator \sa ImageIteratorBase
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
using ImageIterator = ImageIteratorBase<TImage, /*VIsConst=*/false>;

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageIteratorBase(TImage *, const typename TImage::RegionType &) -> ImageIteratorBase<TImage, false>;

} // end namespace itk

// NOTE: the legacy itkImageIterator.hxx file is intentionally NOT included
// here anymore. The alias-template form defines all member functions inline
// in itkImageConstIterator.h; the out-of-class definitions in the .hxx no
// longer apply to an alias-template and would fail to compile. The .hxx
// file is retained on disk for now (unused) to keep this smoke-test diff
// scoped to the two declared headers, but it would be deleted in a real
// refactor.

#endif
