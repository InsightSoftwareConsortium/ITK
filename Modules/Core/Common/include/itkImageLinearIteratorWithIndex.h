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
#ifndef itkImageLinearIteratorWithIndex_h
#define itkImageLinearIteratorWithIndex_h

#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{
/** SMOKE TEST (unit 6): mutable linear iterator is now the `VIsConst=false`
 * specialization of ImageLinearIteratorWithIndexBase. The const_cast
 * workarounds formerly present in Set() / Value() are gone because the
 * non-const base holds InternalPixelType* directly.
 * \ingroup ITKCommon
 */
template <typename TImage>
using ImageLinearIteratorWithIndex = ImageLinearIteratorWithIndexBase<TImage, /*VIsConst=*/false>;

// Deduction guide (CTAD) for the mutable alias.
template <typename TImage>
ImageLinearIteratorWithIndexBase(TImage *, const typename TImage::RegionType &)
  -> ImageLinearIteratorWithIndexBase<TImage, false>;

} // end namespace itk

#endif
