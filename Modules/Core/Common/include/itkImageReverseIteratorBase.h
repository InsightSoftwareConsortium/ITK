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
#ifndef itkImageReverseIteratorBase_h
#define itkImageReverseIteratorBase_h

// SMOKE TEST - Unit 11: structural scaffold for templating
// ImageReverseIterator / ImageReverseConstIterator on a VIsConst flag.
// This header is intentionally compile-only for the spike and not yet
// wired into the legacy iterator classes. See .devlocal/smoke/unit-11.md.

#include "itkMacro.h"
#include <type_traits>

namespace itk
{

/** \class ImageReverseIteratorBase
 * \brief Templated base for image reverse iterators over const/non-const TImage.
 *
 * VIsConst == true  models ImageReverseConstIterator<TImage>
 * VIsConst == false models ImageReverseIterator<TImage>
 *
 * The InternalPixelType pointer held by this class is const-qualified when
 * VIsConst is true, eliminating the const_cast that the legacy
 * ImageReverseIterator/ImageReverseConstIterator headers use at
 * itkImageReverseIterator.h:103 and itkImageReverseConstIterator.h:340.
 *
 * Set() and the non-const Value() overload are SFINAE-gated on !VIsConst.
 *
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT ImageReverseIteratorBase
{
public:
  using Self = ImageReverseIteratorBase;
  using ImageType = TImage;
  using InternalPixelType = typename TImage::InternalPixelType;
  using PixelType = typename TImage::PixelType;

  /** Buffer pointer is const InternalPixelType* when VIsConst, else InternalPixelType*. */
  using BufferPointerType = std::conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>;

  static constexpr bool IsConst = VIsConst;

  ImageReverseIteratorBase() = default;

  /** Set is only enabled when VIsConst is false. */
  template <bool B = VIsConst, typename = std::enable_if_t<!B>>
  void
  Set(const PixelType & /*value*/) const
  {
    // Intentionally a structural placeholder; real body lives in the
    // legacy iterator classes for this spike.
  }

  /** Non-const Value() is only enabled when VIsConst is false. */
  template <bool B = VIsConst, typename = std::enable_if_t<!B>>
  PixelType &
  Value()
  {
    return *m_Buffer;
  }

  /** const Value() is always available. */
  const PixelType &
  Value() const
  {
    return *m_Buffer;
  }

protected:
  BufferPointerType m_Buffer{ nullptr };
};

/** Alias templates matching the legacy class names. */
template <typename TImage>
using ImageReverseConstIteratorTemplated = ImageReverseIteratorBase<TImage, true>;

template <typename TImage>
using ImageReverseIteratorTemplated = ImageReverseIteratorBase<TImage, false>;

} // end namespace itk

#endif
