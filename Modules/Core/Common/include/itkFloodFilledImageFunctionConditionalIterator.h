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
#ifndef itkFloodFilledImageFunctionConditionalIterator_h
#define itkFloodFilledImageFunctionConditionalIterator_h

#include "itkFloodFilledImageFunctionConditionalConstIterator.h"

namespace itk
{
/**
 * \class FloodFilledImageFunctionConditionalIterator
 * \brief Iterates over a flood-filled image function with write access to pixels.
 *
 * \ingroup ImageIterators
 *
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateImageStartingAtSeed,Iterate Image Starting At Seed}
 * \endsphinx
 */
template <typename TImage, typename TFunction>
class ITK_TEMPLATE_EXPORT FloodFilledImageFunctionConditionalIterator
  : public FloodFilledImageFunctionConditionalIteratorBase<TImage, TFunction, /*VIsConst=*/false>
{
public:
  using Superclass = FloodFilledImageFunctionConditionalIteratorBase<TImage, TFunction, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

} // end namespace itk

#endif
