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
#ifndef itkShapedFloodFilledImageFunctionConditionalIterator_h
#define itkShapedFloodFilledImageFunctionConditionalIterator_h

#include "itkShapedFloodFilledImageFunctionConditionalConstIterator.h"

namespace itk
{
/**
 * \class ShapedFloodFilledImageFunctionConditionalIterator
 * \brief Iterates over a flood-filled image function with write access
 *        to pixels.
 *
 * Contributed as a paper to the Insight Journal:
 * https://doi.org/10.54294/iei8xt
 *
 * \ingroup ImageIterators
 *
 * \ingroup ITKCommon
 */
template <typename TImage, typename TFunction>
class ITK_TEMPLATE_EXPORT ShapedFloodFilledImageFunctionConditionalIterator
  : public ShapedFloodFilledImageFunctionConditionalIteratorBase<TImage, TFunction, /*VIsConst=*/false>
{
public:
  using Superclass = ShapedFloodFilledImageFunctionConditionalIteratorBase<TImage, TFunction, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

} // end namespace itk

#endif
