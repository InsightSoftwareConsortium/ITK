/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkLabelImageGenericInterpolateImageFunction_hxx
#define itkLabelImageGenericInterpolateImageFunction_hxx

#include "itkLabelImageGenericInterpolateImageFunction.h"
#include <itkImageRegionConstIterator.h>

namespace itk
{

template <typename TInputImage, template <typename, typename> class TInterpolator, typename TCoordRep>
void
LabelImageGenericInterpolateImageFunction<TInputImage, TInterpolator, TCoordRep>::SetInputImage(
  const TInputImage * image)
{
  /* We have one adaptor and one interpolator per label to keep the class thread-safe:
   * changing the adaptor's accepted value wouldn't work when called from a multi-threaded filter */
  using IteratorType = itk::ImageRegionConstIterator<TInputImage>;
  if (image)
  {
    m_Labels.clear();
    IteratorType it(image, image->GetLargestPossibleRegion());
    for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      m_Labels.insert(it.Get());
    }
    m_InternalInterpolators.clear();
    m_LabelSelectionAdaptors.clear();
    for (auto i = m_Labels.begin(); i != m_Labels.end(); ++i)
    {
      typename LabelSelectionAdaptorType::Pointer adapt = LabelSelectionAdaptorType::New();
      // This adaptor doesn't implement Set() so this should be safe
      adapt->SetImage(const_cast<TInputImage *>(image));
      adapt->SetAcceptedValue(*i);
      m_LabelSelectionAdaptors.push_back(adapt);
      typename InternalInterpolatorType::Pointer interp = InternalInterpolatorType::New();
      interp->SetInputImage(adapt);
      m_InternalInterpolators.push_back(interp);
    }
  }
  Superclass::SetInputImage(image);
}

template <typename TInputImage, template <typename, typename> class TInterpolator, typename TCoordRep>
typename LabelImageGenericInterpolateImageFunction<TInputImage, TInterpolator, TCoordRep>::OutputType
LabelImageGenericInterpolateImageFunction<TInputImage, TInterpolator, TCoordRep>::EvaluateAtContinuousIndex(
  const ContinuousIndexType & cindex,
  OutputType *                itkNotUsed(grad)) const
{
  /* Interpolate the binary mask corresponding to each label and return the label
   * with the highest value */
  double         value = 0;
  InputPixelType best_label = itk::NumericTraits<InputPixelType>::ZeroValue();
  int            i = 0;
  for (auto it = m_Labels.begin(); it != m_Labels.end(); ++it)
  {
    double tmp = m_InternalInterpolators[i]->EvaluateAtContinuousIndex(cindex);
    if (tmp > value)
    {
      value = tmp;
      best_label = (*it);
    }
    ++i;
  }
  return best_label;
}

} // namespace itk

#endif
