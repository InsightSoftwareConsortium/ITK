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

#ifndef itkShanbhagThresholdCalculator_h
#define itkShanbhagThresholdCalculator_h

#include "itkHistogramThresholdCalculator.h"

namespace itk
{

/**
 *\class ShanbhagThresholdCalculator
 * \brief Computes the Shanbhag threshold for an image. Aka intermeans
 *
 * Shanhbag A.G. (1994) "Utilization of Information Measure as a Means of
 * Image Thresholding" Graphical Models and Image Processing, 56(5): 414-419
 *
 * This class is templated over the input histogram type.
 * \warning This calculator assumes that the input histogram has only one dimension.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/811
 *
 * \ingroup Operators
 * \ingroup ITKThresholding
 */
template <typename THistogram, typename TOutput = double>
class ITK_TEMPLATE_EXPORT ShanbhagThresholdCalculator : public HistogramThresholdCalculator<THistogram, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ShanbhagThresholdCalculator);

  /** Standard class type aliases. */
  using Self = ShanbhagThresholdCalculator;
  using Superclass = HistogramThresholdCalculator<THistogram, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShanbhagThresholdCalculator, HistogramThresholdCalculator);

  /** Type definition for the input image. */
  using HistogramType = THistogram;
  using OutputType = TOutput;

protected:
  ShanbhagThresholdCalculator() = default;
  ~ShanbhagThresholdCalculator() override = default;
  void
  GenerateData() override;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShanbhagThresholdCalculator.hxx"
#endif

#endif
