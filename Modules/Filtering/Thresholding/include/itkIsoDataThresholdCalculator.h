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

#ifndef itkIsoDataThresholdCalculator_h
#define itkIsoDataThresholdCalculator_h

#include "itkHistogramThresholdCalculator.h"

namespace itk
{

/**
 *\class IsoDataThresholdCalculator
 * \brief Computes the IsoData threshold for an image. Aka intermeans
 *
 * Iterative procedure based on the isodata algorithm [T.W. Ridler, S. Calvard, Picture
 * thresholding using an iterative selection method, IEEE Trans. System, Man and
 * Cybernetics, SMC-8 (1978) 630-632.]
 * The procedure divides the image into objects and background by taking an initial threshold,
 * then the averages of the pixels at or below the threshold and pixels above are computed.
 * The averages of those two values are computed, the threshold is incremented and the
 * process is repeated until the threshold is larger than the composite average. That is,
 * threshold = (average background + average objects)/2
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
class ITK_TEMPLATE_EXPORT IsoDataThresholdCalculator : public HistogramThresholdCalculator<THistogram, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IsoDataThresholdCalculator);

  /** Standard class type aliases. */
  using Self = IsoDataThresholdCalculator;
  using Superclass = HistogramThresholdCalculator<THistogram, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IsoDataThresholdCalculator, HistogramThresholdCalculator);

  /** Type definition for the input image. */
  using HistogramType = THistogram;
  using OutputType = TOutput;

protected:
  IsoDataThresholdCalculator() = default;
  ~IsoDataThresholdCalculator() override = default;
  void
  GenerateData() override;

  using SizeValueType = typename HistogramType::SizeValueType;
  using InstanceIdentifier = typename HistogramType::InstanceIdentifier;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIsoDataThresholdCalculator.hxx"
#endif

#endif
