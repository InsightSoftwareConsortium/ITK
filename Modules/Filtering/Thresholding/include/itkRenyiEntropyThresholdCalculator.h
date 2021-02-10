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

#ifndef itkRenyiEntropyThresholdCalculator_h
#define itkRenyiEntropyThresholdCalculator_h

#include "itkHistogramThresholdCalculator.h"

namespace itk
{

/**
 *\class RenyiEntropyThresholdCalculator
 * \brief Computes the RenyiEntropy's threshold for an image.
 *
 * Kapur J.N., Sahoo P.K., and Wong A.K.C. (1985) "A New Method for
 * Gray-Level Picture Thresholding Using the Entropy of the Histogram"
 * Graphical Models and Image Processing, 29(3): 273-285
 * M. Emre Celebi
 * 06.15.2007
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
class ITK_TEMPLATE_EXPORT RenyiEntropyThresholdCalculator : public HistogramThresholdCalculator<THistogram, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RenyiEntropyThresholdCalculator);

  /** Standard class type aliases. */
  using Self = RenyiEntropyThresholdCalculator;
  using Superclass = HistogramThresholdCalculator<THistogram, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RenyiEntropyThresholdCalculator, HistogramThresholdCalculator);

  /** Type definition for the input image. */
  using HistogramType = THistogram;
  using OutputType = TOutput;

protected:
  RenyiEntropyThresholdCalculator()
  {
    m_FirstBin = 0;
    m_LastBin = 0;
    m_Size = 0;
  }
  ~RenyiEntropyThresholdCalculator() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using TotalAbsoluteFrequencyType = typename HistogramType::TotalAbsoluteFrequencyType;
  using AbsoluteFrequencyType = typename HistogramType::AbsoluteFrequencyType;
  using InstanceIdentifier = typename HistogramType::InstanceIdentifier;
  using SizeValueType = typename HistogramType::SizeValueType;

  InstanceIdentifier
  MaxEntropyThresholding(const HistogramType *       histogram,
                         const std::vector<double> & normHisto,
                         const std::vector<double> & P1,
                         const std::vector<double> & P2);

  InstanceIdentifier
  MaxEntropyThresholding2(const HistogramType *       histogram,
                          const std::vector<double> & normHisto,
                          const std::vector<double> & P1,
                          const std::vector<double> & P2);

  InstanceIdentifier
  MaxEntropyThresholding3(const HistogramType *       histogram,
                          const std::vector<double> & normHisto,
                          const std::vector<double> & P1,
                          const std::vector<double> & P2);

private:
  InstanceIdentifier m_FirstBin;
  InstanceIdentifier m_LastBin;
  SizeValueType      m_Size;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRenyiEntropyThresholdCalculator.hxx"
#endif

#endif
