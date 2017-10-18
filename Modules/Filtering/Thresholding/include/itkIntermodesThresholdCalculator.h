/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#ifndef itkIntermodesThresholdCalculator_h
#define itkIntermodesThresholdCalculator_h

#include "itkHistogramThresholdCalculator.h"

namespace itk
{

/** \class IntermodesThresholdCalculator
 * \brief Computes the Intermodes's threshold for an image.
 *
 * J. M. S. Prewitt and M. L. Mendelsohn, "The analysis of cell images," in
 * Annals of the New York Academy of Sciences, vol. 128, pp. 1035-1053, 1966.
 *  *
 * Assumes a bimodal histogram. The histogram needs is smoothed (using a
 * running average of size 3, iteratively) until there are only two local maxima.
 * j and k
 * Threshold t is (j+k)/2.
 * Images with histograms having extremely unequal peaks or a broad and
 * flat valley are unsuitable for this method.
 *
 * This class is templated over the input histogram type.
 * \warning This calculator assumes that the input histogram has only one dimension.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.

 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/10380/3279  or
 * http://www.insight-journal.org/browse/publication/811
 *
 * \ingroup Operators
 * \ingroup ITKThresholding
 */
template <typename THistogram, typename TOutput=double>
class ITK_TEMPLATE_EXPORT IntermodesThresholdCalculator : public HistogramThresholdCalculator<THistogram, TOutput>
{
public:
  /** Standard class typedefs. */
  typedef IntermodesThresholdCalculator                     Self;
  typedef HistogramThresholdCalculator<THistogram, TOutput> Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IntermodesThresholdCalculator, HistogramThresholdCalculator);

  /** Type definition for the input image. */
  typedef THistogram  HistogramType;
  typedef TOutput     OutputType;

  typedef typename HistogramType::InstanceIdentifier          InstanceIdentifier;

  itkSetMacro( MaximumSmoothingIterations, SizeValueType );
  itkGetConstMacro( MaximumSmoothingIterations, SizeValueType );

  /** Select whether midpoint (intermode = true) or minimum between
  peaks is used. Default is "On". */
  itkSetMacro( UseInterMode, bool);
  itkGetConstMacro( UseInterMode, bool );
  itkBooleanMacro( UseInterMode );

protected:
  IntermodesThresholdCalculator()
  {
    m_MaximumSmoothingIterations = 10000;
    m_UseInterMode = true;
  }

  virtual ~IntermodesThresholdCalculator() ITK_OVERRIDE {}

  void GenerateData(void) ITK_OVERRIDE;
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  typedef typename HistogramType::TotalAbsoluteFrequencyType  TotalAbsoluteFrequencyType;
  typedef typename HistogramType::AbsoluteFrequencyType       AbsoluteFrequencyType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IntermodesThresholdCalculator);
  bool BimodalTest(const std::vector<double> & h);

  SizeValueType m_MaximumSmoothingIterations;
  bool          m_UseInterMode;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIntermodesThresholdCalculator.hxx"
#endif

#endif
