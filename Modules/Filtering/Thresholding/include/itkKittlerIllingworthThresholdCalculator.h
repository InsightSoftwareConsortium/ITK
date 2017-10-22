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

#ifndef itkKittlerIllingworthThresholdCalculator_h
#define itkKittlerIllingworthThresholdCalculator_h

#include "itkHistogramThresholdCalculator.h"

namespace itk
{

/** \class KittlerIllingworthThresholdCalculator
 * \brief Computes the KittlerIllingworth's threshold for an image.
 *
 * Kittler and J. Illingworth, "Minimum error thresholding," Pattern Recognition, vol. 19, pp. 41-47, 1986.
 * C. A. Glasbey, "An analysis of histogram-based thresholding algorithms," CVGIP: Graphical Models and Image Processing, vol. 55, pp. 532-537, 1993.
 * Original Matlab code Copyright (C) 2004 Antti Niemisto
 * See http://www.cs.tut.fi/~ant/histthresh/ for an excellent slide presentation
 * and the original Matlab code.
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
class ITK_TEMPLATE_EXPORT KittlerIllingworthThresholdCalculator : public HistogramThresholdCalculator<THistogram, TOutput>
{
public:
  /** Standard class typedefs. */
  typedef KittlerIllingworthThresholdCalculator             Self;
  typedef HistogramThresholdCalculator<THistogram, TOutput> Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KittlerIllingworthThresholdCalculator, HistogramThresholdCalculator);

  /** Type definition for the input image. */
  typedef THistogram  HistogramType;
  typedef TOutput     OutputType;

protected:
  KittlerIllingworthThresholdCalculator() {}
  virtual ~KittlerIllingworthThresholdCalculator() ITK_OVERRIDE {}

  void GenerateData(void) ITK_OVERRIDE;

  typedef typename HistogramType::TotalAbsoluteFrequencyType  TotalAbsoluteFrequencyType;
  typedef typename HistogramType::AbsoluteFrequencyType       AbsoluteFrequencyType;
  typedef typename HistogramType::InstanceIdentifier          InstanceIdentifier;
  typedef typename HistogramType::SizeValueType               SizeValueType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KittlerIllingworthThresholdCalculator);

  IndexValueType Mean();
  double A( InstanceIdentifier j);
  double B( InstanceIdentifier j);
  double C( InstanceIdentifier j);

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKittlerIllingworthThresholdCalculator.hxx"
#endif

#endif
