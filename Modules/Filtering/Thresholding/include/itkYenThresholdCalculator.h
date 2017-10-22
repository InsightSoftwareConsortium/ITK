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

#ifndef itkYenThresholdCalculator_h
#define itkYenThresholdCalculator_h

#include "itkHistogramThresholdCalculator.h"

namespace itk
{

/** \class YenThresholdCalculator
 * \brief Computes the Yen's threshold for an image.
 *
 * Implements Yen  thresholding method
 * 1) Yen J.C., Chang F.J., and Chang S. (1995) "A New Criterion
 *    for Automatic Multilevel Thresholding" IEEE Trans. on Image
 *    Processing, 4(3): 370-378
 * 2) Sezgin M. and Sankur B. (2004) "Survey over Image Thresholding
 *    Techniques and Quantitative Performance Evaluation" Journal of
 *    Electronic Imaging, 13(1): 146-165
 *    http://citeseer.ist.psu.edu/sezgin04survey.html
 *
 * M. Emre Celebi
 * 06.15.2007
 *
 * This class is templated over the input histogram type.
 * \warning This calculator assumes that the input histogram has only one dimension.
 *
 * \author Richard Beare
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
class ITK_TEMPLATE_EXPORT YenThresholdCalculator : public HistogramThresholdCalculator<THistogram, TOutput>
{
public:
  /** Standard class typedefs. */
  typedef YenThresholdCalculator                            Self;
  typedef HistogramThresholdCalculator<THistogram, TOutput> Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(YenThresholdCalculator, HistogramThresholdCalculator);

  /** Type definition for the input image. */
  typedef THistogram  HistogramType;
  typedef TOutput     OutputType;

protected:
  YenThresholdCalculator() {};
  virtual ~YenThresholdCalculator() ITK_OVERRIDE {};
  void GenerateData(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(YenThresholdCalculator);

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkYenThresholdCalculator.hxx"
#endif

#endif
