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

#ifndef __itkOtsuThresholdCalculator_h
#define __itkOtsuThresholdCalculator_h

#include "itkHistogramThresholdCalculator.h"
#include "itkOtsuMultipleThresholdsCalculator.h"

namespace itk
{

/** \class OtsuThresholdCalculator
 * \brief Computes the Otsu's threshold for an image.
 *
 * This calculator computes the Otsu's threshold which separates an image
 * into foreground and background components. The method relies on a
 * histogram of image intensities. The basic idea is to maximize the
 * between-class variance.
 *
 * This class is templated over the input histogram type.
 *
 * \warning This calculator assumes that the input histogram has only one dimension.
 *
 * \ingroup Operators
 * \ingroup ITKThresholding
 */
template <typename THistogram, typename TOutput=double>
class OtsuThresholdCalculator : public HistogramThresholdCalculator<THistogram, TOutput>
{
public:
  /** Standard class typedefs. */
  typedef OtsuThresholdCalculator         Self;
  typedef Object                          Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(OtsuThresholdCalculator, Object);

  /** Type definition for the input image. */
  typedef THistogram  HistogramType;
  typedef TOutput     OutputType;

  /** for backward compatibility. Update() should be preferred. */
  void Compute()
  {
    this->Update();
  }

protected:
  OtsuThresholdCalculator()
  {
    m_OtsuMultipleThresholdsCalculator = OtsuMultipleThresholdsCalculator<THistogram>::New();
  }
  virtual ~OtsuThresholdCalculator() {};
  void GenerateData(void);

private:
  OtsuThresholdCalculator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  typename OtsuMultipleThresholdsCalculator<THistogram>::Pointer m_OtsuMultipleThresholdsCalculator;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOtsuThresholdCalculator.hxx"
#endif

#endif
