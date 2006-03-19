/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKullbackLeiblerCompareHistogramImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKullbackLeiblerCompareHistogramImageToImageMetric_txx
#define __itkKullbackLeiblerCompareHistogramImageToImageMetric_txx

#include "itkKullbackLeiblerCompareHistogramImageToImageMetric.h"
#include "itkHistogram.h"

// Todo: need to access Use_Padding in parent. Make in protected
// need to figure out what to do when "stuff" is not in histogram
// kernel function?

namespace itk
{
template <class TFixedImage, class TMovingImage>
KullbackLeiblerCompareHistogramImageToImageMetric<TFixedImage, TMovingImage>::
KullbackLeiblerCompareHistogramImageToImageMetric() 
{
  m_Epsilon                = 1e-12; // should be smaller than 1/numBins^2
}

template <class TFixedImage, class TMovingImage>
void 
KullbackLeiblerCompareHistogramImageToImageMetric<TFixedImage, TMovingImage>
::Initialize()  throw (ExceptionObject)
{
  Superclass::Initialize();
}

template <class TFixedImage, class TMovingImage>
typename KullbackLeiblerCompareHistogramImageToImageMetric<TFixedImage, \
                                                           TMovingImage>::MeasureType
KullbackLeiblerCompareHistogramImageToImageMetric<TFixedImage, \
                                                  TMovingImage>
::EvaluateMeasure(HistogramType& histogram) const
{
  // Two terms.
  // First the term that measures the entropy of the term
  // p(x,y) log p(x,y) - p(x,y) log q(x,y)

  MeasureType    KullbackLeibler = NumericTraits<MeasureType>::Zero;

  HistogramIteratorType measured_it   = histogram.Begin();
  HistogramIteratorType measured_end  = histogram.End();

  HistogramIteratorType training_it   = this->GetTrainingHistogram()->Begin();
  HistogramIteratorType training_end  = this->GetTrainingHistogram()->End();

  while (measured_it != measured_end)
    {
    // Every bin gets epsilon added to it
    double TrainingFreq = training_it.GetFrequency()+m_Epsilon;
    double MeasuredFreq = measured_it.GetFrequency()+m_Epsilon;

    KullbackLeibler += MeasuredFreq*vcl_log(MeasuredFreq/TrainingFreq);

    ++measured_it;
    ++training_it;
    }

  if (training_it != training_end)
    itkWarningMacro("The Measured and Training Histograms have different number of bins.");

  // Get the total frequency for each histogram.
  HistogramFrequencyType totalTrainingFreq = this->GetTrainingHistogram()->GetTotalFrequency();
  HistogramFrequencyType totalMeasuredFreq = histogram.GetTotalFrequency();

  // The actual number of total frequency is a bit larger
  // than the number of counts because we add m_Epsilon to every bin
  double AdjustedTotalTrainingFreq = totalTrainingFreq +
    this->GetHistogramSize()[0]*this->GetHistogramSize()[1]*m_Epsilon;
  double AdjustedTotalMeasuredFreq = totalMeasuredFreq +
    this->GetHistogramSize()[0]*this->GetHistogramSize()[1]*m_Epsilon;

  KullbackLeibler = KullbackLeibler/static_cast<MeasureType>(AdjustedTotalMeasuredFreq)
    - vcl_log(AdjustedTotalMeasuredFreq/AdjustedTotalTrainingFreq);

  return KullbackLeibler;
}

template <class TFixedImage, class TMovingImage>
void KullbackLeiblerCompareHistogramImageToImageMetric<TFixedImage, TMovingImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Epsilon: " << m_Epsilon << std::endl;
}


} // End namespace itk

#endif // itkKullbackLeiblerCompareHistogramImageToImageMetric_txx
