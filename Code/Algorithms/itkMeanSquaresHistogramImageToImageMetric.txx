/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresHistogramImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanSquaresHistogramImageToImageMetric_txx
#define __itkMeanSquaresHistogramImageToImageMetric_txx

#include "itkMeanSquaresHistogramImageToImageMetric.h"

namespace itk
{
  template <class TFixedImage, class TMovingImage>
  typename MeanSquaresHistogramImageToImageMetric<TFixedImage, TMovingImage>
  ::MeasureType
  MeanSquaresHistogramImageToImageMetric<TFixedImage, TMovingImage>
  ::EvaluateMeasure(HistogramType& histogram) const
  {
    MeasureType measure = NumericTraits<MeasureType>::Zero;
    HistogramIteratorType it = histogram.Begin();
    HistogramIteratorType end = histogram.End();
    HistogramFrequencyType totalNoOfSamples =
      NumericTraits<HistogramFrequencyType>::Zero;

    while (it != end)
    {
      HistogramFrequencyType freq = it.GetFrequency();
      if (freq > 0)
      {
        HistogramMeasurementVectorType value = it.GetMeasurementVector();
        measure += (value[0] - value[1])*(value[0] - value[1])*freq;
        totalNoOfSamples += freq;
      }
      ++it;
    }

    measure /= totalNoOfSamples;

    return measure;
  }
} // End namespace itk

#endif // itkMeanSquaresHistogramImageToImageMetric_txx
