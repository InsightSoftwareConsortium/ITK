/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogramMean.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkStatHistogramMean_h
#define __itkStatHistogramMean_h

#include <vector>
#include <itkLightObject.h>

namespace itk{

/** \class HistogramMean 
 *  \brief This class calculates the mean of a histogram
 *
 */

template <class TMean, class THistogram>
class ITK_EXPORT HistogramMean : public LightObject
{
public:

 /**
  * Standard "Self" typedef.
  */
  typedef HistogramMean  Self;

 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  * Method for creation through the object factory.
  */
  itkNewMacro(Self);

 /**
  * Method to set histogram
  */
  void SetHistogram(THistogram* histogram) { m_Histogram = histogram; };

 /**
  * Method to get mean
  */
  double GetMean(int i) { return m_Mean[i]; };

 /**
  * Method to get mean
  */
  std::vector<TMean> GetMean() { return m_Mean; };

 /**
  * Calculate the mean of a histogram
  */
  void Execute();

private:
  typename THistogram::Pointer m_Histogram;
  std::vector<TMean> m_Mean;

};

}; // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatHistogramMean.txx"
#endif

#endif
