/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogramStandardDeviation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkStatHistogramStandardDeviation_h
#define __itkStatHistogramStandardDeviation_h

#include <vector>
#include <itkLightObject.h>

namespace itk{

/** \class HistogramStandardDeviation 
 *  \brief This class calculates the standard deviation of a histogram
 *
 */

template <class TStd, class THistogram>
class ITK_EXPORT HistogramStandardDeviation : public LightObject
{
public:

 /**
  * Standard "Self" typedef.
  */
  typedef HistogramStandardDeviation  Self;

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
  double GetStandardDeviation(int i) { return m_Std[i]; };

 /**
  * Method to get mean
  */
  std::vector<TStd> GetStandardDeviation() { return m_Std; };

 /**
  * Calculate the mean of a histogram
  */
  void Execute();

private:
  typename THistogram::Pointer m_Histogram;
  std::vector<double> m_Std;

};

}; // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatHistogramStandardDeviation.txx"
#endif

#endif
