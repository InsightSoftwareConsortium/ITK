/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogramCovariance.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkStatHistogramCovariance_h
#define __itkStatHistogramCovariance_h

#include <vector>
#include <vnl/vnl_matrix.h>
#include <itkLightObject.h>

namespace itk{

/** \class HistogramCovariance 
 *  \brief This class calculates the covariance matrix of a histogram
 *
 */

template <class TCovariance, class THistogram>
class ITK_EXPORT HistogramCovariance : public LightObject
{
public:

 /**
  * Standard "Self" typedef.
  */
  typedef HistogramCovariance  Self;

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
  * Method to get covariance matrix
  */
  TCovariance GetCovariance(int r, int c) { return m_Covariance(r,c); };

 /**
  * Method to get covariance matrix
  */
  vnl_matrix<TCovariance> GetCovariance() { return m_Covariance; };

 /**
  * Calculate the mean of a histogram
  */
  void Execute();

protected:
  HistogramCovariance(){};
  virtual ~HistogramCovariance() {};
  HistogramCovariance(const Self&) {};
  void operator=(const Self&) {};

private:
  typename THistogram::Pointer m_Histogram;
  vnl_matrix<TCovariance> m_Covariance;

};

}; // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatHistogramCovariance.txx"
#endif

#endif
