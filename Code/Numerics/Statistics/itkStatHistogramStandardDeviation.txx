/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogramStandardDeviation.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef _itkStatHistogramStandardDeviation_txx
#define _itkStatHistogramStandardDeviation_txx

#include "itkStatHistogramStandardDeviation.h"
#include "itkStatHistogramMean.h"

namespace itk
{

template <class TStd, class THistogram>
void
HistogramStandardDeviation<TStd, THistogram>
::Execute()
{
  unsigned int dimension = THistogram::Dimension;

  vnl_vector< TStd > meanVec; 
  
  // allocate m_Std vector
  m_Stds.resize(dimension);
  meanVec.resize(dimension);

  int i = 0;

  // initialize m_Std vector
  for ( i=0; i < dimension; i++ )
    {
    m_Stds[i]  = 0;
    meanVec[i] = 0;
    }

  // frequency 
  TStd frequency = 0;

  // sum of frequncy
  TStd sum = 0;

  typedef HistogramMean<TStd, THistogram> HistogramMeanType;
  HistogramMeanType::Pointer mean = HistogramMeanType::New();
  mean->SetHistogram(m_Histogram);
  mean->Execute();
  meanVec = mean->GetMeans();
 
  typename THistogram::Iterator it(m_Histogram);
 
  it = it.Begin();
  while ( !it.IsAtEnd() )
    {
    frequency = it.GetFrequency();
    sum = sum + frequency;
    for ( i=0; i < dimension; i++)
      {
      m_Stds[i] = m_Stds[i] + frequency*pow(it.GetFeature(i) - meanVec[i],2);
      }
    ++it;
    }
 
  for ( i=0; i < dimension; i++)
    {
    m_Stds[i] = m_Stds[i]/sum;
    m_Stds[i] = sqrt(m_Stds[i]);
    }
};

} // end of namespace

#endif
