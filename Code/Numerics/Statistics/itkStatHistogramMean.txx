/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogramMean.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef _itkStatHistogramMean_txx
#define _itkStatHistogramMean_txx

#include "itkStatHistogramMean.h"

namespace itk
{

template <class TMean, class THistogram>
void
HistogramMean<TMean, THistogram>::Execute()
{
  unsigned int dimension = THistogram::Dimension;

  // allocate m_Mean vector
  m_Mean.resize(dimension);

  int i = 0;

  // initialize m_Mean vector
  for ( i=0; i < dimension; i++ )
    {
    m_Mean[i] = 0;
    }

  // frequency 
  float frequency = 0;

  // sum of frequncy
  float sum = 0;

  typename THistogram::Iterator it(m_Histogram);

  it = it.Begin();
  while ( !it.IsAtEnd() )
    {
    frequency = it.GetFrequency();
    sum = sum + frequency;
    for ( i=0; i < dimension; i++)
      {
      m_Mean[i] = m_Mean[i] + it.GetFeature(i)*frequency;
      }
    ++it;
    }
 
  for ( i=0; i < dimension; i++)
    {
    m_Mean[i] = m_Mean[i]/sum;
    }
  
};

} // end of namespace

#endif
