/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatHistogramCovariance.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef _itkStatHistogramCovariance_txx
#define _itkStatHistogramCovariance_txx

#include "itkStatHistogramCovariance.h"
#include "itkStatHistogramMean.h"

namespace itk
{

template <class TCovariance, class THistogram>
void
HistogramCovariance<TCovariance, THistogram>
::Execute()
{

  unsigned int dimension = THistogram::Dimension;

  m_Covariance.resize(dimension, dimension);

  vnl_vector<TCovariance> meanVec;
  meanVec.resize(dimension);
  typedef HistogramMean<TCovariance, THistogram> HistogramMeanType;
  HistogramMeanType::Pointer mean =  HistogramMeanType::New();
 
  mean->SetHistogram(m_Histogram);
  mean->Execute();
  meanVec = mean->GetMeans();

  // frequency 
  float frequency = 0;

  // sum of frequncy
  float sum = 0;

  int row, col;

  //initialize
  for ( int row = 0; row < dimension; row++)
    {
    for ( int col = 0; col < dimension; col++)
      {
      m_Covariance.put(row,col,0);
      }
    }

  double temp;
  typename THistogram::Iterator it(m_Histogram);
  it = it.Begin();
  while ( !it.IsAtEnd() )
    {
    frequency = it.GetFrequency();
    sum = sum + frequency;
    for ( row = 0; row < dimension; row++)
      {
      for ( col = 0; col < dimension; col++)
        {
        temp = frequency*(it.GetFeature(row) - meanVec[row])
                        *(it.GetFeature(col) - meanVec[col]);
        m_Covariance(row,col) = m_Covariance(row,col) + temp;
        }
      }    
    ++it;
    }
  for ( row = 0; row < dimension; row++)
    {
    for ( col = 0; col < dimension; col++)
      {
      m_Covariance(row,col) = (TCovariance)(m_Covariance(row,col)/sum);
      }
    }    
};

} // end of namespace

#endif
