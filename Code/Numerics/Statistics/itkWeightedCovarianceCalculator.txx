/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedCovarianceCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeightedCovarianceCalculator_txx
#define __itkWeightedCovarianceCalculator_txx

#include "itkWeightedCovarianceCalculator.h"

namespace itk{ 
namespace Statistics{

template< class TSample >
WeightedCovarianceCalculator< TSample >
::WeightedCovarianceCalculator()
{
  m_Output = new OutputType() ;
  m_WeightFunction = 0 ;
  m_Weights = 0 ;
  m_Mean = 0 ;
  m_InternalMean = 0 ;
}

template< class TSample >
WeightedCovarianceCalculator< TSample >
::~WeightedCovarianceCalculator()
{
  if ( m_InternalMean != 0 )
    {
    delete m_InternalMean ;
    m_InternalMean = 0 ;
    m_Mean = 0 ;
    }
  delete m_Output ;
  m_Output = 0 ;
}


template< class TSample >
void
WeightedCovarianceCalculator< TSample >
::SetMean(MeanType* mean)
{
  if ( m_Mean != mean )
    {
    if ( m_InternalMean != mean && m_InternalMean != 0 )
      {
      delete m_InternalMean ;
      m_InternalMean = 0 ;
      }

    m_Mean = mean ;
    this->Modified() ;
    }
}

template< class TSample >
typename WeightedCovarianceCalculator< TSample >::MeanType*
WeightedCovarianceCalculator< TSample >
::GetMean()
{
  if ( m_InternalMean != 0 )
    {
    return m_InternalMean ;
    }
  else
    {
    return m_Mean ;
    }
} 

template< class TSample >
void
WeightedCovarianceCalculator< TSample >
::SetWeights(WeightArrayType* array)
{
  m_Weights = array ;
}

template< class TSample >
typename WeightedCovarianceCalculator< TSample >::WeightArrayType*
WeightedCovarianceCalculator< TSample >
::GetWeights()
{
  return m_Weights ;
}

template< class TSample >
void
WeightedCovarianceCalculator< TSample >
::SetWeightFunction(WeightFunctionType* func)
{
  m_WeightFunction = func ;
}

template< class TSample >
typename WeightedCovarianceCalculator< TSample >::WeightFunctionType*
WeightedCovarianceCalculator< TSample >
::GetWeightFunction()
{
  return m_WeightFunction ;
}

template< class TSample >
void
WeightedCovarianceCalculator< TSample >
::ComputeCovarianceWithGivenMean()
{
  m_Output->GetVnlMatrix().fill(0.0) ;
  double weight;
  double sumWeight = 0.0 ;
  double sumSquaredWeight = 0.0 ;
  
  unsigned int row, col ;
  unsigned int i ;
  typename TSample::Iterator iter = this->GetInputSample()->Begin() ; 
  typename TSample::Iterator end = this->GetInputSample()->End() ;
  MeanType diff ;
  typename TSample::MeasurementVectorType measurements ;
  int measurementVectorIndex = 0 ;
  // fills the lower triangle and the diagonal cells in the covariance matrix
  if (m_WeightFunction != 0) 
    {
    while (iter != end)
      {
      measurements = iter.GetMeasurementVector() ;
      weight = iter.GetFrequency() * m_WeightFunction->Evaluate(measurements) ;
      sumWeight += weight ;
      sumSquaredWeight += weight * weight ;
      for (i = 0 ; i < MeasurementVectorSize ; i++)
        {
        diff[i] = measurements[i] - (*m_Mean)[i] ;
        }
          
      for ( row = 0; row < MeasurementVectorSize ; row++)
        {
        for ( col = 0; col < row + 1 ; col++)
          {
          m_Output->GetVnlMatrix()(row,col) += 
            weight * diff[row] * diff[col] ;
          }
        }
      ++iter ;
      }
    }
  else
    {
    while (iter != end)
      {
      measurements = iter.GetMeasurementVector() ;
      weight = iter.GetFrequency() * (*m_Weights)[measurementVectorIndex] ;
      sumWeight += weight ;
      sumSquaredWeight += weight * weight ;
      for (i = 0 ; i < MeasurementVectorSize ; i++)
        {
        diff[i] = measurements[i] - (*m_Mean)[i] ;
        }
          
      for ( row = 0; row < MeasurementVectorSize ; row++)
        {
        for ( col = 0; col < row + 1 ; col++)
          {
          m_Output->GetVnlMatrix()(row,col) += 
            weight * diff[row] * diff[col] ;
          }
        }
      ++measurementVectorIndex ;
      ++iter ;
      } // end of while
    } // end of if-else

  // fills the upper triangle using the lower triangle  
  for (row = 1 ; row < MeasurementVectorSize ; row++)
    {
    for (col = 0 ; col < row ; col++)
      {
      m_Output->GetVnlMatrix()(col, row) = 
        m_Output->GetVnlMatrix()(row, col) ;
      } 
    }
  
  m_Output->GetVnlMatrix() /= 
    (sumWeight - (sumSquaredWeight / sumWeight) )  ;
}

template< class TSample >
void
WeightedCovarianceCalculator< TSample >
::ComputeCovarianceWithoutGivenMean()
{
  m_Output->GetVnlMatrix().fill(0.0) ;
  m_InternalMean->Fill(0.0) ;
  double weight;
  double tempWeight ;
  double sumWeight = 0.0 ;
  double sumSquaredWeight = 0.0 ;
  
  unsigned int row, col ;
  unsigned int i ;
  typename TSample::Iterator iter = this->GetInputSample()->Begin() ; 
  typename TSample::Iterator end = this->GetInputSample()->End() ;
  MeanType diff ;
  typename TSample::MeasurementVectorType measurements ;
  int measurementVectorIndex = 0 ;
  // fills the lower triangle and the diagonal cells in the covariance matrix
  if (m_WeightFunction != 0) 
    {
    while (iter != end)
      {
      measurements = iter.GetMeasurementVector() ;
      weight = 
        iter.GetFrequency() * m_WeightFunction->Evaluate(measurements) ;
      sumWeight += weight ;
      sumSquaredWeight += weight * weight ;
      for (i = 0 ; i < MeasurementVectorSize ; i++)
        {
        diff[i] = measurements[i] - (*m_InternalMean)[i] ;
        }
          
      // updates the mean vector
      tempWeight = weight / sumWeight ;
      for ( i = 0 ; i < MeasurementVectorSize ; ++i )
        {
        (*m_InternalMean)[i] += tempWeight * diff[i] ;
        }

      tempWeight = tempWeight * ( sumWeight - weight ) ;
      for ( row = 0; row < MeasurementVectorSize ; row++)
        {
        for ( col = 0; col < row + 1 ; col++)
          {
          m_Output->GetVnlMatrix()(row,col) += 
            tempWeight * diff[row] * diff[col] ;
          }
        }
      ++iter ;
      }
    }
  else
    {
    while (iter != end)
      {
      weight = iter.GetFrequency() * (*m_Weights)[measurementVectorIndex] ;
      sumWeight += weight ;
      measurements = iter.GetMeasurementVector() ;
      sumSquaredWeight += weight * weight ;
      for (i = 0 ; i < MeasurementVectorSize ; i++)
        {
        diff[i] = measurements[i] - (*m_InternalMean)[i] ;
        }
          
      // updates the mean vector
      tempWeight = weight / sumWeight ;
      for ( i = 0 ; i < MeasurementVectorSize ; ++i )
        {
        (*m_InternalMean)[i] += tempWeight * diff[i] ;
        }

      tempWeight = tempWeight * ( sumWeight - weight ) ;
      for ( row = 0; row < MeasurementVectorSize ; row++)
        {
        for ( col = 0; col < row + 1 ; col++)
          {
          m_Output->GetVnlMatrix()(row,col) += 
            tempWeight * diff[row] * diff[col] ;
          }
        }
      ++measurementVectorIndex ;
      ++iter ;
      }
    }

  // fills the upper triangle using the lower triangle  
  for (row = 1 ; row < MeasurementVectorSize ; row++)
    {
    for (col = 0 ; col < row ; col++)
      {
      m_Output->GetVnlMatrix()(col, row) = 
        m_Output->GetVnlMatrix()(row, col) ;
      } 
    }

  m_Output->GetVnlMatrix() /= 
    (sumWeight - (sumSquaredWeight / sumWeight) )  ;
}

template< class TSample >
void
WeightedCovarianceCalculator< TSample >
::GenerateData() 
{
  if ( m_Mean == 0 )
    {
    m_InternalMean = new MeanType() ;
    this->ComputeCovarianceWithoutGivenMean() ;
    }
  else
    {
    this->ComputeCovarianceWithGivenMean() ;
    }
}

template< class TSample >
typename WeightedCovarianceCalculator< TSample >::OutputType*
WeightedCovarianceCalculator< TSample >
::GetOutput()
{
  return m_Output ;
}

template< class TSample >
void
WeightedCovarianceCalculator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Output: " << m_Output << std::endl;
  os << indent << "Weights: " << m_Weights << std::endl;

  if ( m_Mean != 0)
    {
    os << indent << "Mean: [" << *m_Mean << "]" << std::endl ;
    }
  else
    {
    os << indent << "Mean: not set" << std::endl ;
    }

  if ( m_InternalMean != 0)
    {
    os << indent << "Internal Mean: [" << *m_InternalMean << "]" << std::endl ;
    }
  else
    {
    os << indent << "Internal Mean: not used" << std::endl ;
    }
}

} // end of namespace Statistics 
} // end of namespace itk

#endif

