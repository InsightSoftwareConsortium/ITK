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
}

template< class TSample >
WeightedCovarianceCalculator< TSample >
::~WeightedCovarianceCalculator()
{
  delete m_Output ;
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
::GenerateData() 
{
  m_Output->GetVnlMatrix().fill(0.0) ;
  double weight = 0.0 ;
  double totalWeight = 0.0 ;
  
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
          totalWeight += weight ;
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
          weight = iter.GetFrequency() * (*m_Weights)[measurementVectorIndex] ;
          totalWeight += weight ;
          measurements = iter.GetMeasurementVector() ;
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

  // fills the upper triangle using the lower triangle  
  for (row = 1 ; row < MeasurementVectorSize ; row++)
    {
      for (col = 0 ; col < row ; col++)
        {
          m_Output->GetVnlMatrix()(col, row) = 
            m_Output->GetVnlMatrix()(row, col) ;
        } 
    }
  
  m_Output->GetVnlMatrix() /= totalWeight ;
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
  os << indent << "Mean: " << m_Mean << std::endl ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif

