/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedMeanCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeightedMeanCalculator_txx
#define __itkWeightedMeanCalculator_txx

namespace itk{ 
namespace Statistics{

template< class TSample >
WeightedMeanCalculator< TSample >
::WeightedMeanCalculator()
{
}

template< class TSample >
void
WeightedMeanCalculator< TSample >
::SetWeights(WeightArrayType* array)
{
  m_Weights = array ;
}

template< class TSample >
WeightedMeanCalculator< TSample >::WeightArrayType*
WeightedMeanCalculator< TSample >
::GetWeights()
{
  return m_Weights ;
}

template< class TSample >
void
WeightedMeanCalculator< TSample >
::GenerateData() 
{
  m_Output.Fill(0.0) ;

  typename TSample::Iterator iter = this->GetInputSample()->Begin() ;
  typename TSample::Iterator end = this->GetInputSample()->End() ;
  double totalWeight = 0.0 ;
  double weight = 0.0 ;
  unsigned int dim = 0 ;
  int measurementVectorIndex = 0 ;

  while (iter != end)
    {
      weight = iter.GetFrequency() * (*m_Weights)[measurementVectorIndex] ;
      totalWeight += weight ;
      for (dim = 0 ; dim < MeasurementVectorSize ; dim++)
        {
          m_Output[dim] += iter.GetMeasurementVector()[dim] * weight ;
        }
      ++measurementVectorIndex ;
      ++iter ;
    }

  m_Output /= totalWeight ;
}

template< class TSample >
WeightedMeanCalculator< TSample >::OutputType*
WeightedMeanCalculator< TSample >
::GetOutput()
{
  return &m_Output ;
}

template< class TSample >
void
WeightedMeanCalculator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  std::cout << "Output: " << m_Output << std::endl ;
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif

