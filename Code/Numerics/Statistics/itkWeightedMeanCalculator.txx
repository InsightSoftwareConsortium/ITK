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
  m_WeightFunction = 0 ;
  m_Weights = 0 ;
}

template< class TSample >
void
WeightedMeanCalculator< TSample >
::SetWeights(WeightArrayType* array)
{
  m_Weights = array ;
}

template< class TSample >
typename WeightedMeanCalculator< TSample >::WeightArrayType*
WeightedMeanCalculator< TSample >
::GetWeights()
{
  return m_Weights ;
}

template< class TSample >
void
WeightedMeanCalculator< TSample >
::SetWeightFunction(WeightFunctionType* func)
{
  m_WeightFunction = func ;
}

template< class TSample >
typename WeightedMeanCalculator< TSample >::WeightFunctionType*
WeightedMeanCalculator< TSample >
::GetWeightFunction()
{
  return m_WeightFunction ;
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
  double weight ;
  unsigned int dim ;
  int measurementVectorIndex = 0 ;
  typename TSample::MeasurementVectorType measurements ;

  if (m_WeightFunction != 0) 
    {
      while (iter != end)
        {
          measurements = iter.GetMeasurementVector() ;
          weight = 
            iter.GetFrequency() * m_WeightFunction->Evaluate(measurements) ;
          totalWeight += weight ;
          for (dim = 0 ; dim < MeasurementVectorSize ; dim++)
            {
              m_Output[dim] += measurements[dim] * weight ;
            }
          ++iter ;
        }
      m_Output /= totalWeight ;
    }
  else
    {
      while (iter != end)
        {
          measurements = iter.GetMeasurementVector() ;
          weight = iter.GetFrequency() * (*m_Weights)[measurementVectorIndex] ;
          totalWeight += weight ;
          for (dim = 0 ; dim < MeasurementVectorSize ; dim++)
            {
              m_Output[dim] += measurements[dim] * weight ;
            }
          ++measurementVectorIndex ;
          ++iter ;
        }

      m_Output /= totalWeight ;
    }
}

template< class TSample >
typename WeightedMeanCalculator< TSample >::OutputType*
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

