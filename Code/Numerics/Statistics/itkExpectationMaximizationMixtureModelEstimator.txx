/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExpectationMaximizationMixtureModelEstimator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExpectationMaximizationMixtureModelEstimator_txx
#define __itkExpectationMaximizationMixtureModelEstimator_txx

#include "itkExpectationMaximizationMixtureModelEstimator.h"

namespace itk{ 
namespace Statistics{
  
template< class TSample >
ExpectationMaximizationMixtureModelEstimator< TSample >
::ExpectationMaximizationMixtureModelEstimator()
{
  m_TerminationCode = NOT_CONVERGED ;
}
 
template< class TSample >
void
ExpectationMaximizationMixtureModelEstimator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent) ;
}


template< class TSample >
void
ExpectationMaximizationMixtureModelEstimator< TSample >
::SetMaximumIteration(int numberOfIterations) 
{
  m_MaxIteration = numberOfIterations ;
}

template< class TSample >
int
ExpectationMaximizationMixtureModelEstimator< TSample >
::GetMaximumIteration() 
{
  return m_MaxIteration ;
}

template< class TSample >
void
ExpectationMaximizationMixtureModelEstimator< TSample >
::SetInitialProportions(ProportionVectorType &proportions) 
{
  m_InitialProportions = proportions ;
}

template< class TSample >
ExpectationMaximizationMixtureModelEstimator< TSample >::ProportionVectorType*
ExpectationMaximizationMixtureModelEstimator< TSample >
::GetInitialProportions() 
{
  return m_InitialProportions ;
}

template< class TSample >
ExpectationMaximizationMixtureModelEstimator< TSample >::ProportionVectorType*
ExpectationMaximizationMixtureModelEstimator< TSample >
::GetProportions() 
{
  return &m_Proportions ;
}

template< class TSample >
void
ExpectationMaximizationMixtureModelEstimator< TSample >
::SetSample(SamplePointer sample) 
{
  m_Sample = sample ;
}

template< class TSample >
int
ExpectationMaximizationMixtureModelEstimator< TSample >
::AddComponent(ComponentPointer component)
{
  m_ComponentVector.push_back(component) ;
  //component->SetSample(m_Sample) ;
}

template< class TSample >
int
ExpectationMaximizationMixtureModelEstimator< TSample >
::GetNumberOfComponents()
{
  return m_ComponentVector.size() ;
}

template< class TSample >
ExpectationMaximizationMixtureModelEstimator< TSample >::TERMINATION_CODE
ExpectationMaximizationMixtureModelEstimator< TSample >
::GetTerminationCode()
{
  return m_TerminationCode ;
}

template< class TSample >
ExpectationMaximizationMixtureModelEstimator< TSample >::ComponentMembershipFunctionPointer 
ExpectationMaximizationMixtureModelEstimator< TSample >
::GetComponentMembershipFunction(int componentIndex)
{
  return (m_ComponentVector[componentIndex])->GetMembershipFunction() ;
}


template< class TSample >
bool
ExpectationMaximizationMixtureModelEstimator< TSample >
::CalculateDensities()
{
  bool componentModified  = false ;

  for (int i = 0 ; i < m_ComponentVector.size() ; i++)
    {
      if ( (m_ComponentVector[i])->AreParametersModified() )
        {
          componentModified = true ;
        }
    }

  if (!componentModified)
    {
      return false ;
    }

  double temp ;
  int numberOfComponents = m_ComponentVector.size() ;
  std::vector< double > tempWeights(numberOfComponents) ;

  typename TSample::Iterator iter = m_Sample->Begin() ;
  typename TSample::Iterator last = m_Sample->End() ;

  int componentIndex ;

  typename TSample::FrequencyType frequency ;
  typename TSample::FrequencyType zeroFrequency =
    NumericTraits< typename TSample::FrequencyType >::Zero ;
  typename TSample::MeasurementVectorType mvector ;
  double density ;
  double densitySum ;
  double minDouble = NumericTraits< double >::min() ;

  long measurementVectorIndex = 0 ;
  while (iter != last)
    {
      mvector = iter.GetMeasurementVector() ;
      frequency = iter.GetFrequency() ;
      densitySum = 0.0 ;
      if ( frequency > zeroFrequency )
        {
          for (componentIndex = 0 ; componentIndex < numberOfComponents ; 
               componentIndex++)
            {
              density = m_Proportions[componentIndex] *
                m_ComponentVector[componentIndex]->Evaluate(mvector) ;
              tempWeights[componentIndex] = density ;
              densitySum += density ;
            }
          
          for (componentIndex = 0 ; componentIndex < numberOfComponents ; 
               componentIndex++)
            {
              temp = tempWeights[componentIndex] ;
              temp /= densitySum ;
              m_ComponentVector[componentIndex]->SetWeight(measurementVectorIndex,
                                                           temp) ; 
            }
        }
      else
        {
          for (componentIndex = 0 ; componentIndex < numberOfComponents ; 
               componentIndex++)
            {
              m_ComponentVector[componentIndex]->SetWeight(measurementVectorIndex,
                                                           minDouble) ; 
            }
        }

      ++iter ;
      ++measurementVectorIndex ;
    }
  
  return true ;
}

template< class TSample >
double
ExpectationMaximizationMixtureModelEstimator< TSample >
::CalculateExpectation()
{
  int componentIndex, measurementVectorIndex ;
  long size = m_Sample->Size() ;
  double logProportion ;
  double sum = 0.0 ;
  double temp = 0.0 ;
  for (componentIndex = 0 ; componentIndex < m_ComponentVector.size() ;
       componentIndex++)
    {
      logProportion = log(m_Proportions[componentIndex]) ; 
      for (measurementVectorIndex = 0 ; measurementVectorIndex < size ;
           measurementVectorIndex++)
        {
          temp = m_ComponentVector[componentIndex]->
            GetWeight(measurementVectorIndex) ;
          sum += 
            temp * ( logProportion + 
                     log( m_ComponentVector[componentIndex]->
                          GetWeight(measurementVectorIndex) ) ) ;
        }
    }
  return sum ;
}

template< class TSample >
bool
ExpectationMaximizationMixtureModelEstimator< TSample >
::UpdateComponentParameters()
{
  int componentIndex ;
  bool updated = false ;
  ComponentPointer component ;

  for (componentIndex = 0 ; componentIndex < m_ComponentVector.size() ;
       componentIndex++)
    {
      component = m_ComponentVector[componentIndex] ;
      component->Update() ;
      if (component->AreParametersModified())
        {
          updated = true ;
        }
    }

  return updated ;
}

template< class TSample >
bool
ExpectationMaximizationMixtureModelEstimator< TSample >
::UpdateProportions()
{
  int numberOfComponents = m_ComponentVector.size() ;
  long sampleSize = m_Sample->Size() ;
  double totalFrequency = (double) (m_Sample->GetTotalFrequency(0)) ;
  long i, j ;
  double tempSum ;
  bool updated = false ;

  for (i = 0 ; i < numberOfComponents ; i++)
    {
      tempSum = 0.0 ;
      for (j = 0 ; j < sampleSize ; j++)
        {
          tempSum += 
            (m_ComponentVector[i]->GetWeight(j) * 
             m_Sample->GetFrequency(j)) ;
        }

      tempSum /= totalFrequency ;

      if (tempSum != m_Proportions[i])
        {
          m_Proportions[i] = tempSum ; 
          updated = true ;
        }
    }

  return updated ;
}

template< class TSample >
void
ExpectationMaximizationMixtureModelEstimator< TSample >
::GenerateData() 
{
  m_Proportions = m_InitialProportions ;   

  int iteration = 0 ;
  m_CurrentIteration = 0 ;
  while (iteration < m_MaxIteration)
    {
      m_CurrentIteration = iteration ;
      if (this->CalculateDensities())
        {
          this->UpdateComponentParameters() ;
          this->UpdateProportions() ;
        }
      else
        {
          m_TerminationCode = CONVERGED ;
          break ;
        }
      ++iteration ;
    }
  
  m_TerminationCode = NOT_CONVERGED ;
}

template< class TSample >
void 
ExpectationMaximizationMixtureModelEstimator< TSample >
::Update()
{
  this->GenerateData() ;
}
 
} // end of namespace Statistics 
} // end of namespace itk

#endif







