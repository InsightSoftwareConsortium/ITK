/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleClassifier_txx
#define __itkSampleClassifier_txx

#include "itkSampleClassifier.h"

namespace itk{ 
  namespace Statistics{

template< class TSample >
SampleClassifier< TSample >
::SampleClassifier()
{
  m_Sample = 0 ;
  m_Output = OutputType::New() ;
}

template< class TSample >
void
SampleClassifier< TSample >
::SetSample(TSample* sample)
{
  if ( m_Sample != sample )
    {
      m_Sample = sample ;
      m_Output->SetSample(sample) ;
    }
}

template< class TSample >
TSample*
SampleClassifier< TSample >
::GetSample()
{
  return m_Sample ;
}

template< class TSample >
void
SampleClassifier< TSample >
::SetMembershipFunctionClassLabels(ClassLabelVectorType& labels)
{
  m_ClassLabels = labels ;
}

template< class TSample >
void
SampleClassifier< TSample >
::GenerateData()
{
  unsigned int i ;
  typename TSample::Iterator iter = GetSample()->Begin() ;
  typename TSample::Iterator end = GetSample()->End() ;
  typename TSample::MeasurementVectorType measurements ;

  m_Output->Resize(GetSample()->Size()) ;
  std::vector< double > discriminantScores ;
  unsigned int numberOfClasses = this->GetNumberOfClasses() ;
  discriminantScores.resize(numberOfClasses) ;
  unsigned int classLabel ;
  m_Output->SetNumberOfClasses(numberOfClasses) ;
  typename Superclass::DecisionRuleType::Pointer rule = 
    this->GetDecisionRule() ;
  typename Superclass::MembershipFunctionPointerVector mfs =
    this->GetMembershipFunctions() ;
  

  if ( m_ClassLabels.size() != mfs.size() )
    {
      while (iter != end)
        {
          measurements = iter.GetMeasurementVector() ;
          for (i = 0 ; i < numberOfClasses ; i++)
            {
              discriminantScores[i] = (mfs[i])->Evaluate(measurements) ;
            }
          classLabel = rule->Evaluate(discriminantScores) ;
          m_Output->AddInstance(classLabel, iter.GetInstanceIdentifier()) ;
          ++iter ;
        }
    }
  else
    {
      while (iter != end)
        {
          measurements = iter.GetMeasurementVector() ;
          for (i = 0 ; i < numberOfClasses ; i++)
            {
              discriminantScores[i] = (mfs[i])->Evaluate(measurements) ;
            }
          classLabel = rule->Evaluate(discriminantScores) ;
          m_Output->AddInstance(m_ClassLabels[classLabel], 
                                iter.GetInstanceIdentifier()) ;
          ++iter ;
        }
    }
}

template< class TSample >
typename SampleClassifier< TSample >::OutputType*
SampleClassifier< TSample >
::GetOutput() 
{
  return m_Output ;
}

template< class TSample >
void
SampleClassifier< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Sample: " << m_Sample << std::endl;
  os << indent << "Output: " << m_Output << std::endl;
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif








