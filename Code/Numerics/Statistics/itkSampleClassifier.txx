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

namespace itk{ 
  namespace Statistics{

template< class TSample, class TMembershipCalculator, class TDecisionRule >
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SampleClassifier()
{
  m_Output = OutputType::New() ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SetSample(SamplePointer sample)
{
  m_Sample = sample ;
  m_Output->SetSample(sample) ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >::SamplePointer
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetSample()
{
  return m_Sample ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
unsigned int 
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::AddMembershipCalculator(MembershipCalculatorPointer function)
{
  m_MembershipCalculators.push_back(function) ;
  return m_MembershipCalculators.size() ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SetDecisionRule(DecisionRulePointer decisionRule)
{
  m_DecisionRule = decisionRule ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >::DecisionRulePointer
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetDecisionRule()
{
  return m_DecisionRule ;
}


template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GenerateData()
{
  unsigned int i ;

  typename TSample::Iterator iter = GetSample()->Begin() ;
  typename TSample::Iterator end = GetSample()->End() ;
  typename TSample::MeasurementVectorType measurements ;
  
  std::vector< double > discriminantScores ;
  unsigned int numberOfClasses = m_MembershipCalculators.size() ;
  discriminantScores.resize(numberOfClasses) ;
  unsigned int classLabel ;
  m_Output->SetNumberOfClasses(numberOfClasses) ;
  typename TDecisionRule::Pointer rule = this->GetDecisionRule() ;

  while (iter != end)
    {
      measurements = iter.GetMeasurementVector() ;
      for (i = 0 ; i < numberOfClasses ; i++)
        {
          discriminantScores[i] = (m_MembershipCalculators[i])->Evaluate(measurements) ;
        }
      
      classLabel = rule->Evaluate(discriminantScores) ;
      m_Output->AddInstance(classLabel, iter.GetInstanceIdentifier()) ;
      ++iter ;
    }
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >::OutputPointer
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetOutput() 
{
  return m_Output ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
SampleClassifier< TSample, TMembershipCalculator, TDecisionRule >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i ;
  Superclass::PrintSelf(os,indent);

  os << indent << "Sample: " << m_Sample << std::endl;
  os << indent << "DecisionRule: " << m_DecisionRule << std::endl;

  os << indent << "MembershipCalculators: [" ;
  for (i=0; i < m_MembershipCalculators.size() - 1; i++)
    {
    os << m_MembershipCalculators[i] << ", ";
    }
  os << m_MembershipCalculators[i] << "]" << std::endl;
  
  os << indent << "Output: " << m_Output << std::endl;
  //  os << indent << "Accesor: " << m_Accessor << std::endl;
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif








