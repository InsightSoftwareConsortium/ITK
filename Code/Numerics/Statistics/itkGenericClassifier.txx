/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGenericClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGenericClassifier_txx
#define __itkGenericClassifier_txx

namespace itk{ 
  namespace Statistics{

template< class TSample, class TMembershipCalculator, class TDecisionRule >
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GenericClassifier()
{
  m_Output = OutputType::New() ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SetSample(SamplePointer sample)
{
  m_Sample = sample ;
  m_Output->SetSample(sample) ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >::SamplePointer
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetSample()
{
  return m_Sample ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
unsigned int 
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::AddMembershipCalculator(MembershipCalculatorPointer function)
{
  m_MembershipCalculators.push_back(function) ;
  return m_MembershipCalculators.size() ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SetDecisionRule(DecisionRulePointer decisionRule)
{
  m_DecisionRule = decisionRule ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >::DecisionRulePointer
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetDecisionRule()
{
  return m_DecisionRule ;
}


template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GenerateData()
{
  unsigned int i ;

  typename TSample::Iterator iter = GetSample()->Begin() ;
  typename TSample::Iterator last = GetSample()->End() ;
  typename TSample::MeasurementVectorType measurements ;
  
  std::vector< double > discriminantScores ;
  discriminantScores.resize(m_MembershipCalculators.size()) ;
  unsigned int classLabel ;
  typename TDecisionRule::Pointer rule = this->GetDecisionRule() ;
  while (iter != last)
    {
      measurements = iter.GetMeasurementVector() ;
      for (i = 0 ; i < m_MembershipCalculators.size() ; i++)
        {
          discriminantScores[i] = (m_MembershipCalculators[i])->Evaluate(measurements) ;
        }

      classLabel = rule->Evaluate(discriminantScores) ;
      m_Output->AddInstance(classLabel, iter.GetInstanceIdentifier()) ;
      ++iter ;
    }
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >::OutputPointer
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetOutput() 
{
  return m_Output ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
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








