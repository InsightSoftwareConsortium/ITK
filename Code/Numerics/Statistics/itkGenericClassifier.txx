/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGenericClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkGenericClassifier_txx
#define __itkGenericClassifier_txx

namespace itk{ 
  namespace Statistics{

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SetSample(SamplePointer sample)
{
  m_Sample = sample ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >::SamplePointer
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetSample()
{
  return m_Sample ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::PrepareClassSampleVector(unsigned int numberOfClasses)
{
  for (unsigned int i = 0 ; i < numberOfClasses ; i++)
    {
      m_ClassSamples.push_back(ClassSampleType::New()) ;
      (m_ClassSamples[i])->SetSample(m_Sample) ;
    }
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >::ClassSamplePointer
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetClassSample(unsigned int classLabel) 
  throw (ExceptionObject)
{
  if (classLabel >= m_ClassSamples.size())
    {
      throw ExceptionObject(__FILE__, __LINE__) ;
    }

  return m_ClassSamples[classLabel] ;
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

  this->PrepareClassSampleVector(m_MembershipCalculators.size()) ;
  
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
      this->GetClassSample(classLabel)->AddInstance(iter.GetInstanceIdentifier()) ;
      ++iter ;
    }
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >::ClassSampleVectorType
GenericClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GetOutput() 
{
  return m_ClassSamples ;
}

  } // end of namespace Statistics 
} // end of namespace itk

#endif








