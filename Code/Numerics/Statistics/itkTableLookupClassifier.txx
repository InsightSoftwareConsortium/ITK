/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTableLookupClassifier.txx
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
#ifndef __itkTableLookupClassifier_txx
#define __itkTableLookupClassifier_txx


namespace itk{ 
  namespace Statistics{

template< class TSample, class TMembershipCalculator, class TDecisionRule >
TableLookupClassifier< TSample, TMembershipCalculator, TDecisionRule >
::TableLookupClassifier()
{
  m_LookupTable = 0 ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
TableLookupClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SetLookupTableLowerBound(CachedMeasurementVectorType lower)
{
  m_LowerBound = lower ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
TableLookupClassifier< TSample, TMembershipCalculator, TDecisionRule >
::SetLookupTableUpperBound(CachedMeasurementVectorType upper)
{
  m_UpperBound = upper ;
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
TableLookupClassifier< TSample, TMembershipCalculator, TDecisionRule >
::PrepareLookupTable()
{
  size_t i ;
  RegionType region ;
  SizeType size ;
  m_LookupTable = LookupTableType::New() ;
  for (i = 0 ; i < MeasurementVectorSize ; i++)
    {
      size[i] = m_UpperBound[i] - m_LowerBound[i] + 1 ;
    }

  region.SetIndex(m_LowerBound) ;
  region.SetSize(size) ;
  
  m_LookupTable->SetLargestPossibleRegion(region) ;
  m_LookupTable->SetBufferedRegion(region) ;
  m_LookupTable->Allocate() ;

  std::vector< double > discriminantScores ;
  size_t numberOfClasses = GetNumberOfClasses() ;
  discriminantScores.resize(numberOfClasses) ;

  CachedMeasurementVectorType cachedMeasurementVector ;
  MeasurementVectorType measurementVector ;
  DecisionRulePointer rule = GetDecisionRule() ;
  MembershipCalculatorVectorType membershipCalculators = 
    GetMembershipCalculatorVector() ;
  LookupTableIteratorType iter(m_LookupTable, region) ;
  while (!iter.IsAtEnd())
    {
      cachedMeasurementVector = iter.GetIndex() ;
      for (i = 0 ; i < MeasurementVectorSize ; i++)
        {
          measurementVector[i] = cachedMeasurementVector[i] ;
        }

      for (i = 0 ; i < numberOfClasses ; i++)
        {
          discriminantScores[i] = (membershipCalculators[i])->Evaluate(measurementVector) ;
        }

      iter.Set(rule->Evaluate(discriminantScores)) ;
      ++iter ;
    }
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
TableLookupClassifier< TSample, TMembershipCalculator, TDecisionRule >
::GenerateData()
{
  unsigned int i ;
  
  if (m_LookupTable == 0)
    {
      PrepareLookupTable() ;
    }

  typename TSample::Iterator iter = GetSample()->Begin() ;
  typename TSample::Iterator last = GetSample()->End() ;
  typename TSample::MeasurementVectorType measurements ;
  CachedMeasurementVectorType temp ;
  OutputPointer output = GetOutput() ;

  while (iter != last)
    {
      measurements = iter.GetMeasurementVector() ;
      for (size_t i = 0 ; i < MeasurementVectorSize ; i++)
        {
          temp[i] = measurements[i] ;
        }
      m_LookupTable->GetPixel(temp) ;
      output->AddInstance(m_LookupTable->GetPixel(temp), iter.GetInstanceIdentifier()) ;
      ++iter ;
    }
}

template< class TSample, class TMembershipCalculator, class TDecisionRule >
void
TableLookupClassifier< TSample, TMembershipCalculator, TDecisionRule >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i ;
  Superclass::PrintSelf(os,indent);
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif








