/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTableLookupSampleClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTableLookupSampleClassifier_txx
#define __itkTableLookupSampleClassifier_txx

#include "itkTableLookupSampleClassifier.h"
#include "itkDecisionRuleBase.h"

namespace itk{ 
  namespace Statistics{

template< class TSample >
TableLookupSampleClassifier< TSample >
::TableLookupSampleClassifier()
{
  m_LookupTable = 0 ;
}

template< class TSample >
void
TableLookupSampleClassifier< TSample >
::SetLookupTableLowerBound(MeasurementVectorType lower)
{
  for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
    {
      m_LowerBound[i] = lower[i] ;
    }
}

template< class TSample >
void
TableLookupSampleClassifier< TSample >
::SetLookupTableUpperBound(MeasurementVectorType upper)
{
  for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
    {
      m_UpperBound[i] = upper[i] ;
    }
}

template< class TSample >
void
TableLookupSampleClassifier< TSample >
::PrepareLookupTable()
{
  RegionType region ;
  SizeType size ;
  m_LookupTable = LookupTableType::New() ;
  for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
    {
    size[i] = m_UpperBound[i] - m_LowerBound[i] + 1 ;
    }

  region.SetIndex(m_LowerBound) ;
  region.SetSize(size) ;
  
  m_LookupTable->SetLargestPossibleRegion(region) ;
  m_LookupTable->SetBufferedRegion(region) ;
  m_LookupTable->Allocate() ;

  std::vector< double > discriminantScores ;
  unsigned int numberOfClasses = GetNumberOfClasses() ;
  discriminantScores.resize(numberOfClasses) ;

  CachedMeasurementVectorType cachedMeasurementVector ;
  MeasurementVectorType measurementVector ;
  DecisionRuleBase::Pointer rule = this->GetDecisionRule() ;
  typename Superclass::MembershipFunctionPointerVector mf = 
    this->GetMembershipFunctions() ;
  LookupTableIteratorType iter(m_LookupTable, region) ;

  while (!iter.IsAtEnd())
    {
    cachedMeasurementVector = iter.GetIndex() ;
    for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
      {
      measurementVector[i] = cachedMeasurementVector[i] ;
      }

    for (unsigned int i = 0 ; i < numberOfClasses ; i++)
      {
      discriminantScores[i] = (mf[i])->Evaluate(measurementVector) ;
      }

    iter.Set(rule->Evaluate(discriminantScores)) ;
    ++iter ;
    }
}

template< class TSample >
void
TableLookupSampleClassifier< TSample >
::GenerateData()
{
  if ( m_LookupTable.IsNull() )
    {
      PrepareLookupTable() ;
    }

  typename TSample::Iterator iter = GetSample()->Begin() ;
  typename TSample::Iterator end = GetSample()->End() ;
  typename TSample::MeasurementVectorType measurements ;
  unsigned int numberOfClasses = this->GetNumberOfClasses() ;
  CachedMeasurementVectorType temp ;
  OutputType* output = GetOutput() ;
  unsigned int classLabel ;
  output->SetNumberOfClasses(numberOfClasses) ;
  while (iter != end)
    {
      measurements = iter.GetMeasurementVector() ;
      for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
        {
          temp[i] = measurements[i] ;
        }
      classLabel = m_LookupTable->GetPixel(temp) ;
      output->AddInstance(classLabel, iter.GetInstanceIdentifier()) ;
      ++iter ;
    }
}

template< class TSample >
void
TableLookupSampleClassifier< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif








