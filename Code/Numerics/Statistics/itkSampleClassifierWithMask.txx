/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleClassifierWithMask.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleClassifierWithMask_txx
#define __itkSampleClassifierWithMask_txx

#include "itkSampleClassifierWithMask.h"

namespace itk{ 
namespace Statistics{

template< class TSample, class TMaskSample >
SampleClassifierWithMask< TSample, TMaskSample >
::SampleClassifierWithMask()
{
  m_OtherClassLabel = 0 ;
  m_Mask = 0 ;
}

template< class TSample, class TMaskSample >
void
SampleClassifierWithMask< TSample, TMaskSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Mask: " ;
  if ( m_Mask != 0 )
    {
    os << m_Mask << std::endl;
    }
  else
    {
    os << "not set." << std::endl ;
    }

  os << indent << "SelectedClassLabels: " ;
  for ( unsigned int i = 0 ; i < m_SelectedClassLabels.size() ; ++i )
    {
    os << " " << m_SelectedClassLabels[i] ;
    }
  os << std::endl ;
  os << indent << "OtherClassLabel: " << m_OtherClassLabel << std::endl ;
}

template< class TSample, class TMaskSample >
void
SampleClassifierWithMask< TSample, TMaskSample >
::SetMask(TMaskSample* mask)
{
  if ( m_Mask != mask )
    {
    m_Mask = mask ;
    }
}

template< class TSample, class TMaskSample >
void
SampleClassifierWithMask< TSample, TMaskSample >
::GenerateData()
{
  unsigned int i ;
  typename TSample::Iterator iter = GetSample()->Begin() ;
  typename TSample::Iterator end = GetSample()->End() ;
  typename TSample::MeasurementVectorType measurements ;

  typename TMaskSample::Iterator m_iter = this->GetMask()->Begin() ;

  OutputType* output = this->GetOutput() ;
  output->Resize(GetSample()->Size()) ;
  std::vector< double > discriminantScores ;
  unsigned int numberOfClasses = this->GetNumberOfClasses() ;
  discriminantScores.resize(numberOfClasses) ;
  output->SetNumberOfClasses(numberOfClasses + 1) ;
  unsigned int classLabel ;
  typename Superclass::DecisionRuleType::Pointer rule = 
    this->GetDecisionRule() ;
  typename Superclass::MembershipFunctionPointerVector mfs =
    this->GetMembershipFunctions() ;
  typename Superclass::ClassLabelVectorType classLabels = 
    this->GetMembershipFunctionClassLabels() ;
  
  if ( this->GetMask()->Size() != this->GetSample()->Size() )
    {
    throw ExceptionObject
      (__FILE__, __LINE__, 
       "The sizes of the mask sample and the input sample do not match.") ;
    }

  if ( classLabels.size() != mfs.size() )
    {
    while (iter != end)
      {
      measurements = iter.GetMeasurementVector() ;
      if ( std::find(m_SelectedClassLabels.begin(), 
                     m_SelectedClassLabels.end(), 
                     m_iter.GetMeasurementVector()[0]) != 
           m_SelectedClassLabels.end() )
        {
        for (i = 0 ; i < numberOfClasses ; i++)
          {
          discriminantScores[i] = (mfs[i])->Evaluate(measurements) ;
          }
        classLabel = rule->Evaluate(discriminantScores) ;
        }
      else
        {
        classLabel = m_OtherClassLabel ;
        }
      output->AddInstance(classLabel, iter.GetInstanceIdentifier()) ;
      ++iter ;
      ++m_iter ;
      }
    }
  else
    {
    while (iter != end)
      {
      measurements = iter.GetMeasurementVector() ;
      if ( std::find(m_SelectedClassLabels.begin(), 
                     m_SelectedClassLabels.end(), 
                     m_iter.GetMeasurementVector()[0]) != 
           m_SelectedClassLabels.end() )
        {
        for (i = 0 ; i < numberOfClasses ; i++)
          {
          discriminantScores[i] = (mfs[i])->Evaluate(measurements) ;
          }
        classLabel = rule->Evaluate(discriminantScores) ;
        output->AddInstance(classLabels[classLabel], 
                            iter.GetInstanceIdentifier()) ;
        }
      else
        {
        output->AddInstance(m_OtherClassLabel, 
                            iter.GetInstanceIdentifier()) ;
        }
      ++iter ;
      ++m_iter ;
      }
    }
}

} // end of namespace Statistics 
} // end of namespace itk

#endif








