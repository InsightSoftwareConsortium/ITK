/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMembershipSample_txx
#define __itkMembershipSample_txx

namespace itk{ 
  namespace Statistics{

template< class TSample >
MembershipSample< TSample >
::MembershipSample()
{
  m_CurrentClassLabel = 0 ;
}

template< class TSample >
void 
MembershipSample< TSample >
::SetSample(SamplePointer sample)
{
  m_Sample = sample ; 
}

template< class TSample >
MembershipSample< TSample >::SamplePointer 
MembershipSample< TSample >
::GetSample()
{
  return m_Sample ; 
} 

template< class TSample >
unsigned int
MembershipSample< TSample >
::GetNumberOfClasses()
{
  return m_ClassLabels.size() ;
}

template< class TSample >
bool
MembershipSample< TSample >
::ClassLabelExists(unsigned int classLabel)
{
  
  if (m_ClassLabels.find(classLabel) != m_ClassLabels.end())
    {
      return true ;
    }
  else
    {
      return false ;
    }
}

template< class TSample >
void 
MembershipSample< TSample >
::AddInstance(unsigned int classLabel, InstanceIdentifier id) 
{ 
  if (m_ClassLabels.find(classLabel) == m_ClassLabels.end())
    {
      m_ClassLabels.insert(classLabel) ;
    }
  
  m_ClassLabelHolder[id] = classLabel ; 
}

template< class TSample >
unsigned int 
MembershipSample< TSample >
::GetClassLabel(InstanceIdentifier id) throw (ExceptionObject)
{
  ClassLabelHolder::iterator iter = m_ClassLabelHolder.find(id) ;
  if (iter != m_ClassLabelHolder.end())
    {
      return iter->second ;
    }
  else
    {
      throw ExceptionObject(__FILE__, __LINE__) ;
    }
}

template< class TSample >
MembershipSample< TSample >::ClassSamplePointer
MembershipSample< TSample >
::GetClassSample(unsigned int classLabel)
{
  ClassSamplePointer classSample = ClassSampleType::New() ;
  classSample->SetSample(this->GetSample()) ;
  Iterator iter = this->Begin() ;
  while (iter != this->End())
    {
      if (iter.GetClassLabel() == classLabel)
        {
          classSample->AddInstance(iter.GetInstanceIdentifier()) ;
        }
      ++iter ;
    }
  return classSample ; 
}

template< class TSample >
MembershipSample< TSample >::SizeType
MembershipSample< TSample >
::GetSize()
{
  return m_Sample->GetSize() ; 
}
  
template< class TSample >
MembershipSample< TSample >::SizeValueType
MembershipSample< TSample >
::GetSize(unsigned int dimension) 
{
  return m_Sample->GetSize(dimension) ;
}

template< class TSample >
MembershipSample< TSample >::MeasurementVectorType
MembershipSample< TSample >
::GetMeasurementVector(const InstanceIdentifier id)
{
  return m_Sample->GetMeasurementVector(id) ; 
}

template< class TSample >
MembershipSample< TSample >::FrequencyType
MembershipSample< TSample >
::GetFrequency(const InstanceIdentifier id)
{
  return m_Sample->GetFrequency(id) ; 
}

template< class TSample >
MembershipSample< TSample >::MeasurementType
MembershipSample< TSample >
::GetMeasurement(const unsigned int d, const unsigned long n) 
{ 
  return m_Sample->GetMeasurement(d, n) ;
}
  
template< class TSample >
MembershipSample< TSample >::FrequencyType
MembershipSample< TSample >
::GetFrequency(const unsigned int d, const unsigned long n)
{
  return m_Sample->GetFrequency(d, n) ;
}

template< class TSample >
MembershipSample< TSample >::FrequencyType
MembershipSample< TSample >
::GetTotalFrequency(const unsigned int d)
{
  return m_Sample->GetTotalFrequency(d) ;
}

template< class TSample >
void
MembershipSample< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Sample: " << m_Sample << std::endl;
  os << indent << "CurrentClassLabel: " << m_CurrentClassLabel << std::endl;
  os << indent << "ClassLabelHolder: " << &m_ClassLabelHolder << std::endl;
  os << indent << "ClassLabels: [0, " << m_ClassLabels.size() <<  ")" ;
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif







