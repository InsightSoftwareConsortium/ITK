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
::SetSample(TSample* sample)
{
  m_Sample = sample ; 
}

template< class TSample >
TSample*
MembershipSample< TSample >
::GetSample()
{
  return m_Sample ; 
} 

template< class TSample >
void
MembershipSample< TSample >
::SetNumberOfClasses(unsigned int numberOfClasses)
{
  m_NumberOfClasses = numberOfClasses ;
  m_ClassSampleSizes.resize(m_NumberOfClasses) ;
  m_ClassSamples.resize(m_NumberOfClasses) ;
  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      m_ClassSamples[i] = ClassSampleType::New() ;
      (m_ClassSamples[i])->SetSample(this->GetSample()) ;
      m_ClassSampleSizes[i] = 0 ;
    }
}

template< class TSample >
unsigned int
MembershipSample< TSample >
::GetNumberOfClasses() const
{
  return m_NumberOfClasses ;
}

template< class TSample >
unsigned int
MembershipSample< TSample >
::GetNumberOfInstances() const
{
  return m_Sample->GetNumberOfInstances() ;
}

template< class TSample >
inline void 
MembershipSample< TSample >
::AddInstance(const unsigned int &classLabel, const InstanceIdentifier &id) 
{ 
  m_ClassLabelHolder[id] = classLabel ; 
  int classIndex = this->GetInternalClassLabel(classLabel) ;
  if ( classIndex == -1 )
    {
      m_UniqueClassLabels.push_back(classLabel) ;
      classIndex = m_UniqueClassLabels.size() - 1 ;
    }

  m_ClassSampleSizes[classIndex] += 1 ;

  (m_ClassSamples[classIndex])->AddInstance(id) ;
}

template< class TSample >
inline unsigned int 
MembershipSample< TSample >
::GetClassLabel(const InstanceIdentifier &id) const
{
  return (*(m_ClassLabelHolder.find(id))).second ;
}

template< class TSample >
inline int 
MembershipSample< TSample >
::GetInternalClassLabel(const unsigned int classLabel) const
{
  for ( unsigned int i = 0 ; i < m_UniqueClassLabels.size() ; i++ )
    {
      if ( m_UniqueClassLabels[i] == classLabel )
        {
          return i ;
        }
    }

  return -1 ;
}

template< class TSample >
unsigned int
MembershipSample< TSample >
::GetClassSampleSize(const unsigned int &classLabel) const
{
  int classIndex = this->GetInternalClassLabel(classLabel) ;
  return m_ClassSampleSizes[classIndex] ;
}


template< class TSample >
typename MembershipSample< TSample >::ClassSampleType*
MembershipSample< TSample >
::GetClassSample(const unsigned int &classLabel)
{
  int classIndex = this->GetInternalClassLabel(classLabel) ;
  return m_ClassSamples[classIndex] ; 
}

template< class TSample >
inline unsigned int
MembershipSample< TSample >
::Size(void) const
{
  return m_Sample->Size() ; 
}
  
template< class TSample >
inline unsigned int 
MembershipSample< TSample >
::Size(const unsigned int &dimension) const
{
  return m_Sample->Size(dimension) ;
}

template< class TSample >
inline typename MembershipSample< TSample >::MeasurementVectorType&
MembershipSample< TSample >
::GetMeasurementVector(const InstanceIdentifier &id)
{
  return m_Sample->GetMeasurementVector(id) ; 
}

template< class TSample >
inline typename MembershipSample< TSample >::MeasurementType&
MembershipSample< TSample >
::GetMeasurement(const InstanceIdentifier &id,
                 const unsigned int &dimension)
{ 
  return m_Sample->GetMeasurement(id, dimension) ;
}

template< class TSample >
inline typename MembershipSample< TSample >::FrequencyType
MembershipSample< TSample >
::GetFrequency(const InstanceIdentifier &id) const
{
  return m_Sample->GetFrequency(id) ; 
}
  
template< class TSample >
inline typename MembershipSample< TSample >::FrequencyType
MembershipSample< TSample >
::GetTotalFrequency(const unsigned int &dimension) const
{
  return m_Sample->GetTotalFrequency(dimension) ;
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
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif








