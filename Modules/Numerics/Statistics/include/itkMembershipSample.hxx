/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMembershipSample_hxx
#define itkMembershipSample_hxx

#include "itkMembershipSample.h"

namespace itk
{
namespace Statistics
{
template< typename TSample >
MembershipSample< TSample >
::MembershipSample()
{
  this->m_NumberOfClasses = 0;
}

template< typename TSample >
void
MembershipSample< TSample >
::SetNumberOfClasses(unsigned int numberOfClasses)
{
  m_NumberOfClasses = numberOfClasses;
  m_ClassSamples.resize(m_NumberOfClasses);
  for ( unsigned int i = 0; i < m_NumberOfClasses; i++ )
    {
    m_ClassSamples[i] = ClassSampleType::New();
    ( m_ClassSamples[i] )->SetSample( this->GetSample() );
    }
}

template< typename TSample >
inline void
MembershipSample< TSample >
::AddInstance(const ClassLabelType & classLabel, const InstanceIdentifier & id)
{
  m_ClassLabelHolder[id] = classLabel;
  int classIndex = this->GetInternalClassLabel(classLabel);
  if ( classIndex == -1 )
    {
    m_UniqueClassLabels.push_back(classLabel);
    classIndex = static_cast<int>( m_UniqueClassLabels.size() ) - 1;
    }

  ( m_ClassSamples[classIndex] )->AddInstance(id);
}

template< typename TSample >
inline unsigned int
MembershipSample< TSample >
::GetClassLabel(const InstanceIdentifier & id) const
{
  return ( *( m_ClassLabelHolder.find(id) ) ).second;
}

template< typename TSample >
inline int
MembershipSample< TSample >
::GetInternalClassLabel(const ClassLabelType classLabel) const
{
  for ( unsigned int i = 0; i < m_UniqueClassLabels.size(); i++ )
    {
    if ( m_UniqueClassLabels[i] == classLabel )
      {
      return i;
      }
    }

  return -1;
}

template< typename TSample >
const typename MembershipSample< TSample >::ClassLabelHolderType
MembershipSample< TSample >
::GetClassLabelHolder() const
{
  return m_ClassLabelHolder;
}

template< typename TSample >
const typename MembershipSample< TSample >::ClassSampleType *
MembershipSample< TSample >
::GetClassSample(const ClassLabelType & classLabel) const
{
  int classIndex = this->GetInternalClassLabel(classLabel);
  if (classIndex < 0)
    {
    return ITK_NULLPTR;
    }

  return m_ClassSamples[classIndex];
}

template< typename TSample >
inline const typename MembershipSample< TSample >::MeasurementVectorType &
MembershipSample< TSample >
::GetMeasurementVector(const InstanceIdentifier & id) const
{
  return m_Sample->GetMeasurementVector(id);
}

template< typename TSample >
inline typename MembershipSample< TSample >::MeasurementType
MembershipSample< TSample >
::GetMeasurement(const InstanceIdentifier & id,
                 const unsigned int & dimension)
{
  return m_Sample->GetMeasurement(id, dimension);
}

template< typename TSample >
inline typename MembershipSample< TSample >::AbsoluteFrequencyType
MembershipSample< TSample >
::GetFrequency(const InstanceIdentifier & id) const
{
  return m_Sample->GetFrequency(id);
}

template< typename TSample >
inline typename MembershipSample< TSample >::TotalAbsoluteFrequencyType
MembershipSample< TSample >
::GetTotalFrequency() const
{
  return m_Sample->GetTotalFrequency();
}

template< typename TSample >
void
MembershipSample< TSample >
::Graft(const DataObject *thatObject)
{
  this->Superclass::Graft(thatObject);

  // Most of what follows is really a deep copy, rather than grafting of
  // output. Wish it were managed by pointers to bulk data. Sigh !

  const Self *thatConst = dynamic_cast< const Self * >( thatObject );
  if ( thatConst )
    {
    Self *that = const_cast< Self * >( thatConst );
    this->m_UniqueClassLabels = that->m_UniqueClassLabels;
    this->m_ClassLabelHolder  = that->m_ClassLabelHolder;
    this->m_ClassSamples      = that->m_ClassSamples;
    this->m_Sample            = that->m_Sample;
    this->m_NumberOfClasses   = that->m_NumberOfClasses;
    }
}

template< typename TSample >
void
MembershipSample< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Sample: " << m_Sample.GetPointer() << std::endl;
  os << indent << "NumberOfClasses: " << this->GetNumberOfClasses() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
