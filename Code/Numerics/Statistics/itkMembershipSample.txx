/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSample.txx
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







