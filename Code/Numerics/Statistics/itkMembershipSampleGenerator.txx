/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSampleGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMembershipSampleGenerator_txx
#define __itkMembershipSampleGenerator_txx

namespace itk{
namespace Statistics{

template< class TInputSample, class TClassMaskSample >
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::MembershipSampleGenerator()
{
  m_Input = 0 ;
  m_ClassMask = 0 ;
  m_Output = OutputType::New() ;
}

template< class TInputSample, class TClassMaskSample >
void
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Input: " ;
  if ( m_Input != 0 )
    {
    os << m_Input << std::endl ;
    }
  else
    {
    os << "not set." << std::endl ;
    }
  
  os << indent << "ClassMask: " ;
  if ( m_ClassMask != 0 )
    {
    os << m_ClassMask << std::endl ;
    }
  else
    {
    os << "not set." << std::endl ;
    }
  
  os << indent << "Output: " << m_Output << std::endl;
}

template< class TInputSample, class TClassMaskSample >
void
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::SetInput(TInputSample* sample)
{
  m_Input = sample ;
}
  
template< class TInputSample, class TClassMaskSample >
TInputSample*
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::GetInput()
{
  return m_Input ;
}

template< class TInputSample, class TClassMaskSample >
void
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::SetClassMask(TClassMaskSample* classMask)
{
  m_ClassMask = classMask ;
}

template< class TInputSample, class TClassMaskSample >
TClassMaskSample*
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::GetClassMask()
{
  return m_ClassMask ;
}

template< class TInputSample, class TClassMaskSample >
void
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::SetNumberOfClasses(int numberOfClasses)
{
  m_NumberOfClasses = numberOfClasses ;
}

template< class TInputSample, class TClassMaskSample >
int
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::GetNumberOfClasses() 
{
  return m_NumberOfClasses ;
}


template< class TInputSample, class TClassMaskSample >
typename MembershipSampleGenerator< TInputSample, TClassMaskSample >::OutputPointer
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::GetOutput()
{
  return m_Output ;
}

template< class TInputSample, class TClassMaskSample >
void
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::GenerateData()
{
  unsigned int classLabel ;
  m_Output->SetSample(m_Input) ;
  m_Output->SetNumberOfClasses(m_NumberOfClasses) ;
  typename TClassMaskSample::Iterator iter = m_ClassMask->Begin() ;
  while (iter != m_ClassMask->End())
    {
    classLabel = iter.GetMeasurementVector()[0] ;
    m_Output->AddInstance(classLabel, iter.GetInstanceIdentifier()) ;
    ++iter ;
    }
}

} // end namespace Statistics
} // end namespace itk

#endif
