/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSelectiveSubsampleGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSelectiveSubsampleGenerator_txx
#define __itkSelectiveSubsampleGenerator_txx

namespace itk{
namespace Statistics{

template< class TInputSample, class TClassMaskSample >
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::SelectiveSubsampleGenerator()
{
  m_Input = 0 ;
  m_ClassMask = 0 ;
  m_Output = OutputType::New() ;
}

template< class TInputSample, class TClassMaskSample >
void
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Input: " ;
  if ( m_Input != 0 )
    {
    os << m_Input << std::endl;
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
  os << indent << "SelectedClassLabels: " ;
  for ( unsigned int i = 0 ; i < m_SelectedClassLabels.size() ; ++i )
    {
    os << " " << m_SelectedClassLabels[i] ;
    }
  os << std::endl ;
}

template< class TInputSample, class TClassMaskSample >
void
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::SetInput(TInputSample* sample)
{
  m_Input = sample ;
}
  
template< class TInputSample, class TClassMaskSample >
TInputSample*
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::GetInput()
{
  return m_Input ;
}

template< class TInputSample, class TClassMaskSample >
void
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::SetClassMask(TClassMaskSample* classMask)
{
  m_ClassMask = classMask ;
}

template< class TInputSample, class TClassMaskSample >
TClassMaskSample*
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::GetClassMask()
{
  return m_ClassMask ;
}

template< class TInputSample, class TClassMaskSample >
typename SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >::OutputType*
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::GetOutput()
{
  return m_Output.GetPointer() ;
}

template< class TInputSample, class TClassMaskSample >
void
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::GenerateData()
{
  m_Output->SetSample(m_Input) ;
  typename TClassMaskSample::Iterator iter = m_ClassMask->Begin() ;
  while (iter != m_ClassMask->End())
    {
    if ( std::find(m_SelectedClassLabels.begin(), 
                   m_SelectedClassLabels.end(), 
                   iter.GetMeasurementVector()[0]) != 
         m_SelectedClassLabels.end() )
      {
      m_Output->AddInstance(iter.GetInstanceIdentifier()) ;
      }
    ++iter ;
    }
}

} // end namespace Statistics
} // end namespace itk

#endif
