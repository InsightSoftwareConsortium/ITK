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
  m_Output = OutputType::New() ;
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

template< class TInputSample, class TClassMaskSample >
void
SelectiveSubsampleGenerator< TInputSample, TClassMaskSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Input: " << m_Input << std::endl;
  os << indent << "Output: " << m_Output << std::endl;
  os << indent << "ClassMask: " << m_ClassMask << std::endl ;
  //  os << indent << "Selected Class Labels: " << &m_SelectedClassLabels 
  //  << std::endl ;
}
} // end namespace Statistics
} // end namespace itk

#endif
