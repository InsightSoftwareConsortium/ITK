/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSampleGenerator.txx
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
#ifndef __itkMembershipSampleGenerator_txx
#define __itkMembershipSampleGenerator_txx

namespace itk{
  namespace Statistics{

template< class TInputSample, class TClassMaskSample >
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::MembershipSampleGenerator()
{
}

template< class TInputSample, class TClassMaskSample >
void
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::SetInput(InputPointer sample)
{
  m_Input = sample ;
}
  
template< class TInputSample, class TClassMaskSample >
MembershipSampleGenerator< TInputSample, TClassMaskSample >::InputPointer
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::GetInput()
{
  return m_Input ;
}

template< class TInputSample, class TClassMaskSample >
void
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::SetClassMask(ClassMaskPointer classMask)
{
  m_ClassMask = classMask ;
}

template< class TInputSample, class TClassMaskSample >
MembershipSampleGenerator< TInputSample, TClassMaskSample >::ClassMaskPointer
MembershipSampleGenerator< TInputSample, TClassMaskSample >
::GetClassMask()
{
  return m_ClassMask ;
}

template< class TInputSample, class TClassMaskSample >
MembershipSampleGenerator< TInputSample, TClassMaskSample >::OutputPointer
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
  m_Output = OutputType::New() ;
  m_Output->SetSample(m_Input) ;
  typename TClassMaskSample::Iterator iter = m_ClassMask->Begin() ;
  while (iter != m_ClassMask->End())
    {
      classLabel = iter.GetMeasurement(0) ;
      m_Output->AddInstance(classLabel, iter.GetInstanceIdentifier()) ;
      ++iter ;
    }
}

  } // end namespace Statistics
} // end namespace itk

#endif
