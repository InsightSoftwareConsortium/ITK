/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionRegistration.txx
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
#ifndef _itkMultiResolutionRegistration_txx
#define _itkMultiResolutionRegistration_txx

#include "itkMultiResolutionRegistration.h"

namespace itk
{


/**
 * Constructor
 */
template <class TTraits>
MultiResolutionRegistration<TTraits>
::MultiResolutionRegistration()
{
  m_TargetPyramid = TargetPyramidType::New();
  m_ReferencePyramid = ReferencePyramidType::New();
  m_InternalRegistrationMethod = RegistrationType::New();

  this->SetNumberOfLevels( 2 );
  m_CurrentLevel = 0;

}


/**
 * Set the number of computation levels
 */
template <class TRegistrationMethod>
void
MultiResolutionRegistration<TRegistrationMethod>
::SetNumberOfLevels(
unsigned int num )
{
  // clamp value to be at least one
  m_NumberOfLevels = num;
  if( m_NumberOfLevels == 0 ) m_NumberOfLevels = 1;

  m_TargetPyramid->SetNumberOfLevels( m_NumberOfLevels );
  m_ReferencePyramid->SetNumberOfLevels( m_NumberOfLevels );

  this->Modified();
}


/**
 * Set the target
 */
template <class TRegistrationMethod>
void
MultiResolutionRegistration<TRegistrationMethod>
::SetTarget(
TargetType * ptr )
{
  if( m_TargetPyramid->GetInput() != ptr )
    {
    m_TargetPyramid->SetInput( ptr );
    this->Modified();
    }
}


/**
 * Get the target
 */
template <class TRegistrationMethod>
MultiResolutionRegistration<TRegistrationMethod>::TargetConstPointer
MultiResolutionRegistration<TRegistrationMethod>
::GetTarget()
{
  return m_TargetPyramid->GetInput();
}


/**
 * Set the reference
 */
template <class TRegistrationMethod>
void
MultiResolutionRegistration<TRegistrationMethod>
::SetReference(
ReferenceType * ptr )
{
  if( m_ReferencePyramid->GetInput() != ptr )
    {
    m_ReferencePyramid->SetInput( ptr );
    this->Modified();
    }
}



/**
 * Get the target
 */
template <class TRegistrationMethod>
MultiResolutionRegistration<TRegistrationMethod>::ReferenceConstPointer
MultiResolutionRegistration<TRegistrationMethod>
::GetReference()
{
  return m_ReferencePyramid->GetInput();
}

/**
 * Start the registration
 */
template <class TRegistrationMethod>
void
MultiResolutionRegistration<TRegistrationMethod>
::StartRegistration()
{
  for( m_CurrentLevel = 0; m_CurrentLevel < m_NumberOfLevels; 
    m_CurrentLevel++ )
    {
    
    this->OneLevelPreRegistration( m_CurrentLevel );
    this->OneLevelRegistration( m_CurrentLevel );
    this->OneLevelPostRegistration( m_CurrentLevel );
        
    }

}



/**
 * OneLevelRegistration
 */
template <class TRegistrationMethod>
void
MultiResolutionRegistration<TRegistrationMethod>
::OneLevelRegistration(
unsigned int level )
{

  std::cout << "Registering level: " << level << std::endl;

  m_TargetPyramid->SetCurrentLevel( level );
  m_TargetPyramid->Update();
  m_InternalRegistrationMethod->SetTarget( m_TargetPyramid->GetOutput() );

  m_ReferencePyramid->SetCurrentLevel( level );
  m_ReferencePyramid->Update();
  m_InternalRegistrationMethod->SetReference( m_ReferencePyramid->GetOutput() );

  m_InternalRegistrationMethod->StartRegistration();

}


/**
 * PrintSelf method
 */
template <class TRegistrationMethod>
void
MultiResolutionRegistration<TRegistrationMethod>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "No. levels: " << m_NumberOfLevels << std::endl;

}



} // namespace itk

#endif
