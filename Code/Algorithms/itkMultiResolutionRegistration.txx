/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  // construct the image pyramids
  itkDebugMacro( << "Building image pyramids" );
  m_TargetPyramid->UpdateLargestPossibleRegion();
  m_ReferencePyramid->UpdateLargestPossibleRegion();


  for( m_CurrentLevel = 0; m_CurrentLevel < m_NumberOfLevels; 
    m_CurrentLevel++ )
    {
    
    // invoke an iteration event    
    this->InvokeEvent( IterationEvent() );

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
::OneLevelRegistration(unsigned int level )
{

  itkDebugMacro( << "Registering level " << level  );

  unsigned int targetLevel = vnl_math_min( 
    level, m_TargetPyramid->GetNumberOfLevels() );

  unsigned int referenceLevel = vnl_math_min(
    level, m_ReferencePyramid->GetNumberOfLevels() );

  m_InternalRegistrationMethod->SetTarget( 
    m_TargetPyramid->GetOutput(targetLevel) );

  m_InternalRegistrationMethod->SetReference( 
    m_ReferencePyramid->GetOutput(referenceLevel) );

  m_InternalRegistrationMethod->StartRegistration();

}


/**
 * PrintSelf method
 */
template <class TRegistrationMethod>
void
MultiResolutionRegistration<TRegistrationMethod>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "No. levels: " << m_NumberOfLevels << std::endl;
  os << indent << "Internal registration method: ";
  os << m_InternalRegistrationMethod.GetPointer() << std::endl;
  os << indent << "Target pyramid: " << m_TargetPyramid.GetPointer();
  os << std::endl;
  os << indent << "Reference pyramid: " << m_ReferencePyramid.GetPointer();
  os << std::endl;
  os << indent << "Current level: " << m_CurrentLevel << std::endl;

}



} // namespace itk

#endif
