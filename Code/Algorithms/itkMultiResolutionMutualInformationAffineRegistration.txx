/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionMutualInformationAffineRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMultiResolutionMutualInformationAffineRegistration_txx
#define _itkMultiResolutionMutualInformationAffineRegistration_txx

#ifdef COMPILE_LONG_NAMES_REGISTRATION_CLASSES
#include "itkMultiResolutionMutualInformationAffineRegistration.h"

namespace itk
{


/*
 * Constructor
 */
template <class TReference, class TTarget>
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::MultiResolutionMutualInformationAffineRegistration()
{

}


/*
 * Set number of levels
 */
template <class TReference, class TTarget>
void
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::SetNumberOfLevels( unsigned int num )
{
  this->Superclass::SetNumberOfLevels( num );
  m_NumberOfIterations.resize( num );
  m_LearningRates.resize( num );
}


/*
 * OneLevelPreRegistration
 */
template <class TReference, class TTarget>
void
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::OneLevelPreRegistration(
unsigned int level )
{

  // set the registration parameters
  this->GetInternalRegistrationMethod()->SetNumberOfIterations(
    m_NumberOfIterations[ level ] );

  this->GetInternalRegistrationMethod()->SetLearningRate(
    m_LearningRates[ level ] );

}
 

/*
 * OneLevelPostRegistration
 */
template <class TReference, class TTarget>
void
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::OneLevelPostRegistration(
unsigned int level )
{

}
 


/*
 * PrintSelf method
 */
template <class TReference, class TTarget>
void
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int j;
  os << indent << "Learning rates: [";
  for( j = 0; j < this->GetNumberOfLevels() - 1; j++ )
    {
    os << m_LearningRates[j] << ", ";
    }
  os << m_LearningRates[j] << "]" << std::endl;

  os << indent << "No. iterations: [";
  for( j = 0; j < this->GetNumberOfLevels() - 1; j++ )
    {
    os << m_NumberOfIterations[j] << ", ";
    }
  os << m_NumberOfIterations[j] << "]" << std::endl;
 
}



} // namespace itk

#endif
#endif
