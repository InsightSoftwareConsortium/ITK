/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionMutualInformationAffineRegistration.txx
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
#ifndef _itkMultiResolutionMutualInformationAffineRegistration_txx
#define _itkMultiResolutionMutualInformationAffineRegistration_txx

#include "itkMultiResolutionMutualInformationAffineRegistration.h"

namespace itk
{


/**
 * Constructor
 */
template <class TReference, class TTarget>
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::MultiResolutionMutualInformationAffineRegistration()
{

}


/**
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
  m_TranslationScales.resize( num );
}


/**
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

  this->GetInternalRegistrationMethod()->SetTranslationScale(
    m_TranslationScales[ level ] );

  // set up the initial solution
  typename RegistrationType::ParametersType parameters =
		this->GetInternalRegistrationMethod()->GetParameters();

  // correct for change in translation scale parameters
  double correction;
  if( level == 0 )
    {
      correction = 1.0 / m_TranslationScales[ level ];
    }
  else
    {
      correction = m_TranslationScales[ level - 1 ] /
      m_TranslationScales[ level ];
    }

  int start = TargetImageDimension*TargetImageDimension;
  int end   = start + TargetImageDimension;
  for( int j = start; j < end; j++ )
    {
      parameters[j] *= correction;
    }
    this->GetInternalRegistrationMethod()->SetParameters( parameters );

}
 

/**
 * OneLevelPostRegistration
 */
template <class TReference, class TTarget>
void
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::OneLevelPostRegistration(
unsigned int level )
{

  if( level == this->GetNumberOfLevels() - 1 )
    {

    typename RegistrationType::ParametersType parameters =
	  	this->GetInternalRegistrationMethod()->GetParameters();

    double correction = m_TranslationScales[ level ];
    int start = TargetImageDimension*TargetImageDimension;
    int end   = start + TargetImageDimension;
    for( int j = start; j < end; j++ )
      {
	parameters[j] *= correction;
      }

    this->GetInternalRegistrationMethod()->SetParameters( parameters );

    }

}
 


/**
 * PrintSelf method
 */
template <class TReference, class TTarget>
void
MultiResolutionMutualInformationAffineRegistration<TReference,TTarget>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Multi-resolution mutual information registration" << std::endl;

}



} // namespace itk

#endif
