/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMultiResolutionPDEDeformableRegistration.txx
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
#ifndef _itkMultiResolutionPDEDeformableRegistration_txx_
#define _itkMultiResolutionPDEDeformableRegistration_txx_

#include "itkImageRegionIterator.h"
#include "vnl/vnl_math.h"

namespace itk {

/**
 * Default constructor
 */
template <class TReference, class TTarget, class TDeformationField>
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::MultiResolutionPDEDeformableRegistration()
{
 
  this->SetNumberOfRequiredInputs(3);

  typename DefaultRegistrationType::Pointer registrator =
    DefaultRegistrationType::New();
  m_RegistrationFilter = static_cast<RegistrationType*>(
    registrator.GetPointer() );

  m_ReferencePyramid  = ReferencePyramidType::New();
  m_TargetPyramid     = TargetPyramidType::New();
  m_FieldExpander     = FieldExpanderType::New();

  m_NumberOfLevels = 3;
  m_NumberOfIterations.resize( m_NumberOfLevels );
  m_ReferencePyramid->SetNumberOfLevels( m_NumberOfLevels );
  m_TargetPyramid->SetNumberOfLevels( m_NumberOfLevels );

  unsigned int ilevel;
  for( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
    {
    m_NumberOfIterations[ilevel] = 10;
    }

}


/**
 * Set the reference image.
 */
template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::SetReference(
ReferenceType * ptr )
{
  this->ProcessObject::SetNthInput( 1, ptr );
}


/**
 * Get the reference image.
 */
template <class TReference, class TTarget, class TDeformationField>
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::ReferencePointer
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::GetReference()
{
  return static_cast< ReferenceType * >
    ( this->ProcessObject::GetInput( 1 ).GetPointer() );
}


/**
 * Set the target image.
 */
template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::SetTarget(
TargetType * ptr )
{
  this->ProcessObject::SetNthInput( 2, ptr );
}


/**
 * Get the target image.
 */
template <class TReference, class TTarget, class TDeformationField>
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::TargetPointer
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::GetTarget()
{
  return static_cast< TargetType * >
    ( this->ProcessObject::GetInput( 2 ).GetPointer() );
}


/**
 * Set the number of multi-resolution levels
 */
template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::SetNumberOfLevels(
unsigned int num )
{
  if( m_NumberOfLevels != num )
    {
    this->Modified();
    m_NumberOfLevels = num;
    m_NumberOfIterations.resize( m_NumberOfLevels );
    }

  if( m_ReferencePyramid && m_ReferencePyramid->GetNumberOfLevels() != num )
    {
    m_ReferencePyramid->SetNumberOfLevels( m_NumberOfLevels );
    }
  if( m_TargetPyramid && m_TargetPyramid->GetNumberOfLevels() != num )
    {
    m_TargetPyramid->SetNumberOfLevels( m_NumberOfLevels );
    }  
    
}


/**
 * Standard PrintSelf method.
 */
template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "No. of Levels: " << m_NumberOfLevels << std::endl;

  os << indent << "No. of Iterations: [";
  unsigned int ilevel;
  for( ilevel = 0; ilevel < m_NumberOfLevels - 1; ilevel++ )
    {
    os << m_NumberOfIterations[ilevel] << ", ";
    }
  os << m_NumberOfIterations[ilevel] << "]" << std::endl;
  
  os << indent << "RegistrationFilter: " << m_RegistrationFilter << std::endl;
  os << indent << "ReferencePyramid: " << m_ReferencePyramid << std::endl;
  os << indent << "TargetPyramid: " << m_TargetPyramid << std::endl;

}

/**
 * Perform a the deformable registration using a multiresolution scheme
 * using an internal mini-pipeline
 *
 *  ref_pyramid ->  registrator  ->  field_expander --|| tempField
 * test_pyramid ->           |                              |
 *                           |                              |
 *                           --------------------------------    
 *
 * A tempField image is used to break the cycle between the
 * registrator and field_expander.
 *
 */                              
template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::GenerateData()
{

  // allocate memory for the results
  DeformationFieldPointer outputPtr = this->GetOutput();
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // get a pointer to the reference and test images
  ReferencePointer reference = this->GetReference();
  TargetPointer target = this->GetTarget();

  if( !reference || !target )
    {
    itkErrorMacro( << "Reference and/or Target not set" );
    }

  if( !m_ReferencePyramid || !m_TargetPyramid )
    {
    itkErrorMacro( << "Reference and/or Target Pyramid not set" );
    }

  if( !m_RegistrationFilter )
    {
    itkErrorMacro( << "Registration filter not set" );
    }
  
  m_ReferencePyramid->SetInput( reference );
  m_TargetPyramid->SetInput( target );
  m_RegistrationFilter->SetReference( m_ReferencePyramid->GetOutput() );
  m_RegistrationFilter->SetTarget( m_TargetPyramid->GetOutput() );
  m_RegistrationFilter->SetInitialDeformationField( NULL );
  m_FieldExpander->SetInput( m_RegistrationFilter->GetOutput() );

 /**
   * \todo What to do if there is an input deformation field?
   * Will need a VectorMultiResolutionImagePyramid to downsample it.
   */
  //DeformationFieldPointer initialField = this->GetInput();

  unsigned int ilevel, refLevel, targetLevel;
  int idim;
  unsigned int lastShrinkFactors[ImageDimension];
  unsigned int expandFactors[ImageDimension];

  DeformationFieldPointer tempField = DeformationFieldType::New();

  for( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
    {
    this->UpdateProgress( (float) ilevel / (float) m_NumberOfLevels );
   
    refLevel = vnl_math_min( (int) ilevel, 
      (int) m_ReferencePyramid->GetNumberOfLevels() );
    targetLevel = vnl_math_min( (int) ilevel, 
      (int) m_TargetPyramid->GetNumberOfLevels() );

    if( ilevel > 0 )
      {

      // compute the expand factors for upsampling the deformation field
      for( idim = 0; idim < ImageDimension; idim++ )
        {
        expandFactors[idim] = (unsigned int) ceil( 
         (float) lastShrinkFactors[idim] / 
         (float) m_TargetPyramid->GetSchedule()[targetLevel][idim] );
        if( expandFactors[idim] < 1 )
          {
          expandFactors[idim] = 1;
          }  
        }

      // graft a temporary image as the output of the expander
      // this is used to break the loop between the registrator
      // and expander
      m_FieldExpander->GraftOutput( tempField );
      m_FieldExpander->SetExpandFactors( expandFactors ); 
      m_FieldExpander->UpdateLargestPossibleRegion();

      tempField = m_FieldExpander->GetOutput();
      tempField->DisconnectPipeline();
      m_RegistrationFilter->SetInitialDeformationField( tempField );

      }

    // setup registration filter and pyramids 
    m_ReferencePyramid->SetCurrentLevel( refLevel );
    m_TargetPyramid->SetCurrentLevel( targetLevel );

    m_RegistrationFilter->SetNumberOfIterations(
      m_NumberOfIterations[ilevel] );

    // cache shrink factors for computing the next expand factors.
    for( idim = 0; idim < ImageDimension; idim++ )
      {
      lastShrinkFactors[idim] = 
        m_TargetPyramid->GetSchedule()[targetLevel][idim];
      }

    if( ilevel < m_NumberOfLevels - 1)
      {

      // compute new deformation field
      m_RegistrationFilter->UpdateLargestPossibleRegion();

      }
    else
      {

      // this is the last level
      for( idim = 0; idim < ImageDimension; idim++ )
        {
        if ( lastShrinkFactors[idim] > 1 ) { break; }
        }
      if( idim < ImageDimension )
        {

        // some of the last shrink factors are not one
        // graft the output of the expander filter to
        // to output of this filter
        m_FieldExpander->GraftOutput( outputPtr );
        m_FieldExpander->SetExpandFactors( lastShrinkFactors );
        m_FieldExpander->UpdateLargestPossibleRegion();
        this->GraftOutput( m_FieldExpander->GetOutput() );

        }
      else
        {

        // all the last shrink factors are all ones
        // graft the output of registration filter to
        // to output of this filter
        m_RegistrationFilter->GraftOutput( outputPtr );
        m_RegistrationFilter->UpdateLargestPossibleRegion();
        this->GraftOutput( m_RegistrationFilter->GetOutput() );

        }
      
      } // end if ilevel
    } // end ilevel loop


}



template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::GenerateOutputInformation()
{

 typename DataObject::Pointer output;

 if( this->GetInput(0) )
  {
  // Initial deformation field is set.
  // Copy information from initial field.
  this->Superclass::GenerateOutputInformation();

  }
 else if( this->GetInput(2) )
  {
  // Initial deforamtion field is not set. 
  // Copy information from the target image.
  for (unsigned int idx = 0; idx < 
    this->GetNumberOfOutputs(); ++idx )
    {
    output = this->GetOutput(idx);
    if (output)
      {
      output->CopyInformation(this->GetInput(2));
      }  
    }

  }

}


template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::GenerateInputRequestedRegion()
{

  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the reference, 
  // target and initial deformation field
  ReferencePointer refPtr = this->GetReference();
  if( refPtr )
    {
    refPtr->SetRequestedRegionToLargestPossibleRegion();
    }
  
  TargetPointer targetPtr = this->GetTarget();
  if( targetPtr )
    {
    targetPtr->SetRequestedRegionToLargestPossibleRegion();
    }

  DeformationFieldPointer inputPtr = this->GetInput();
  if( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }

}


template <class TReference, class TTarget, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TReference,TTarget,TDeformationField>
::EnlargeOutputRequestedRegion(
DataObject * ptr )
{
  // call the superclass's implementation
  Superclass::EnlargeOutputRequestedRegion( ptr );

  // set the output requested region to largest possible.
  DeformationFieldType * outputPtr;
  outputPtr = dynamic_cast<DeformationFieldType*>( ptr );

  if( outputPtr )
    {
    outputPtr->SetRequestedRegionToLargestPossibleRegion();
    }

}


} // end namespace itk

#endif
