/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionPDEDeformableRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMultiResolutionPDEDeformableRegistration_txx
#define _itkMultiResolutionPDEDeformableRegistration_txx
#include "itkMultiResolutionPDEDeformableRegistration.h"

#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkImageRegionIterator.h"
#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::MultiResolutionPDEDeformableRegistration()
{
 
  this->SetNumberOfRequiredInputs(3);

  typename DefaultRegistrationType::Pointer registrator =
    DefaultRegistrationType::New();
  m_RegistrationFilter = static_cast<RegistrationType*>(
    registrator.GetPointer() );

  m_MovingImagePyramid  = MovingImagePyramidType::New();
  m_FixedImagePyramid     = FixedImagePyramidType::New();
  m_FieldExpander     = FieldExpanderType::New();

  m_NumberOfLevels = 3;
  m_NumberOfIterations.resize( m_NumberOfLevels );
  m_FixedImagePyramid->SetNumberOfLevels( m_NumberOfLevels );
  m_MovingImagePyramid->SetNumberOfLevels( m_NumberOfLevels );

  unsigned int ilevel;
  for( ilevel = 0; ilevel < m_NumberOfLevels; ilevel++ )
    {
    m_NumberOfIterations[ilevel] = 10;
    }
  m_CurrentLevel = 0;

}


/*
 * Set the moving image image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::SetMovingImage(
  const MovingImageType * ptr )
{
  this->ProcessObject::SetNthInput( 2, const_cast< MovingImageType * >( ptr ) );
}


/*
 * Get the moving image image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
const typename MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::MovingImageType *
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::GetMovingImage(void)
{
  return dynamic_cast< const MovingImageType * >
    ( this->ProcessObject::GetInput( 2 ) );
}


/*
 * Set the fixed image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::SetFixedImage(
  const FixedImageType * ptr )
{
  this->ProcessObject::SetNthInput( 1, const_cast< FixedImageType * >( ptr ) );
}


/*
 * Get the fixed image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
const typename MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::FixedImageType *
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::GetFixedImage(void)
{
  return dynamic_cast< const FixedImageType * >
    ( this->ProcessObject::GetInput( 1 ) );
}


/*
 * Set the number of multi-resolution levels
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::SetNumberOfLevels(
  unsigned int num )
{
  if( m_NumberOfLevels != num )
    {
    this->Modified();
    m_NumberOfLevels = num;
    m_NumberOfIterations.resize( m_NumberOfLevels );
    }

  if( m_MovingImagePyramid && m_MovingImagePyramid->GetNumberOfLevels() != num )
    {
    m_MovingImagePyramid->SetNumberOfLevels( m_NumberOfLevels );
    }
  if( m_FixedImagePyramid && m_FixedImagePyramid->GetNumberOfLevels() != num )
    {
    m_FixedImagePyramid->SetNumberOfLevels( m_NumberOfLevels );
    }  
    
}


/*
 * Standard PrintSelf method.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NumberOfLevels: " << m_NumberOfLevels << std::endl;
  os << indent << "CurrentLevel: " << m_CurrentLevel << std::endl;

  os << indent << "NumberOfIterations: [";
  unsigned int ilevel;
  for( ilevel = 0; ilevel < m_NumberOfLevels - 1; ilevel++ )
    {
    os << m_NumberOfIterations[ilevel] << ", ";
    }
  os << m_NumberOfIterations[ilevel] << "]" << std::endl;
  
  os << indent << "RegistrationFilter: ";
  os << m_RegistrationFilter.GetPointer() << std::endl;
  os << indent << "MovingImagePyramid: ";
  os << m_MovingImagePyramid.GetPointer() << std::endl;
  os << indent << "FixedImagePyramid: ";
  os << m_FixedImagePyramid.GetPointer() << std::endl;

}

/*
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
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::GenerateData()
{

  // allocate memory for the results
  DeformationFieldPointer outputPtr = this->GetOutput();
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // get a pointer to the MovingImage and test images
  MovingImageConstPointer movingImage = this->GetMovingImage();
  FixedImageConstPointer  fixedImage = this->GetFixedImage();

  if( !movingImage || !fixedImage )
    {
    itkExceptionMacro( << "Fixed and/or moving image not set" );
    }

  if( !m_MovingImagePyramid || !m_FixedImagePyramid )
    {
    itkExceptionMacro( << "Fixed and/or moving pyramid not set" );
    }

  if( !m_RegistrationFilter )
    {
    itkExceptionMacro( << "Registration filter not set" );
    }
  
  // setup the filters
  m_MovingImagePyramid->SetInput( movingImage );
  m_FixedImagePyramid->SetInput( fixedImage );
  m_FieldExpander->SetInput( m_RegistrationFilter->GetOutput() );


  unsigned int movingLevel, fixedLevel;
  unsigned int idim;
  unsigned int lastShrinkFactors[ImageDimension];
  unsigned int expandFactors[ImageDimension];

  DeformationFieldPointer tempField = DeformationFieldType::New();

  for( m_CurrentLevel = 0; m_CurrentLevel < m_NumberOfLevels; 
       m_CurrentLevel++ )
    {
   
    movingLevel = vnl_math_min( (int) m_CurrentLevel, 
                                (int) m_MovingImagePyramid->GetNumberOfLevels() );
    fixedLevel = vnl_math_min( (int) m_CurrentLevel, 
                               (int) m_FixedImagePyramid->GetNumberOfLevels() );

    if( m_CurrentLevel == 0 )
      {
      /*
         * \todo What to do if there is an input deformation field?
         * Will need a VectorMultiResolutionPyramidImageFilter to downsample it.
         */
      //DeformationFieldPointer initialField = this->GetInput();
      m_RegistrationFilter->SetInitialDeformationField( NULL );
      }
    else
      {

      // compute the expand factors for upsampling the deformation field
      for( idim = 0; idim < ImageDimension; idim++ )
        {
        expandFactors[idim] = (unsigned int) ceil( 
          (float) lastShrinkFactors[idim] / 
          (float) m_FixedImagePyramid->GetSchedule()[fixedLevel][idim] );
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

    // Invoke an iteration event.
    this->InvokeEvent( IterationEvent() );

    // setup registration filter and pyramids 
    m_RegistrationFilter->SetMovingImage( m_MovingImagePyramid->GetOutput(movingLevel) );
    m_RegistrationFilter->SetFixedImage( m_FixedImagePyramid->GetOutput(fixedLevel) );

    m_RegistrationFilter->SetNumberOfIterations(
      m_NumberOfIterations[m_CurrentLevel] );

    // cache shrink factors for computing the next expand factors.
    for( idim = 0; idim < ImageDimension; idim++ )
      {
      lastShrinkFactors[idim] = 
        m_FixedImagePyramid->GetSchedule()[fixedLevel][idim];
      }

    if( m_CurrentLevel < m_NumberOfLevels - 1)
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
      
      } // end if m_CurrentLevel
    } // end m_CurrentLevel loop


  // Reset the m_CurrentLevel to zero
  m_CurrentLevel = 0;
}



template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::GenerateOutputInformation()
{

  typename DataObject::Pointer output;

  if( this->GetInput(0) )
    {
    // Initial deformation field is set.
    // Copy information from initial field.
    this->Superclass::GenerateOutputInformation();

    }
  else if( this->GetFixedImage() )
    {
    // Initial deforamtion field is not set. 
    // Copy information from the fixed image.
    for (unsigned int idx = 0; idx < 
           this->GetNumberOfOutputs(); ++idx )
      {
      output = this->GetOutput(idx);
      if (output)
        {
        output->CopyInformation(this->GetFixedImage());
        }  
      }

    }

}


template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
::GenerateInputRequestedRegion()
{

  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the moving image
  MovingImagePointer movingPtr = 
    const_cast< MovingImageType * >( this->GetMovingImage() );
  if( movingPtr )
    {
    movingPtr->SetRequestedRegionToLargestPossibleRegion();
    }
  
  // just propagate up the output requested region for
  // the fixed image and initial deformation field.
  DeformationFieldPointer inputPtr = 
    const_cast< DeformationFieldType * >( this->GetInput() );
  DeformationFieldPointer outputPtr = this->GetOutput();
  FixedImagePointer fixedPtr = 
    const_cast< FixedImageType *>( this->GetFixedImage() );

  if( inputPtr )
    {
    inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

  if( fixedPtr )
    {
    fixedPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

}


template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MultiResolutionPDEDeformableRegistration<TFixedImage,TMovingImage,TDeformationField>
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
