/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPDEDeformableRegistrationFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPDEDeformableRegistrationFilter_txx_
#define _itkPDEDeformableRegistrationFilter_txx_

#include "itkPDEDeformableRegistrationFilter.h"

#include "itkExceptionObject.h"
#include "itkImageRegionIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkDataObject.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::PDEDeformableRegistrationFilter()
{
 
  this->SetNumberOfRequiredInputs(3);

  m_NumberOfIterations = 10;
 
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    m_StandardDeviations[j] = 1.0;
    }

  m_TempField = DeformationFieldType::New();
  m_MaximumError = 0.1;

}


/*
 * Set the fixed image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::SetFixedImage(
  const FixedImageType * ptr )
{
  this->ProcessObject::SetNthInput( 1, const_cast< FixedImageType * >( ptr ) );
}


/*
 * Get the fixed image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
const typename PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::FixedImageType *
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::GetFixedImage()
{
  return dynamic_cast< const FixedImageType * >
    ( this->ProcessObject::GetInput( 1 ) );
}


/*
 * Set the moving image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::SetMovingImage(
  const MovingImageType * ptr )
{
  this->ProcessObject::SetNthInput( 2, const_cast< MovingImageType * >( ptr ) );
}


/*
 * Get the moving image.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
const typename PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::MovingImageType *
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::GetMovingImage()
{
  return dynamic_cast< MovingImageType * >
    ( this->ProcessObject::GetInput( 2 ) );
}


/*
 * Set the standard deviations.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::SetStandardDeviations(
  double value )
{

  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    if( value != m_StandardDeviations[j] )
      {
      break;
      }
    }
  if( j < ImageDimension )
    {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
      {
      m_StandardDeviations[j] = value;
      }
    }

}


/*
 * Standard PrintSelf method.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "No. of Iterations: " << m_NumberOfIterations << std::endl;
  os << indent << "Standard deviations: [";
  unsigned int j;
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_StandardDeviations[j] << ", ";
    }
  os << m_StandardDeviations[j] << "]" << std::endl;

}


/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{

  MovingImageConstPointer movingPtr = this->GetMovingImage();
  FixedImageConstPointer fixedPtr = this->GetFixedImage();

  if( !movingPtr || !fixedPtr )
    {
    itkExceptionMacro( << "Fixed and/or moving image not set" );
    }

  // update variables in the equation object
  PDEDeformableRegistrationFunctionType *f = 
    dynamic_cast<PDEDeformableRegistrationFunctionType *>
    (this->GetDifferenceFunction().GetPointer());

  if ( !f )
    {
    itkExceptionMacro(<<"FiniteDifferenceFunction not of type PDEDeformableRegistrationFilterFunction");
    }

  f->SetFixedImage( fixedPtr );
  f->SetMovingImage( movingPtr );

  this->Superclass::InitializeIteration();           

  // progress feedback
  if ( m_NumberOfIterations != 0 )
    {
    this->UpdateProgress(((float)(this->GetElapsedIterations()))
                         /((float)(m_NumberOfIterations)));
    }
  else 
    {
    this->UpdateProgress(0);
    }

}


/*
 * Override the default implemenation for the case when the 
 * initial deformation is not set.
 * If the initial deformation is not set, the output is
 * fill with zero vectors.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::CopyInputToOutput()
{

  typename InputImageType::ConstPointer  inputPtr  = this->GetInput();
  
  if( inputPtr )
    {
    this->Superclass::CopyInputToOutput();
    }
  else
    {
    typename Superclass::PixelType zeros;
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      zeros[j] = 0;
      }

    typename OutputImageType::Pointer output = this->GetOutput();
  
    ImageRegionIterator<OutputImageType> out(output, output->GetRequestedRegion());

    while( ! out.IsAtEnd() )
      {
      out.Value() =  zeros;
      ++out;
      }
    }
}


template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
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
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
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


/*
 * Copy one deformation field into the buffer or another field
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::CopyDeformationField(
  DeformationFieldType * input,
  DeformationFieldType * output
  )
{
  typedef ImageRegionIterator<DeformationFieldType> Iterator;
  Iterator inIter( input, output->GetBufferedRegion() );
  Iterator outIter( output, output->GetBufferedRegion() );

  for( ; !inIter.IsAtEnd(); ++inIter, ++outIter )
    {
    outIter.Set( inIter.Get() );
    }

}


/*
 * Smooth deformation using a separable Gaussian kernel
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
PDEDeformableRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::SmoothDeformationField()
{

  DeformationFieldPointer field = this->GetOutput();

  // copy field to TempField
  m_TempField->SetSpacing( field->GetSpacing() );
  m_TempField->SetOrigin( field->GetOrigin() );
  m_TempField->SetLargestPossibleRegion( 
    field->GetLargestPossibleRegion() );
  m_TempField->SetRequestedRegion(
    field->GetRequestedRegion() );
  m_TempField->SetBufferedRegion( field->GetBufferedRegion() );
  m_TempField->Allocate();

  this->CopyDeformationField( field, m_TempField );
  
  typedef typename DeformationFieldType::PixelType VectorType;
  typedef typename VectorType::ValueType           ScalarType;
  typedef GaussianOperator<ScalarType,ImageDimension> OperatorType;
  typedef VectorNeighborhoodOperatorImageFilter<
    DeformationFieldType,
    DeformationFieldType> SmootherType;

  OperatorType * oper = new OperatorType;
  typename SmootherType::Pointer smoother = SmootherType::New();

  DeformationFieldPointer swapPtr;

  // graft the output field onto the mini-pipeline
  smoother->GraftOutput( field ); 
  swapPtr = m_TempField;

  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    // smooth along this dimension
    oper->SetDirection( j );
    double variance = vnl_math_sqr( m_StandardDeviations[j] );
    oper->SetVariance( variance );
    oper->SetMaximumError( m_MaximumError );
    oper->CreateDirectional();

    // todo: make sure we only smooth within the buffered region
    smoother->SetOperator( *oper );
    smoother->SetInput( swapPtr );
    smoother->Update();

    swapPtr = smoother->GetOutput();
    swapPtr->DisconnectPipeline();

    }

  // graft the output back to this filter
  this->GraftOutput( swapPtr );

  delete oper;
  
}



} // end namespace itk

#endif
