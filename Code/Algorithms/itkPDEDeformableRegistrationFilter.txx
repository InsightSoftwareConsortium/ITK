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

#include "itkExceptionObject.h"
#include "itkImageRegionIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkDataObject.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

#include "vnl/vnl_math.h"

namespace itk {

/**
 * Default constructor
 */
template <class TReference, class TTarget, class TDeformationField>
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::PDEDeformableRegistrationFilter()
{
 
  this->SetNumberOfRequiredInputs(3);

  m_NumberOfIterations = 10;
 
  int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    m_StandardDeviations[j] = 1.0;
    }

  m_TempField = DeformationFieldType::New();
  m_MaximumError = 0.1;

}


/**
 * Set the reference image.
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::SetReference(
const ReferenceType * ptr )
{
  this->ProcessObject::SetNthInput( 1, const_cast< ReferenceType * >( ptr ) );
}


/**
 * Get the reference image.
 */
template <class TReference, class TTarget, class TDeformationField>
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::ReferenceConstPointer
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::GetReference()
{
  return dynamic_cast< const ReferenceType * >
    ( this->ProcessObject::GetInput( 1 ).GetPointer() );
}


/**
 * Set the target image.
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::SetTarget(
const TargetType * ptr )
{
  this->ProcessObject::SetNthInput( 2, const_cast< TargetType * >( ptr ) );
}


/**
 * Get the target image.
 */
template <class TReference, class TTarget, class TDeformationField>
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::TargetConstPointer
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::GetTarget()
{
  return dynamic_cast< TargetType * >
    ( this->ProcessObject::GetInput( 2 ).GetPointer() );
}


/**
 * Set the standard deviations.
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::SetStandardDeviations(
double value )
{

  int j;
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


/**
 * Standard PrintSelf method.
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "No. of Iterations: " << m_NumberOfIterations << std::endl;
  os << indent << "Standard deviations: [";
  int j;
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_StandardDeviations[j] << ", ";
    }
  os << m_StandardDeviations[j] << "]" << std::endl;

}


/**
 * Set the function state values before each iteration
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::InitializeIteration()
{

  ReferenceConstPointer refPtr = this->GetReference();
  TargetConstPointer targetPtr = this->GetTarget();

  if( !refPtr || !targetPtr )
    {
    itkExceptionMacro( << "Reference and/or Target image not set" );
    throw ExceptionObject(__FILE__,__LINE__);
    }

  // update variables in the equation object
  try
    {
    PDEDeformableRegistrationFunctionType *f = 
      dynamic_cast<PDEDeformableRegistrationFunctionType *>
      (this->GetDifferenceFunction().GetPointer());
    f->SetReference( refPtr );
    f->SetTarget( targetPtr );
    this->Superclass::InitializeIteration();           
    }
  catch( ... )
    {
    itkExceptionMacro(<<"FiniteDifferenceFunction not of type PDEDeformableRegistrationFilterFunction");
    throw ExceptionObject( __FILE__, __LINE__ );
    }

  // progress feedback
  if( m_NumberOfIterations <= 0 )
    {
      this->UpdateProgress( 1.0 );
    }
  else if ( m_NumberOfIterations < 100 || !(this->GetElapsedIterations() % 10) )
    {
      this->UpdateProgress( (float) this->GetElapsedIterations() /
        (float) m_NumberOfIterations );
    } 

}


/**
 * Override the default implemenation for the case when the 
 * initial deformation is not set.
 * If the initial deformation is not set, the output is
 * fill with zero vectors.
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::CopyInputToOutput()
{

  typename InputImageType::ConstPointer  inputPtr  = this->GetInput();
  
  if( inputPtr )
    {
    this->Superclass::CopyInputToOutput();
    }
  else
    {
    PixelType zeros;
    for( int j = 0; j < ImageDimension; j++ )
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


template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
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
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::GenerateInputRequestedRegion()
{

  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the reference image
  ReferencePointer refPtr = 
      const_cast< ReferenceType * >( this->GetReference().GetPointer() );
  if( refPtr )
    {
    refPtr->SetRequestedRegionToLargestPossibleRegion();
    }
  
  // just propagate up the output requested region for
  // the target image and initial deformation field.
  DeformationFieldPointer inputPtr = 
      const_cast< DeformationFieldType * >( this->GetInput().GetPointer() );
  DeformationFieldPointer outputPtr = this->GetOutput();
  TargetPointer targetPtr = 
        const_cast< TargetType *>( this->GetTarget().GetPointer() );

  if( inputPtr )
    {
    inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

  if( targetPtr )
    {
    targetPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

}


/**
 * Copy one deformation field into the buffer or another field
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
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


/**
 * Smooth deformation using a separable Gaussian kernel
 */
template <class TReference, class TTarget, class TDeformationField>
void
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::SmoothDeformationField()
{

  DeformationFieldPointer field = this->GetOutput();

  // copy field to TempField
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
  SmootherType::Pointer smoother = SmootherType::New();

  DeformationFieldPointer swapPtr;

  // graft the output field onto the mini-pipeline
  smoother->GraftOutput( field ); 
  swapPtr = m_TempField;

  for( int j = 0; j < ImageDimension; j++ )
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
