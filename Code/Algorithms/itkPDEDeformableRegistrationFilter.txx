/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkPDEDeformableRegistrationFilter.txx
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
ReferenceType * ptr )
{
  this->ProcessObject::SetNthInput( 1, ptr );
}


/**
 * Get the reference image.
 */
template <class TReference, class TTarget, class TDeformationField>
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::ReferencePointer
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
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
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::SetTarget(
TargetType * ptr )
{
  this->ProcessObject::SetNthInput( 2, ptr );
}


/**
 * Get the target image.
 */
template <class TReference, class TTarget, class TDeformationField>
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::TargetPointer
PDEDeformableRegistrationFilter<TReference,TTarget,TDeformationField>
::GetTarget()
{
  return static_cast< TargetType * >
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

  ReferencePointer refPtr = this->GetReference();
  TargetPointer targetPtr = this->GetTarget();

  if( !refPtr || !targetPtr )
    {
    itkErrorMacro( << "Reference and/or Target image not set" );
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
    itkErrorMacro(<<"FiniteDifferenceFunction not of type PDEDeformableRegistrationFilterFunction");
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

  typename InputImageType::Pointer  inputPtr  = this->GetInput();
  
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
  ReferencePointer refPtr = this->GetReference();
  if( refPtr )
    {
    refPtr->SetRequestedRegionToLargestPossibleRegion();
    }
  
  // just propagate up the output requested region for
  // the target image and initial deformation field.
  DeformationFieldPointer inputPtr = this->GetInput();
  DeformationFieldPointer outputPtr = this->GetOutput();
  TargetPointer targetPtr = this->GetTarget();

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
