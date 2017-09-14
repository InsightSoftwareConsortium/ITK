/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPDEDeformableRegistrationFilter_hxx
#define itkPDEDeformableRegistrationFilter_hxx

#include "itkPDEDeformableRegistrationFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkDataObject.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

#include "itkMath.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::PDEDeformableRegistrationFilter()
{
  Self::RemoveRequiredInputName("Primary");

  // #0 "InitialDisplacementField" optional
  Self::AddOptionalInputName("InitialDisplacementField", 0);

  // #1 "FixedImage" required
  Self::AddRequiredInputName("FixedImage", 1);

  // #2 "MovingImage" required
  Self::AddRequiredInputName("MovingImage", 2);

  this->SetNumberOfIterations(10);

  unsigned int j;
  for ( j = 0; j < ImageDimension; j++ )
    {
    m_StandardDeviations[j] = 1.0;
    m_UpdateFieldStandardDeviations[j] = 1.0;
    }

  m_TempField = DisplacementFieldType::New();
  m_MaximumError = 0.1;
  m_MaximumKernelWidth = 30;
  m_StopRegistrationFlag = false;

  m_SmoothDisplacementField = true;
  m_SmoothUpdateField = false;
}

/*
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
std::vector< SmartPointer< DataObject > >::size_type
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GetNumberOfValidRequiredInputs() const
{
  typename std::vector< SmartPointer< DataObject > >::size_type num = 0;

  if ( this->GetFixedImage() )
    {
    num++;
    }

  if ( this->GetMovingImage() )
    {
    num++;
    }

  return num;
}

/**
 * Set the standard deviations.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SetStandardDeviations(
  double value)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( Math::NotExactlyEquals(value, m_StandardDeviations[j]) )
      {
      break;
      }
    }
  if ( j < ImageDimension )
    {
    this->Modified();
    for ( j = 0; j < ImageDimension; j++ )
      {
      m_StandardDeviations[j] = value;
      }
    }
}

/*
 * Set the standard deviations.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SetUpdateFieldStandardDeviations(
  double value)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( Math::NotExactlyEquals(value, m_UpdateFieldStandardDeviations[j]) )
      {
      break;
      }
    }
  if ( j < ImageDimension )
    {
    this->Modified();
    for ( j = 0; j < ImageDimension; j++ )
      {
      m_UpdateFieldStandardDeviations[j] = value;
      }
    }
}

/*
 * Standard PrintSelf method.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Smooth deformation field: "
     << ( m_SmoothDisplacementField ? "on" : "off" ) << std::endl;
  unsigned int j = 0;
  os << indent << "Standard deviations: [" << m_StandardDeviations[j];
  for ( j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_StandardDeviations[j];
    }
  os << "]" << std::endl;
  os << indent << "Smooth update field: "
     << ( m_SmoothUpdateField ? "on" : "off" ) << std::endl;
  j = 0;
  os << indent << "Update field standard deviations: [" << m_UpdateFieldStandardDeviations[j];
  for ( j = 1; j < ImageDimension; j++ )
    {
    os<< ", " << m_UpdateFieldStandardDeviations[j];
    }
  os << "]" << std::endl;
  os << indent << "StopRegistrationFlag: ";
  os << m_StopRegistrationFlag << std::endl;
  os << indent << "MaximumError: ";
  os << m_MaximumError << std::endl;
  os << indent << "MaximumKernelWidth: ";
  os << m_MaximumKernelWidth << std::endl;
}

/*
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  MovingImageConstPointer movingPtr = this->GetMovingImage();
  FixedImageConstPointer  fixedPtr = this->GetFixedImage();

  if ( !movingPtr || !fixedPtr )
    {
    itkExceptionMacro(<< "Fixed and/or moving image not set");
    }

  // update variables in the equation object
  PDEDeformableRegistrationFunctionType *f =
    dynamic_cast< PDEDeformableRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !f )
    {
    itkExceptionMacro(<< "FiniteDifferenceFunction not of type PDEDeformableRegistrationFilterFunction");
    }

  f->SetFixedImage(fixedPtr);
  f->SetMovingImage(movingPtr);

  this->Superclass::InitializeIteration();
}

/*
 * Override the default implementation for the case when the
 * initial deformation is not set.
 * If the initial deformation is not set, the output is
 * fill with zero vectors.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::CopyInputToOutput()
{
  typename Superclass::InputImageType::ConstPointer inputPtr  = this->GetInput();

  if ( inputPtr )
    {
    this->Superclass::CopyInputToOutput();
    }
  else
    {
    typename Superclass::PixelType zeros;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      zeros[j] = 0;
      }

    typename OutputImageType::Pointer output = this->GetOutput();

    ImageRegionIterator< OutputImageType > out( output, output->GetRequestedRegion() );

    while ( !out.IsAtEnd() )
      {
      out.Value() =  zeros;
      ++out;
      }
    }
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GenerateOutputInformation()
{
  typename DataObject::Pointer output;

  if ( this->GetInput(0) )
    {
    // Initial deformation field is set.
    // Copy information from initial field.
    this->Superclass::GenerateOutputInformation();
    }
  else if ( this->GetFixedImage() )
    {
    // Initial deforamtion field is not set.
    // Copy information from the fixed image.
    for ( unsigned int idx = 0; idx <
          this->GetNumberOfIndexedOutputs(); ++idx )
      {
      output = this->GetOutput(idx);
      if ( output )
        {
        output->CopyInformation( this->GetFixedImage() );
        }
      }
    }
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the moving image
  MovingImagePointer movingPtr =
    const_cast< MovingImageType * >( this->GetMovingImage() );
  if ( movingPtr )
    {
    movingPtr->SetRequestedRegionToLargestPossibleRegion();
    }

  // just propagate up the output requested region for
  // the fixed image and initial deformation field.
  DisplacementFieldPointer inputPtr =
    const_cast< DisplacementFieldType * >( this->GetInput() );
  DisplacementFieldPointer outputPtr = this->GetOutput();
  FixedImagePointer       fixedPtr =
    const_cast< FixedImageType * >( this->GetFixedImage() );

  if ( inputPtr )
    {
    inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }

  if ( fixedPtr )
    {
    fixedPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
    }
}

/*
 * Release memory of internal buffers
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::PostProcessOutput()
{
  this->Superclass::PostProcessOutput();
  m_TempField->Initialize();
}

/*
 * Initialize flags
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::Initialize()
{
  this->Superclass::Initialize();
  m_StopRegistrationFlag = false;
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SmoothDisplacementField()
{
  DisplacementFieldPointer field = this->GetOutput();

  // copy field to TempField
  m_TempField->SetOrigin( field->GetOrigin() );
  m_TempField->SetSpacing( field->GetSpacing() );
  m_TempField->SetDirection( field->GetDirection() );
  m_TempField->SetLargestPossibleRegion(
    field->GetLargestPossibleRegion() );
  m_TempField->SetRequestedRegion(
    field->GetRequestedRegion() );
  m_TempField->SetBufferedRegion( field->GetBufferedRegion() );
  m_TempField->Allocate();

  typedef typename DisplacementFieldType::PixelType      VectorType;
  typedef typename VectorType::ValueType                 ScalarType;
  typedef GaussianOperator< ScalarType, ImageDimension > OperatorType;
  typedef VectorNeighborhoodOperatorImageFilter<
    DisplacementFieldType,
    DisplacementFieldType >                              SmootherType;

  OperatorType *oper = new OperatorType;
  typename SmootherType::Pointer smoother = SmootherType::New();

  typedef typename DisplacementFieldType::PixelContainerPointer
  PixelContainerPointer;
  PixelContainerPointer swapPtr;

  // graft the output field onto the mini-pipeline
  smoother->GraftOutput(m_TempField);

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    // smooth along this dimension
    oper->SetDirection(j);
    double variance = itk::Math::sqr(m_StandardDeviations[j]);
    oper->SetVariance(variance);
    oper->SetMaximumError(m_MaximumError);
    oper->SetMaximumKernelWidth(m_MaximumKernelWidth);
    oper->CreateDirectional();

    // todo: make sure we only smooth within the buffered region
    smoother->SetOperator(*oper);
    smoother->SetInput(field);
    smoother->Update();

    if ( j + 1 < ImageDimension )
      {
      // swap the containers
      swapPtr = smoother->GetOutput()->GetPixelContainer();
      smoother->GraftOutput(field);
      field->SetPixelContainer(swapPtr);
      smoother->Modified();
      }
    }

  // graft the output back to this filter
  m_TempField->SetPixelContainer( field->GetPixelContainer() );
  this->GraftOutput( smoother->GetOutput() );

  delete oper;
}

/*
 * Smooth deformation using a separable Gaussian kernel
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SmoothUpdateField()
{
  // The update buffer will be overwritten with new data.
  DisplacementFieldPointer field = this->GetUpdateBuffer();

  typedef typename DisplacementFieldType::PixelType       VectorType;
  typedef typename VectorType::ValueType                  ScalarType;
  typedef GaussianOperator< ScalarType, ImageDimension >  OperatorType;
  typedef VectorNeighborhoodOperatorImageFilter<
    DisplacementFieldType,
    DisplacementFieldType >                               SmootherType;

  OperatorType opers[ImageDimension];
  typename SmootherType::Pointer smoothers[ImageDimension];

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    // smooth along this dimension
    opers[j].SetDirection(j);
    double variance = itk::Math::sqr(this->GetUpdateFieldStandardDeviations()[j]);
    //double variance = itk::Math::sqr( 1.0 );
    opers[j].SetVariance(variance);
    opers[j].SetMaximumError( this->GetMaximumError() );
    opers[j].SetMaximumKernelWidth( this->GetMaximumKernelWidth() );
    opers[j].CreateDirectional();

    smoothers[j] = SmootherType::New();
    smoothers[j]->SetOperator(opers[j]);
    smoothers[j]->ReleaseDataFlagOn();

    if ( j > 0 )
      {
      smoothers[j]->SetInput( smoothers[j - 1]->GetOutput() );
      }
    }
  smoothers[0]->SetInput(field);
  smoothers[ImageDimension - 1]->GetOutput()
  ->SetRequestedRegion( field->GetBufferedRegion() );

  smoothers[ImageDimension - 1]->Update();

  // field to contain the final smoothed data, do the equivalent of a graft
  field->SetPixelContainer( smoothers[ImageDimension - 1]->GetOutput()
                            ->GetPixelContainer() );
  field->SetRequestedRegion( smoothers[ImageDimension - 1]->GetOutput()
                             ->GetRequestedRegion() );
  field->SetBufferedRegion( smoothers[ImageDimension - 1]->GetOutput()
                            ->GetBufferedRegion() );
  field->SetLargestPossibleRegion( smoothers[ImageDimension - 1]->GetOutput()
                                   ->GetLargestPossibleRegion() );
  field->CopyInformation( smoothers[ImageDimension - 1]->GetOutput() );
}
} // end namespace itk

#endif
