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
#ifndef itkPhaseCorrelationImageRegistrationMethod_hxx
#define itkPhaseCorrelationImageRegistrationMethod_hxx

#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMath.h"

namespace itk
{

template < typename TFixedImage, typename TMovingImage >
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::PhaseCorrelationImageRegistrationMethod()
{
  this->SetNumberOfRequiredInputs( 2 );
  this->SetNumberOfRequiredOutputs( 2 );  // for 0-the Transform, 1-the phase correlation image

  m_FixedImage = nullptr; // has to be provided by the user.
  m_MovingImage = nullptr; // has to be provided by the user.
  m_Operator = nullptr; // has to be provided by the user.
  m_RealOptimizer = nullptr; // has to be provided by the user.
  m_ComplexOptimizer = nullptr; // has to be provided by the user.

  m_FixedPadder = FixedPadderType::New();
  m_MovingPadder = MovingPadderType::New();
  m_FixedFFT = FFTFilterType::New();
  m_MovingFFT = FFTFilterType::New();
  m_IFFT = IFFTFilterType::New();
  m_CropFilter = CropFilterType::New();

  m_FixedPadder->SetConstant( 0 );
  m_MovingPadder->SetConstant( 0 );

  m_FixedFFT->SetInput( m_FixedPadder->GetOutput() );
  m_MovingFFT->SetInput( m_MovingPadder->GetOutput() );

  m_CropFilter->SetInput( m_IFFT->GetOutput() );

  m_TransformParameters = ParametersType(ImageDimension);
  m_TransformParameters.Fill( 0.0f );

  TransformOutputPointer transformDecorator =
                 static_cast< TransformOutputType * >(
                                  this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );
  std::cout << "output is " << this->GetOutput()->Get() << std::endl;

  typename RealImageType::Pointer phaseCorrelation = static_cast< RealImageType * >( this->MakeOutput( 1 ).GetPointer() );
  this->ProcessObject::SetNthOutput( 1, phaseCorrelation.GetPointer() );
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::Initialize()
{
  itkDebugMacro( "initializing registration" );

  if( !m_FixedImage )
    {
    itkExceptionMacro(<<"FixedImage is not present");
    }

  if( !m_MovingImage )
    {
    itkExceptionMacro(<<"MovingImage is not present");
    }

  if ( !m_Operator )
    {
    itkExceptionMacro(<<"Operator is not present" );
    }

  if ( !m_RealOptimizer && !m_ComplexOptimizer)
    {
    itkExceptionMacro(<<"Optimizer is not present" );
    }


  //
  // Connect new transform to the Decorator if necessary.
  //
  TransformOutputPointer transformOutput(
    static_cast<TransformOutputType *>( this->ProcessObject::GetOutput(0) ));
  TransformPointer transform (
    const_cast<TransformType *>(transformOutput->Get()));

  if (transform.IsNull())
    {
    transform = TransformType::New();
    transformOutput->Set( transform.GetPointer() );
    }

  //
  // set up the pipeline
  //
  m_FixedPadder->SetInput( m_FixedImage );
  m_MovingPadder->SetInput( m_MovingImage );
  m_Operator->SetFixedImage( m_FixedFFT->GetOutput() );
  m_Operator->SetMovingImage( m_MovingFFT->GetOutput() );
  if ( m_RealOptimizer )
    {
    m_IFFT->SetInput( m_Operator->GetOutput() );
    m_RealOptimizer->SetInput( m_CropFilter->GetOutput() );
    }
  else
    {
    m_ComplexOptimizer->SetInput( m_Operator->GetOutput() );
    }
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::DeterminePadding()
{
  //
  //set up padding to resize the images to cover the same real-space area
  //
  typename FixedImageType::SizeType fixedSize =
      m_FixedImage->GetLargestPossibleRegion().GetSize();
  typename MovingImageType::SizeType movingSize =
      m_MovingImage->GetLargestPossibleRegion().GetSize();

  typename FixedImageType::SizeType fixedPad;
  fixedPad.Fill( 0 );
  typename MovingImageType::SizeType movingPad;
  movingPad.Fill( 0 );

  typename CropFilterType::SizeType croppingSize;

  const SizeValueType sizeGreatestPrimeFactor = m_FixedFFT->GetSizeGreatestPrimeFactor();

  for (unsigned int ii = 0; ii < ImageDimension; ++ii)
    {
    // First, pad so both images have the same size
    if( fixedSize[ii] == movingSize[ii] )
      {
      // no padding required
      }
    else if( fixedSize[ii] > movingSize[ii] )
      {
      movingPad[ii] = fixedSize[ii] - movingSize[ii];
      }
    else
      {
      fixedPad[ii] = movingSize[ii] - fixedSize[ii];
      }

    // Next, pad for the requirements of the FFT filter
    if( sizeGreatestPrimeFactor > 1 )
      {
      while( Math::GreatestPrimeFactor( fixedSize[ii] + fixedPad[ii] ) > sizeGreatestPrimeFactor )
        {
        ++fixedPad[ii];
        ++movingPad[ii];
        }
      }
    else if( sizeGreatestPrimeFactor == 1 )
      {
      // make sure the total size is even
      fixedPad[ii] += ( fixedSize[ii] + fixedPad[ii] ) % 2;
      movingPad[ii] += ( movingSize[ii] + movingPad[ii] ) % 2;
      }

    // Crop back down to larger of the fixed or moving images
    if( fixedSize[ii] > movingSize[ii] )
      {
      croppingSize[ii] = fixedPad[ii];
      }
    else
      {
      croppingSize[ii] = movingPad[ii];
      }
    }

  m_FixedPadder->SetPadUpperBound( fixedPad );
  m_MovingPadder->SetPadUpperBound( movingPad );

  m_CropFilter->SetUpperBoundaryCropSize( croppingSize );
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::StartOptimization()
{
  ParametersType empty(ImageDimension);
  empty.Fill( 0.0 );
  m_TransformParameters = empty;
  itkDebugMacro( "starting optimization" );
  typedef typename RealOptimizerType::OffsetType OffsetType;
  OffsetType offset;
  try
    {
    RealImageType * phaseCorrelation =  static_cast< RealImageType * >( this->ProcessObject::GetOutput(1) );
    phaseCorrelation->Allocate();
    m_CropFilter->GraftOutput( phaseCorrelation );
    m_CropFilter->Update();
    if ( m_RealOptimizer )
      {
      m_RealOptimizer->Update();
      offset = m_RealOptimizer->GetOffset();
      }
    else
      {
      m_ComplexOptimizer->Update();
      offset = m_ComplexOptimizer->GetOffset();
      }
    phaseCorrelation->Graft( m_CropFilter->GetOutput() );
    }
  catch( ExceptionObject& err )
    {
    // Pass exception to caller
    itkDebugMacro( "exception caught during optimization - passing" );
    throw err;
    }
  itkDebugMacro( "optimization finished" );

  //
  // now offset is a real-coordinate shift between the two images
  // but with origin offset not included
  m_TransformParameters = ParametersType( ImageDimension );
  typename FixedImageType::PointType fixedOrigin = m_FixedImage->GetOrigin();
  typename MovingImageType::PointType movingOrigin = m_MovingImage->GetOrigin();
  for( unsigned int i = 0; i < ImageDimension; ++i )
    {
    m_TransformParameters[i] = offset[i] - ( movingOrigin[i] - fixedOrigin[i] );
    }

  // set the output transform
  TransformOutputType * transformOutput =
     static_cast< TransformOutputType * >( this->ProcessObject::GetOutput(0) );
  TransformPointer transform(
     const_cast<TransformType * > (transformOutput->Get()));
  transform->SetParameters( m_TransformParameters );

  itkDebugMacro( "output set to " << transform );
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Operator: " << m_Operator.GetPointer() << std::endl;
  os << indent << "Real Optimizer: " << m_RealOptimizer.GetPointer()
               << std::endl;
  os << indent << "Complex Optimizer: " << m_ComplexOptimizer.GetPointer()
               << std::endl;
  os << indent << "Fixed Image: " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "Transform Parameters: " << m_TransformParameters
               << std::endl;

  typename TransformType::ConstPointer t(this->GetOutput()->Get());
  os << indent << "Output transform: " << t.GetPointer() << std::endl;
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  this->Initialize();
  this->DeterminePadding();

  m_CropFilter->UpdateOutputInformation();

  RealImageType * phaseCorrelation = static_cast< RealImageType * >( this->ProcessObject::GetOutput( 1 ) );
  phaseCorrelation->CopyInformation( m_CropFilter->GetOutput() );
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::GenerateData()
{
  this->Initialize();
  this->StartOptimization();
}


template < typename TFixedImage, typename TMovingImage >
const typename PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
    ::TransformOutputType *
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::GetOutput() const
{
  return static_cast< const TransformOutputType * >(
                                            this->ProcessObject::GetOutput(0) );
}


template < typename TFixedImage, typename TMovingImage >
const typename PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
    ::RealImageType *
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::GetPhaseCorrelationImage() const
{
  return static_cast< const RealImageType * >(
                                            this->ProcessObject::GetOutput(1) );
}


template < typename TFixedImage, typename TMovingImage >
DataObject::Pointer
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::MakeOutput(DataObjectPointerArraySizeType output)
{
  switch (output)
    {
    case 0:
      return static_cast<DataObject*>(TransformOutputType::New().GetPointer());
      break;
    case 1:
      return static_cast<DataObject*>(RealImageType::New().GetPointer());
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
    }
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::SetFixedImage( const FixedImageType * fixedImage )
{
  itkDebugMacro("setting Fixed Image to " << fixedImage );

  if (this->m_FixedImage.GetPointer() != fixedImage )
    {
    this->m_FixedImage = fixedImage;

    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(0,
                                  const_cast< FixedImageType *>( fixedImage ) );

    this->Modified();
    }
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::SetMovingImage( const MovingImageType * movingImage )
{
  itkDebugMacro("setting Moving Image to " << movingImage );

  if (this->m_MovingImage.GetPointer() != movingImage )
    {
    this->m_MovingImage = movingImage;

    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(1,
                                const_cast< MovingImageType *>( movingImage ) );

    this->Modified();
    }
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::SetReleaseDataFlag( bool a_flag )
{
  Superclass::SetReleaseDataFlag( a_flag );
  m_FixedPadder->SetReleaseDataFlag( a_flag );
  m_MovingPadder->SetReleaseDataFlag( a_flag );
  m_FixedFFT->SetReleaseDataFlag( a_flag );
  m_MovingFFT->SetReleaseDataFlag( a_flag );
  m_IFFT->SetReleaseDataFlag( a_flag );
  m_CropFilter->SetReleaseDataFlag( a_flag );
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::SetReleaseDataBeforeUpdateFlag( bool a_flag )
{
  Superclass::SetReleaseDataBeforeUpdateFlag( a_flag );
  m_FixedPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_MovingPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_FixedFFT->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_MovingFFT->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_IFFT->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_CropFilter->SetReleaseDataBeforeUpdateFlag( a_flag );
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::SetOptimizer( RealOptimizerType * optimizer )
{
  itkDebugMacro("setting RealOptimizer to " << optimizer );
  if (this->m_RealOptimizer != optimizer)
    {
    this->m_RealOptimizer = optimizer;
    this->m_ComplexOptimizer = 0;
    this->Modified();
    }
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::SetOptimizer( ComplexOptimizerType * optimizer )
{
  itkDebugMacro("setting ComplexOptimizer to " << optimizer );
  if (this->m_ComplexOptimizer != optimizer)
    {
    this->m_ComplexOptimizer = optimizer;
    this->m_RealOptimizer = 0;
    this->Modified();
    }
}


} // end namespace itk

#endif
