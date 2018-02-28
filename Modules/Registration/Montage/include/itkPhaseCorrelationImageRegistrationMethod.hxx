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
  this->SetNumberOfRequiredOutputs( 1 );  // for the Transform

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

  m_FixedPadder->SetConstant( 0 );
  m_MovingPadder->SetConstant( 0 );

  m_FixedFFT->SetInput( m_FixedPadder->GetOutput() );
  m_MovingFFT->SetInput( m_MovingPadder->GetOutput() );


  m_TransformParameters = ParametersType(ImageDimension);
  m_TransformParameters.Fill( 0.0f );

  TransformOutputPointer transformDecorator =
                 static_cast< TransformOutputType * >(
                                  this->MakeOutput(0).GetPointer() );

  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );

  itkDebugMacro( "output is " << this->GetOutput()->Get() );
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
    m_RealOptimizer->SetInput( m_IFFT->GetOutput() );
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
  typename FixedImageType::SpacingType fixedSpacing =
      m_FixedImage->GetSpacing();
  typename MovingImageType::SizeType movingSize =
      m_MovingImage->GetLargestPossibleRegion().GetSize();
  typename MovingImageType::SpacingType movingSpacing =
      m_MovingImage->GetSpacing();

  typename FixedImageType::SizeType fixedPad;
  typename MovingImageType::SizeType movingPad;

  for (int i=0; i<ImageDimension; i++)
    {
    if ( fixedSize[i]*fixedSpacing[i] > movingSize[i]*movingSpacing[i] )
      {
      movingPad[i] = Math::Floor< SizeValueType >( fixedSize[i]*fixedSpacing[i]/movingSpacing[i] -
                                  movingSize[i] );
      fixedPad[i] = 0;
      }
    else
      {
      fixedPad[i] = Math::Floor< SizeValueType >( movingSize[i]*movingSpacing[i]/fixedSpacing[i] -
                                fixedSize[i] );
      movingPad[i] = 0;
      }
    }

  m_FixedPadder->SetPadUpperBound( fixedPad );
  m_MovingPadder->SetPadUpperBound( movingPad );
}


template < typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::StartOptimization()
{
  itkDebugMacro( "starting optimization" );
  typedef typename RealImageType::PointType   OffsetType;
  OffsetType offset;
  try
    {
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
  for (int i = 0; i<ImageDimension; i++)
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
::GenerateData()
{
  if (!m_Updating)
    {
    this->Update();
    }
  else
    {
    ParametersType empty(ImageDimension);
    empty.Fill( 0.0 );
    try
      {
      // initialize the interconnects between components
      this->Initialize();
      }
    catch( ExceptionObject& err )
      {
      itkDebugMacro( "exception caught during intialization - passing" );

      m_TransformParameters = empty;

      // pass exception to caller
      throw err;
      }
    this->DeterminePadding();
    //execute the computation
    this->StartOptimization();
    }

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
DataObject::Pointer
PhaseCorrelationImageRegistrationMethod<TFixedImage,TMovingImage>
::MakeOutput(DataObjectPointerArraySizeType output)
{
  switch (output)
    {
    case 0:
      return static_cast<DataObject*>(TransformOutputType::New().GetPointer());
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
