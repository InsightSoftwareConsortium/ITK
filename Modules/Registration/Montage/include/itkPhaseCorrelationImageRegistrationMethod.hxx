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
#include "itkNumericTraits.h"

#include <algorithm>
#include <cmath>

#ifndef NDEBUG
#include "itkImageFileWriter.h"

namespace
{
template< typename TImage >
void WriteDebug(const TImage* out, const char *filename)
{
  using WriterType = itk::ImageFileWriter<TImage>;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput(out);
  w->SetFileName(filename);
  try
    {
    w->Update();
    }
  catch (itk::ExceptionObject & error)
    {
    std::cerr << error << std::endl;
    }
}
}
#else
namespace
{
template< typename TImage >
void WriteDebug(TImage*, const char *) {}
}
#endif

namespace itk
{
template< typename TFixedImage, typename TMovingImage >
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >::PhaseCorrelationImageRegistrationMethod()
{
  this->SetNumberOfRequiredInputs( 2 );
  this->SetNumberOfRequiredOutputs( 2 ); // for 0-the Transform, 1-the phase correlation image

  m_FixedImage = nullptr;
  m_MovingImage = nullptr;
  m_FixedImageFFT = nullptr;
  m_MovingImageFFT = nullptr;
  m_Operator = nullptr;
  m_RealOptimizer = nullptr;
  m_ComplexOptimizer = nullptr;

  m_FixedConstantPadder = FixedConstantPadderType::New();
  m_MovingConstantPadder = MovingConstantPadderType::New();
  m_FixedMirrorPadder = FixedMirrorPadderType::New();
  m_MovingMirrorPadder = MovingMirrorPadderType::New();
  m_FixedMirrorWEDPadder = FixedMirrorPadderType::New();
  m_MovingMirrorWEDPadder = MovingMirrorPadderType::New();
  m_BandPassFilter = BandBassFilterType::New();

  m_ButterworthOrder = 3;
  m_LowFrequency2 = 0.0025; // 0.05^2
  m_HighFrequency2 = 0.25; // 0.5^2
  m_BandPassFilter->SetFunctor( m_BandPassFunctor );

  m_FixedFFT = FFTFilterType::New();
  m_MovingFFT = FFTFilterType::New();
  m_IFFT = IFFTFilterType::New();

  m_FixedConstantPadder->SetConstant( NumericTraits< FixedImagePixelType >::Zero );
  m_MovingConstantPadder->SetConstant( NumericTraits< MovingImagePixelType >::Zero );
  m_FixedMirrorWEDPadder->SetDecayBase( 0.75 );
  m_MovingMirrorWEDPadder->SetDecayBase( 0.75 );

  m_BandPassFunctor = [this]( typename BandBassFilterType::FrequencyIteratorType& freqIt )
    {
    double f2 = freqIt.GetFrequencyModuloSquare(); // square of scalar frequency
    freqIt.Value() *= 1.0 - 1.0 / ( 1.0 + std::pow( f2 / this->m_LowFrequency2, this->m_ButterworthOrder ) );
    freqIt.Value() /= 1.0 + std::pow( f2 / this->m_HighFrequency2, this->m_ButterworthOrder );
    };
  m_HighPassFunctor = [this]( typename BandBassFilterType::FrequencyIteratorType& freqIt )
    {
    double f2 = freqIt.GetFrequencyModuloSquare(); // square of scalar frequency
    freqIt.Value() *= 1.0 - 1.0 / ( 1.0 + std::pow( f2 / this->m_LowFrequency2, this->m_ButterworthOrder ) );
    };
  m_LowPassFunctor = [this]( typename BandBassFilterType::FrequencyIteratorType& freqIt )
    {
    double f2 = freqIt.GetFrequencyModuloSquare(); // square of scalar frequency
    freqIt.Value() /= 1.0 + std::pow( f2 / this->m_HighFrequency2, this->m_ButterworthOrder );
    };

  m_PadToSize.Fill( 0 );
  m_ObligatoryPadding.Fill( 8 );
  m_PaddingMethod = PaddingMethod::Zero; // make sure the next call does modifications
  SetPaddingMethod( PaddingMethod::MirrorWithExponentialDecay ); // this initializes a few things

  m_TransformParameters = ParametersType( ImageDimension );
  m_TransformParameters.Fill( 0.0f );

  TransformOutputPointer transformDecorator = static_cast< TransformOutputType* >( this->MakeOutput( 0 ).GetPointer() );
  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );
  //std::cout << "output is " << this->GetOutput()->Get() << std::endl;

  typename RealImageType::Pointer phaseCorrelation =
    static_cast< RealImageType* >( this->MakeOutput( 1 ).GetPointer() );
  this->ProcessObject::SetNthOutput( 1, phaseCorrelation.GetPointer() );
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod<TFixedImage, TMovingImage>
::SetPaddingMethod( const PaddingMethod paddingMethod )
{
  if ( this->m_PaddingMethod != paddingMethod )
    {
    this->m_PaddingMethod = paddingMethod;

    switch ( paddingMethod )
      {
      case PaddingMethod::Zero:
        m_FixedPadder = m_FixedConstantPadder;
        m_MovingPadder = m_MovingConstantPadder;
        break;
      case PaddingMethod::Mirror:
        m_FixedPadder = m_FixedMirrorPadder;
        m_MovingPadder = m_MovingMirrorPadder;
        break;
      case PaddingMethod::MirrorWithExponentialDecay:
        m_FixedPadder = m_FixedMirrorWEDPadder;
        m_MovingPadder = m_MovingMirrorWEDPadder;
        break;
      default:
        itkExceptionMacro( "Unknown padding method" );
        break;
      }

    m_FixedFFT->SetInput( m_FixedPadder->GetOutput() );
    m_MovingFFT->SetInput( m_MovingPadder->GetOutput() );
    this->Modified();
    }
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::Initialize()
{
  itkDebugMacro( "initializing registration" );
  if ( !m_FixedImage )
    {
    itkExceptionMacro( << "FixedImage is not present" );
    }
  if ( !m_MovingImage )
    {
    itkExceptionMacro( << "MovingImage is not present" );
    }
  if ( !m_Operator )
    {
    itkExceptionMacro( << "Operator is not present" );
    }
  if ( !m_RealOptimizer && !m_ComplexOptimizer )
    {
    itkExceptionMacro( << "Optimizer is not present" );
    }

  // Connect new transform to the Decorator if necessary.
  TransformOutputPointer transformOutput( static_cast< TransformOutputType* >( this->ProcessObject::GetOutput( 0 ) ) );
  TransformPointer transform( const_cast< TransformType* >( transformOutput->Get() ) );

  if ( transform.IsNull() )
    {
    transform = TransformType::New();
    transformOutput->Set( transform.GetPointer() );
    }

  // set up the pipeline
  m_FixedPadder->SetInput( m_FixedImage );
  m_MovingPadder->SetInput( m_MovingImage );
  if ( m_FixedImageFFT.IsNull() )
    {
    m_Operator->SetFixedImage( m_FixedFFT->GetOutput() );
    }
  else
    {
    m_Operator->SetFixedImage( m_FixedImageFFT );
    }
  if ( m_MovingImageFFT.IsNull() )
    {
    m_Operator->SetMovingImage( m_MovingFFT->GetOutput() );
    }
  else
    {
    m_Operator->SetMovingImage( m_MovingImageFFT );
    }
  m_BandPassFilter->SetInput( m_Operator->GetOutput() );

  using ImageFilter = ImageToImageFilter< ComplexImageType, ComplexImageType >;
  ImageFilter* finalOperatorFilter = m_BandPassFilter;

  if ( m_LowFrequency2 > 0.0 && m_HighFrequency2 > 0.0 )
    {
    m_BandPassFilter->SetFunctor( m_BandPassFunctor );
    }
  else if ( m_HighFrequency2 > 0.0 )
    {
    m_BandPassFilter->SetFunctor( m_HighPassFunctor );
    }
  else if ( m_LowFrequency2 > 0.0 )
    {
    m_BandPassFilter->SetFunctor( m_LowPassFunctor );
    }
  else // neither high nor low filtering is set
    {
    m_BandPassFilter->SetFunctor( m_IdentityFunctor );
    finalOperatorFilter = m_Operator; //we skip the band-pass entirely
    }

  if ( m_RealOptimizer )
    {
    m_IFFT->SetInput( finalOperatorFilter->GetOutput() );
    m_RealOptimizer->SetInput( m_IFFT->GetOutput() );
    m_RealOptimizer->SetFixedImage( m_FixedImage );
    m_RealOptimizer->SetMovingImage( m_MovingImage );
    }
  else
    {
    m_ComplexOptimizer->SetInput( finalOperatorFilter->GetOutput() );
    m_ComplexOptimizer->SetFixedImage( m_FixedImage );
    m_ComplexOptimizer->SetMovingImage( m_MovingImage );
    }
}


template< typename TFixedImage, typename TMovingImage >
typename PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >::SizeType
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::RoundUpToFFTSize( typename PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >::SizeType size )
{
  // FFTs are faster when image size can be factorized using smaller prime numbers
  const auto sizeGreatestPrimeFactor = std::min< SizeValueType >( 5, m_FixedFFT->GetSizeGreatestPrimeFactor() );

  for ( unsigned int d = 0; d < ImageDimension; ++d )
    {
    if ( sizeGreatestPrimeFactor > 1 )
      {
      while ( Math::GreatestPrimeFactor( size[d] ) > sizeGreatestPrimeFactor )
        {
        ++size[d];
        }
      }
    else if ( sizeGreatestPrimeFactor == 1 )
      {
      // make sure the total size is even
      size[d] += size[d] % 2;
      }
    }

  return size;
}

template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::DeterminePadding()
{
  SizeType fixedSize = m_FixedImage->GetLargestPossibleRegion().GetSize();
  SizeType movingSize = m_MovingImage->GetLargestPossibleRegion().GetSize();
  SizeType fftSize;
  SizeType size0;
  size0.Fill( 0 );

  if ( m_PadToSize == size0 )
    {
    // set up padding to resize the images to the same size
    SizeType maxSize;

    for ( unsigned int d = 0; d < ImageDimension; ++d )
      {
      if ( fixedSize[d] >= movingSize[d] )
        {
        maxSize[d] = fixedSize[d];
        }
      else
        {
        maxSize[d] = movingSize[d];
        }
      // we need to pad on both ends along this dimension
      maxSize[d] += 2 * m_ObligatoryPadding[d];
      }

    fftSize = RoundUpToFFTSize( maxSize );
    }
  else
    {
    fftSize = m_PadToSize;
    }

  SizeType fftHalf = fftSize;
  fftHalf[0] = fftSize[0] / 2 + 1;
  if ( m_FixedImageFFT.IsNotNull() )
    {
    SizeType fftCached = m_FixedImageFFT->GetLargestPossibleRegion().GetSize();
    itkAssertOrThrowMacro( fftCached == fftHalf, "FixedImage's cached FFT ("
        << fftCached << ") must have the common padded size: " << fftSize
        << " halved in first dimension: " << fftHalf );
    }
  if ( m_MovingImageFFT.IsNotNull() )
    {
    SizeType fftCached = m_MovingImageFFT->GetLargestPossibleRegion().GetSize();
    itkAssertOrThrowMacro( fftCached == fftHalf, "MovingImage's cached FFT ("
        << fftCached << ") must have the common padded size: " << fftSize
        << " halved in first dimension: " << fftHalf );
    }

  SizeType fixedPad, movingPad;
  for ( unsigned int d = 0; d < ImageDimension; ++d )
    {
    if ( fixedSize[d] + 2 * m_ObligatoryPadding[d] > fftSize[d] )
      {
      itkExceptionMacro( "PadToSize(" << fftSize[d] << ") for dimension " << d
          << " must be larger than fixed image size (" << fixedSize[d] << ")"
          << " and twice the obligatory padding (" << m_ObligatoryPadding[d] << ")" );
      }
    fixedPad[d] = ( fftSize[d] - fixedSize[d] ) - m_ObligatoryPadding[d];
    if ( movingSize[d] + 2 * m_ObligatoryPadding[d] > fftSize[d] )
      {
      itkExceptionMacro( "PadToSize(" << fftSize[d] << ") for dimension " << d
          << " must be larger than moving image size (" << movingSize[d] << ")"
          << " and twice the obligatory padding (" << m_ObligatoryPadding[d] << ")" );
      }
    movingPad[d] = ( fftSize[d] - movingSize[d] ) - m_ObligatoryPadding[d];
    }

  m_FixedPadder->SetPadLowerBound( m_ObligatoryPadding );
  m_MovingPadder->SetPadLowerBound( m_ObligatoryPadding );
  m_FixedPadder->SetPadUpperBound( fixedPad );
  m_MovingPadder->SetPadUpperBound( movingPad );
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::StartOptimization()
{
  ParametersType empty( ImageDimension );
  empty.Fill( 0.0 );
  m_TransformParameters = empty;
  itkDebugMacro( "starting optimization" );
  using OffsetType = typename RealOptimizerType::OffsetType;
  OffsetType offset;
  try
    {
    if ( this->GetDebug() )
      {
      WriteDebug( m_FixedImage.GetPointer(), "m_FixedImage.nrrd" );
      WriteDebug( m_MovingImage.GetPointer(), "m_MovingImage.nrrd" );
      WriteDebug( m_FixedPadder->GetOutput(), "m_FixedPadder.nrrd" );
      WriteDebug( m_MovingPadder->GetOutput(), "m_MovingPadder.nrrd" );
      WriteDebug( m_FixedFFT->GetOutput(), "m_FixedFFT.nrrd" );
      WriteDebug( m_MovingFFT->GetOutput(), "m_MovingFFT.nrrd" );
      }

    m_FixedPadder->UpdateOutputInformation(); // to make sure xSize is valid
    unsigned xSize = m_FixedPadder->GetOutput()->GetLargestPossibleRegion().GetSize( 0 );
    m_IFFT->SetActualXDimensionIsOdd( xSize % 2 != 0 );
    RealImageType* phaseCorrelation = static_cast< RealImageType* >( this->ProcessObject::GetOutput( 1 ) );
    phaseCorrelation->Allocate();
    m_IFFT->GraftOutput( phaseCorrelation );
    m_IFFT->Update();

    if ( m_RealOptimizer )
      {
      m_RealOptimizer->SetOffsetCount( 3 ); // update can reduce this, so we have to set it each time
      m_RealOptimizer->Update();
      offset = m_RealOptimizer->GetOffsets()[0];
      }
    else
      {
      m_ComplexOptimizer->Update();
      offset = m_ComplexOptimizer->GetOffsets()[0];
      }
    phaseCorrelation->Graft( m_IFFT->GetOutput() );

    if ( m_FixedImageFFT.IsNull() )
      {
      m_FixedImageFFT = m_FixedFFT->GetOutput();
      m_FixedImageFFT->DisconnectPipeline();
      }
    if ( m_MovingImageFFT.IsNull() )
      {
      m_MovingImageFFT = m_MovingFFT->GetOutput();
      m_MovingImageFFT->DisconnectPipeline();
      }

    if ( this->GetDebug() )
      {
      WriteDebug( m_IFFT->GetOutput(), "m_IFFT.nrrd" );
      WriteDebug( m_BandPassFilter->GetOutput(), "m_BandPassFilter.nrrd" );
      WriteDebug( m_Operator->GetOutput(), "m_Operator.nrrd" );
      }
    }
  catch ( ExceptionObject& err )
    {
    // Pass exception to caller
    itkDebugMacro( "exception caught during optimization - passing" );
    throw err;
    }
  itkDebugMacro( "optimization finished" );

  m_TransformParameters = ParametersType( ImageDimension );
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    m_TransformParameters[i] = offset[i];
    }

  // set the output transform
  TransformOutputType* transformOutput = static_cast< TransformOutputType* >( this->ProcessObject::GetOutput( 0 ) );
  TransformPointer transform( const_cast< TransformType* >( transformOutput->Get() ) );
  transform->SetParameters( m_TransformParameters );

  itkDebugMacro( "output set to " << transform );
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Operator: " << m_Operator.GetPointer() << std::endl;
  os << indent << "Real Optimizer: " << m_RealOptimizer.GetPointer() << std::endl;
  os << indent << "Complex Optimizer: " << m_ComplexOptimizer.GetPointer() << std::endl;
  os << indent << "Fixed Padder: " << m_FixedPadder.GetPointer() << std::endl;
  os << indent << "Moving Padder: " << m_MovingPadder.GetPointer() << std::endl;

  os << indent << "Pad To Size: " << m_PadToSize << std::endl;
  os << indent << "Obligatory Padding: " << m_ObligatoryPadding << std::endl;
  os << indent << "Padding Method: " << int( m_PaddingMethod ) << std::endl;

  os << indent << "Fixed Image: " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "Fixed Image FFT: " << m_FixedImageFFT.GetPointer() << std::endl;
  os << indent << "Moving Image FFT: " << m_MovingImageFFT.GetPointer() << std::endl;
  os << indent << "Transform Parameters: " << m_TransformParameters << std::endl;

  typename TransformType::ConstPointer t( this->GetOutput()->Get() );
  os << indent << "Output transform: " << t.GetPointer() << std::endl;
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  this->Initialize();
  this->DeterminePadding();

  if ( m_FixedImage->GetSpacing() != m_MovingImage->GetSpacing() )
    {
    itkExceptionMacro( "Fixed image and moving image must have the same spacing!\nFixed spacing: "
        << m_FixedImage->GetSpacing() << "\nMoving spacing: " << m_MovingImage->GetSpacing() );
    }
  if (m_FixedImage->GetDirection() != m_MovingImage->GetDirection())
    {
    itkExceptionMacro( "Fixed image and moving image must have the same direction!\nFixed direction:\n"
        << m_FixedImage->GetDirection() << "\nMoving direction:\n" << m_MovingImage->GetDirection() );
    }

  m_IFFT->UpdateOutputInformation();

  RealImageType* phaseCorrelation = static_cast< RealImageType* >( this->ProcessObject::GetOutput( 1 ) );
  phaseCorrelation->CopyInformation( m_IFFT->GetOutput() );
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::GenerateData()
{
  this->Initialize();
  this->StartOptimization();
}


template< typename TFixedImage, typename TMovingImage >
const typename PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >::TransformOutputType*
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::GetOutput() const
{
  return static_cast< const TransformOutputType* >( this->ProcessObject::GetOutput( 0 ) );
}


template< typename TFixedImage, typename TMovingImage >
const typename PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >::RealImageType*
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::GetPhaseCorrelationImage() const
{
  return static_cast< const RealImageType* >( this->ProcessObject::GetOutput( 1 ) );
}


template< typename TFixedImage, typename TMovingImage >
DataObject::Pointer
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::MakeOutput( DataObjectPointerArraySizeType output )
{
  switch ( output )
    {
    case 0:
      return static_cast< DataObject* >( TransformOutputType::New().GetPointer() );
      break;
    case 1:
      return static_cast< DataObject* >( RealImageType::New().GetPointer() );
      break;
    default:
      itkExceptionMacro( "MakeOutput request for an output number larger than the expected number of outputs" );
    }
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetFixedImage( const FixedImageType* fixedImage )
{
  itkDebugMacro( "setting Fixed Image to " << fixedImage );
  if ( this->m_FixedImage.GetPointer() != fixedImage )
    {
    this->m_FixedImage = fixedImage;
    this->m_FixedImageFFT = nullptr; // clear cached FFT
    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput( 0, const_cast< FixedImageType* >( fixedImage ) );
    this->Modified();
    }
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetMovingImage( const MovingImageType* movingImage )
{
  itkDebugMacro( "setting Moving Image to " << movingImage );
  if ( this->m_MovingImage.GetPointer() != movingImage )
    {
    this->m_MovingImage = movingImage;
    this->m_MovingImageFFT = nullptr; // clear cached FFT
    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput( 1, const_cast< MovingImageType* >( movingImage ) );
    this->Modified();
    }
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetFixedImageFFT( const ComplexImageType* fixedImageFFT )
{
  itkDebugMacro( "setting fixedImageFFT Image to " << fixedImageFFT );
  if ( this->m_FixedImageFFT.GetPointer() != fixedImageFFT )
    {
    this->m_FixedImageFFT = const_cast< ComplexImageType* >( fixedImageFFT );
    this->Modified();
    }
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetMovingImageFFT( const ComplexImageType* movingImageFFT )
{
  itkDebugMacro( "setting movingImageFFT Image to " << movingImageFFT );
  if ( this->m_MovingImageFFT.GetPointer() != movingImageFFT )
    {
    this->m_MovingImageFFT = const_cast< ComplexImageType* >( movingImageFFT );
    this->Modified();
    }
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetReleaseDataFlag( bool a_flag )
{
  Superclass::SetReleaseDataFlag( a_flag );
  m_FixedConstantPadder->SetReleaseDataFlag( a_flag );
  m_MovingConstantPadder->SetReleaseDataFlag( a_flag );
  m_FixedMirrorPadder->SetReleaseDataFlag( a_flag );
  m_MovingMirrorPadder->SetReleaseDataFlag( a_flag );
  m_FixedMirrorWEDPadder->SetReleaseDataFlag( a_flag );
  m_MovingMirrorWEDPadder->SetReleaseDataFlag( a_flag );
  m_FixedFFT->SetReleaseDataFlag( a_flag );
  m_MovingFFT->SetReleaseDataFlag( a_flag );
  m_IFFT->SetReleaseDataFlag( a_flag );
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetReleaseDataBeforeUpdateFlag( bool a_flag )
{
  Superclass::SetReleaseDataBeforeUpdateFlag( a_flag );
  m_FixedConstantPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_MovingConstantPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_FixedMirrorPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_MovingMirrorPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_FixedMirrorWEDPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_MovingMirrorWEDPadder->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_FixedFFT->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_MovingFFT->SetReleaseDataBeforeUpdateFlag( a_flag );
  m_IFFT->SetReleaseDataBeforeUpdateFlag( a_flag );
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetOptimizer( RealOptimizerType* optimizer )
{
  itkDebugMacro( "setting RealOptimizer to " << optimizer );
  if ( this->m_RealOptimizer != optimizer )
    {
    this->m_RealOptimizer = optimizer;
    this->m_ComplexOptimizer = nullptr;
    this->Modified();
    }
}


template< typename TFixedImage, typename TMovingImage >
void
PhaseCorrelationImageRegistrationMethod< TFixedImage, TMovingImage >
::SetOptimizer( ComplexOptimizerType* optimizer )
{
  itkDebugMacro( "setting ComplexOptimizer to " << optimizer );
  if ( this->m_ComplexOptimizer != optimizer )
    {
    this->m_ComplexOptimizer = optimizer;
    this->m_RealOptimizer = nullptr;
    this->Modified();
    }
}

} // end namespace itk

#endif
