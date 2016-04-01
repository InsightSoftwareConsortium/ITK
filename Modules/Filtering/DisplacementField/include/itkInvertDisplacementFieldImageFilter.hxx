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
#ifndef itkInvertDisplacementFieldImageFilter_hxx
#define itkInvertDisplacementFieldImageFilter_hxx

#include "itkInvertDisplacementFieldImageFilter.h"

#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkImageDuplicator.h"
#include "itkImageRegionIterator.h"
#include "itkMutexLockHolder.h"

namespace itk
{

/*
 * InvertDisplacementFieldImageFilter class definitions
 */
template<typename TInputImage, typename TOutputImage>
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::InvertDisplacementFieldImageFilter() :
  m_Interpolator(DefaultInterpolatorType::New()),
  m_MaximumNumberOfIterations(20),
  m_MaxErrorToleranceThreshold(0.1),
  m_MeanErrorToleranceThreshold(0.001),

  m_ComposedField(DisplacementFieldType::New()),
  m_ScaledNormImage(RealImageType::New()),
  m_MaxErrorNorm(0.0),
  m_MeanErrorNorm(0.0),
  m_Epsilon(0.0),
  m_DoThreadedEstimateInverse(false),
  m_EnforceBoundaryCondition(true)
{
  this->SetNumberOfRequiredInputs( 1 );
}

template<typename TInputImage, typename TOutputImage>
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::~InvertDisplacementFieldImageFilter()
{
}

template<typename TInputImage, typename TOutputImage>
void
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::SetInterpolator( InterpolatorType *interpolator )
{
  itkDebugMacro( "setting Interpolator to " << interpolator );
  if ( this->m_Interpolator != interpolator )
    {
    this->m_Interpolator = interpolator;
    this->Modified();
    if( !this->GetDisplacementField() )
      {
      this->m_Interpolator->SetInputImage( this->GetInput( 0 ) );
      }
    }
}

template<typename TInputImage, typename TOutputImage>
void
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  this->AllocateOutputs();

  VectorType zeroVector( 0.0 );

  typename DisplacementFieldType::ConstPointer displacementField = this->GetInput();

  typename InverseDisplacementFieldType::Pointer inverseDisplacementField;

  if( this->GetInverseFieldInitialEstimate() )
    {
    typedef ImageDuplicator<InverseDisplacementFieldType> DuplicatorType;
    typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
    duplicator->SetInputImage( this->GetInverseFieldInitialEstimate() );
    duplicator->Update();

    inverseDisplacementField = duplicator->GetModifiableOutput();

    this->SetNthOutput( 0, inverseDisplacementField );
    }
  else
    {
    inverseDisplacementField = this->GetOutput();
    inverseDisplacementField->FillBuffer( zeroVector );
    }

  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    this->m_DisplacementFieldSpacing[d] = displacementField->GetSpacing()[d];
    }

  this->m_ScaledNormImage->CopyInformation( displacementField );
  this->m_ScaledNormImage->SetRegions( displacementField->GetRequestedRegion() );
  this->m_ScaledNormImage->Allocate(true); // initialize
                                                                  // buffer
                                                                  // to zero

  SizeValueType numberOfPixelsInRegion = ( displacementField->GetRequestedRegion() ).GetNumberOfPixels();
  this->m_MaxErrorNorm = NumericTraits<RealType>::max();
  this->m_MeanErrorNorm = NumericTraits<RealType>::max();
  unsigned int iteration = 0;

  while( iteration++ < this->m_MaximumNumberOfIterations &&
    this->m_MaxErrorNorm > this->m_MaxErrorToleranceThreshold &&
    this->m_MeanErrorNorm > this->m_MeanErrorToleranceThreshold )
    {
    itkDebugMacro( "Iteration " << iteration << ": mean error norm = " << this->m_MeanErrorNorm
      << ", max error norm = " << this->m_MaxErrorNorm );

    typedef ComposeDisplacementFieldsImageFilter<DisplacementFieldType> ComposerType;
    typename ComposerType::Pointer composer = ComposerType::New();
    composer->SetDisplacementField( displacementField );
    composer->SetWarpingField( inverseDisplacementField );

    this->m_ComposedField = composer->GetOutput();
    this->m_ComposedField->Update();
    this->m_ComposedField->DisconnectPipeline();

    /**
     * Multithread processing to multiply each element of the composed field by 1 / spacing
     */
    this->m_MeanErrorNorm = NumericTraits<RealType>::ZeroValue();
    this->m_MaxErrorNorm = NumericTraits<RealType>::ZeroValue();

    this->m_DoThreadedEstimateInverse = false;
    typename ImageSource<TOutputImage>::ThreadStruct str0;
    str0.Filter = this;
    this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
    this->GetMultiThreader()->SetSingleMethod( this->ThreaderCallback, &str0 );
    this->GetMultiThreader()->SingleMethodExecute();

    this->m_MeanErrorNorm /= static_cast<RealType>( numberOfPixelsInRegion );

    this->m_Epsilon = 0.5;
    if( iteration == 1 )
      {
      this->m_Epsilon = 0.75;
      }

    /**
     * Multithread processing to estimate inverse field
     */
    this->m_DoThreadedEstimateInverse = true;
    typename ImageSource<TOutputImage>::ThreadStruct str1;
    str1.Filter = this;
    this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
    this->GetMultiThreader()->SetSingleMethod( this->ThreaderCallback, &str1 );
    this->GetMultiThreader()->SingleMethodExecute();
    }
}

template<typename TInputImage, typename TOutputImage>
void
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData( const RegionType & region, ThreadIdType itkNotUsed( threadId ) )
{
  const typename DisplacementFieldType::RegionType fullRegion = this->m_ComposedField->GetRequestedRegion();
  const typename DisplacementFieldType::SizeType size = fullRegion.GetSize();
  const typename DisplacementFieldType::IndexType startIndex = fullRegion.GetIndex();
  const typename DisplacementFieldType::PixelType zeroVector( 0.0 );

  ImageRegionIterator<DisplacementFieldType> ItE( this->m_ComposedField, region );
  ImageRegionIterator<RealImageType> ItS( this->m_ScaledNormImage, region );

  if( this->m_DoThreadedEstimateInverse )
    {
    ImageRegionIterator<DisplacementFieldType> ItI( this->GetOutput(), region );

    for( ItI.GoToBegin(), ItE.GoToBegin(), ItS.GoToBegin(); !ItI.IsAtEnd(); ++ItI, ++ItE, ++ItS )
      {
      VectorType update = ItE.Get();
      RealType scaledNorm = ItS.Get();

      if( scaledNorm > this->m_Epsilon * this->m_MaxErrorNorm  )
        {
        update *= ( this->m_Epsilon * this->m_MaxErrorNorm / scaledNorm );
        }
      update = ItI.Get() + update * this->m_Epsilon;
      ItI.Set( update );
      typename DisplacementFieldType::IndexType index = ItI.GetIndex();
      if( this->m_EnforceBoundaryCondition )
        {
        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          if( index[d] == startIndex[d] || index[d] == static_cast<IndexValueType>( size[d] ) - startIndex[d] - 1 )
            {
            ItI.Set( zeroVector );
            break;
            }
          }
        } // enforce boundary condition
      }
    }
  else
    {
    VectorType inverseSpacing;
    RealType localMean = NumericTraits<RealType>::ZeroValue();
    RealType localMax  = NumericTraits<RealType>::ZeroValue();
    for( unsigned int d = 0; d < ImageDimension; ++d )
      {
      inverseSpacing[d]=1.0/this->m_DisplacementFieldSpacing[d];
      }
    for( ItE.GoToBegin(), ItS.GoToBegin(); !ItE.IsAtEnd(); ++ItE, ++ItS )
      {
      const VectorType & displacement = ItE.Get();
      RealType scaledNorm = 0.0;
      for( unsigned int d = 0; d < ImageDimension; ++d )
        {
        scaledNorm += itk::Math::sqr( displacement[d] * inverseSpacing[d] );
        }
      scaledNorm = std::sqrt( scaledNorm );

      localMean += scaledNorm;
      if( localMax < scaledNorm )
        {
        localMax = scaledNorm;
        }

      ItS.Set( scaledNorm );
      ItE.Set( -displacement );
      }
      {
      MutexLockHolder<SimpleFastMutexLock> holder(m_Mutex);
      this->m_MeanErrorNorm += localMean;
      if( this->m_MaxErrorNorm < localMax )
        {
        this->m_MaxErrorNorm = localMax;
        }
      }
    }
}

template<typename TInputImage, typename TOutputImage>
void
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  itkPrintSelfObjectMacro( Interpolator );

  os << "Maximum number of iterations: " << this->m_MaximumNumberOfIterations << std::endl;
  os << "Max error tolerance threshold: " << this->m_MaxErrorToleranceThreshold << std::endl;
  os << "Mean error tolerance threshold: " << this->m_MeanErrorToleranceThreshold << std::endl;
}

}  //end namespace itk

#endif
