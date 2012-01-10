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
#ifndef __itkInvertDisplacementFieldImageFilter_hxx
#define __itkInvertDisplacementFieldImageFilter_hxx

#include "itkInvertDisplacementFieldImageFilter.h"

#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkStatisticsImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkVectorMagnitudeImageFilter.h"

namespace itk
{

/*
 * InvertDisplacementFieldImageFilter class definitions
 */
template<class TInputImage, class TOutputImage>
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::InvertDisplacementFieldImageFilter() :
 m_MaximumNumberOfIterations( 20 ),
 m_MaxErrorToleranceThreshold( 0.1 ),
 m_MeanErrorToleranceThreshold( 0.001 )
{
  this->SetNumberOfRequiredInputs( 1 );

  typedef VectorLinearInterpolateImageFunction <InputFieldType, RealType> DefaultInterpolatorType;
  typename DefaultInterpolatorType::Pointer interpolator = DefaultInterpolatorType::New();
  this->m_Interpolator = interpolator;

  this->m_ComposedField = DisplacementFieldType::New();
}

template<class TInputImage, class TOutputImage>
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::~InvertDisplacementFieldImageFilter()
{
}

template<class TInputImage, class TOutputImage>
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

template<class TInputImage, class TOutputImage>
void
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  this->AllocateOutputs();

  VectorType zeroVector( 0.0 );

  typename DisplacementFieldType::ConstPointer displacementField = this->GetInput();
  typename InverseDisplacementFieldType::Pointer inverseDisplacementField = this->GetOutput();
  inverseDisplacementField->FillBuffer( zeroVector );

  this->m_NormalizationFactor = 1.0;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    this->m_DisplacementFieldSpacing[d] = displacementField->GetSpacing()[d];
    this->m_NormalizationFactor /= this->m_DisplacementFieldSpacing[d];
    }

  this->m_MaxErrorNorm = NumericTraits<RealType>::max();
  this->m_MeanErrorNorm = NumericTraits<RealType>::max();
  unsigned int iteration = 0;

  while( iteration++ < this->m_MaximumNumberOfIterations &&
    this->m_MaxErrorNorm > this->m_MaxErrorToleranceThreshold &&
    this->m_MeanErrorNorm > m_MeanErrorToleranceThreshold )
    {
    itkDebugMacro( "Iteration " << iteration << ": mean error norm = " << this->m_MeanErrorNorm
      << ", max error norm = " << this->m_MaxErrorNorm );

    this->m_MeanErrorNorm = 0.0;
    this->m_MaxErrorNorm = 0.0;

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
    this->m_DoThreadedEstimateInverse = false;
    typename ImageSource<TOutputImage>::ThreadStruct str0;
    str0.Filter = this;
    this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
    this->GetMultiThreader()->SetSingleMethod( this->ThreaderCallback, &str0 );
    this->GetMultiThreader()->SingleMethodExecute();

    typedef VectorMagnitudeImageFilter<DisplacementFieldType, RealImageType> NormFilterType;
    typename NormFilterType::Pointer normFilter = NormFilterType::New();
    normFilter->SetInput( this->m_ComposedField );

    typedef StatisticsImageFilter<RealImageType> StatisticsType;
    typename StatisticsType::Pointer statistics = StatisticsType::New();
    statistics->SetInput( normFilter->GetOutput() );
    statistics->Update();

    this->m_MeanErrorNorm = statistics->GetMean();
    this->m_MaxErrorNorm = statistics->GetMaximum();

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

template<class TInputImage, class TOutputImage>
void
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData( const RegionType & region, ThreadIdType itkNotUsed( threadId ) )
{
  ImageRegionIterator<DisplacementFieldType> ItE( this->m_ComposedField, region );

  if( this->m_DoThreadedEstimateInverse )
    {
    ImageRegionIterator<DisplacementFieldType> ItI( this->GetOutput(), region );

    for( ItI.GoToBegin(), ItE.GoToBegin(); !ItI.IsAtEnd(); ++ItI, ++ItE )
      {
      VectorType update = -ItE.Get();
      RealType updateNorm = update.GetNorm();

      if( updateNorm > this->m_Epsilon * this->m_MaxErrorNorm / this->m_NormalizationFactor )
        {
        update *= ( this->m_Epsilon * this->m_MaxErrorNorm / ( updateNorm * this->m_NormalizationFactor ) );
        }
      ItI.Set( ItI.Get() + update * this->m_Epsilon );
      }
    }
  else
    {
    for( ItE.GoToBegin(); !ItE.IsAtEnd(); ++ItE )
      {
      VectorType displacement = ItE.Get();
      for( unsigned int d = 0; d < ImageDimension; d++ )
        {
        displacement[d] /= this->m_DisplacementFieldSpacing[d];
        }
      ItE.Set( displacement );
      }
    }
}

template<class TInputImage, class TOutputImage>
void
InvertDisplacementFieldImageFilter<TInputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << "Interpolator:" << std::endl;
  this->m_Interpolator->Print( os, indent );

  os << "Maximum number of iterations: " << this->m_MaximumNumberOfIterations << std::endl;
  os << "Max error tolerance threshold: " << this->m_MaxErrorToleranceThreshold << std::endl;
  os << "Mean error tolerance threshold: " << this->m_MeanErrorToleranceThreshold << std::endl;
}

}  //end namespace itk

#endif
