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
#ifndef itkComposeDisplacementFieldsImageFilter_hxx
#define itkComposeDisplacementFieldsImageFilter_hxx

#include "itkComposeDisplacementFieldsImageFilter.h"

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/*
 * ComposeDisplacementFieldsImageFilter class definitions
 */
template<typename InputImage, typename TOutputImage>
ComposeDisplacementFieldsImageFilter<InputImage, TOutputImage>
::ComposeDisplacementFieldsImageFilter()
{
  this->SetNumberOfRequiredInputs( 2 );

  typedef VectorLinearInterpolateImageFunction<InputFieldType, RealType> DefaultInterpolatorType;
  typename DefaultInterpolatorType::Pointer interpolator = DefaultInterpolatorType::New();
  this->m_Interpolator = interpolator;
}

template<typename InputImage, typename TOutputImage>
ComposeDisplacementFieldsImageFilter<InputImage, TOutputImage>
::~ComposeDisplacementFieldsImageFilter()
{
}

template<typename InputImage, typename TOutputImage>
void
ComposeDisplacementFieldsImageFilter<InputImage, TOutputImage>
::SetInterpolator( InterpolatorType *interpolator )
{
  itkDebugMacro( "setting Interpolator to " << interpolator );
  if ( this->m_Interpolator != interpolator )
    {
    this->m_Interpolator = interpolator;
    this->Modified();
    if( this->GetDisplacementField() )
      {
      this->m_Interpolator->SetInputImage( this->GetInput( 0 ) );
      }
    }
}

template<typename InputImage, typename TOutputImage>
void
ComposeDisplacementFieldsImageFilter<InputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  VectorType zeroVector( 0.0 );

  this->GetOutput()->FillBuffer( zeroVector );

  if( !this->m_Interpolator->GetInputImage() )
    {
    itkExceptionMacro( "Displacement field not set in interpolator." );
    }
}

template<typename InputImage, typename TOutputImage>
void
ComposeDisplacementFieldsImageFilter<InputImage, TOutputImage>
::ThreadedGenerateData( const RegionType & region, ThreadIdType itkNotUsed( threadId ) )
{
  typename OutputFieldType::Pointer output = this->GetOutput();
  typename InputFieldType::ConstPointer warpingField = this->GetWarpingField();

  ImageRegionConstIteratorWithIndex<InputFieldType> ItW( warpingField, region );
  ImageRegionIterator<OutputFieldType> ItF( output, region );

  PointType pointIn1;
  PointType pointIn2;
  PointType pointIn3;

  typename OutputFieldType::PixelType outDisplacement;

  for( ItW.GoToBegin(), ItF.GoToBegin(); !ItW.IsAtEnd(); ++ItW, ++ItF )
    {
    warpingField->TransformIndexToPhysicalPoint( ItW.GetIndex(), pointIn1 );

    VectorType warpVector = ItW.Get();

    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      pointIn2[d] = pointIn1[d] + warpVector[d];
      }

    typename InterpolatorType::OutputType displacement( 0.0 );
    if( this->m_Interpolator->IsInsideBuffer( pointIn2 ) )
      {
      displacement = this->m_Interpolator->Evaluate( pointIn2 );
      }

    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      pointIn3[d] = pointIn2[d] + displacement[d];
      }

    outDisplacement = pointIn3 - pointIn1;

    ItF.Set( outDisplacement );
    }
}

template<typename InputImage, typename TOutputImage>
void
ComposeDisplacementFieldsImageFilter<InputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  itkPrintSelfObjectMacro( Interpolator );
}

}  //end namespace itk

#endif
