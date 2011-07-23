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
#ifndef __itkScalarImageToRunLengthMatrixFilter_hxx
#define __itkScalarImageToRunLengthMatrixFilter_hxx

#include "itkScalarImageToRunLengthMatrixFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "vnl/vnl_math.h"

namespace itk
{
namespace Statistics
{

template<class TImageType, class THistogramFrequencyContainer>
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::ScalarImageToRunLengthMatrixFilter() :
  m_NumberOfBinsPerAxis( itkGetStaticConstMacro( DefaultBinsPerAxis ) ),
  m_Min( NumericTraits<PixelType>::NonpositiveMin() ),
  m_Max( NumericTraits<PixelType>::max() ),
  m_MinDistance( NumericTraits<RealType>::Zero ),
  m_MaxDistance( NumericTraits<RealType>::max() ),
  m_InsidePixelValue( NumericTraits<PixelType>::One )
{
  this->SetNumberOfRequiredInputs( 1 );
  this->SetNumberOfRequiredOutputs( 1 );

  const unsigned int measurementVectorSize = 2;

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput( 0 ) );
  HistogramType *output = const_cast<HistogramType *>( this->GetOutput() );
  output->SetMeasurementVectorSize( measurementVectorSize );

  this->m_LowerBound.SetSize( measurementVectorSize );
  this->m_UpperBound.SetSize( measurementVectorSize );

  this->m_LowerBound[0] = this->m_Min;
  this->m_LowerBound[1] = this->m_MinDistance;
  this->m_UpperBound[0] = this->m_Max;
  this->m_UpperBound[1] = this->m_MaxDistance;
}

template<class TImageType, class THistogramFrequencyContainer>
void
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::SetOffset( const OffsetType offset )
{
  OffsetVectorPointer offsetVector = OffsetVector::New();
  offsetVector->push_back( offset );
  this->SetOffsets( offsetVector );
}

template<class TImageType, class THistogramFrequencyContainer>
void
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::SetInput( const ImageType *image )
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0, const_cast<ImageType *>( image ) );
}

template<class TImageType, class THistogramFrequencyContainer>
void
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::SetMaskImage( const ImageType *image )
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1, const_cast<ImageType *>( image ) );
}

template<class TImageType, class THistogramFrequencyContainer>
const TImageType *
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::GetInput() const
{
  if( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }
  return static_cast<const ImageType *>( this->ProcessObject::GetInput( 0 ) );
}

template<class TImageType, class THistogramFrequencyContainer>
const TImageType *
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::GetMaskImage() const
{
  if( this->GetNumberOfInputs() < 2 )
    {
    return 0;
    }
  return static_cast<const ImageType *>( this->ProcessObject::GetInput( 1 ) );
}

template<class TImageType, class THistogramFrequencyContainer>
const typename ScalarImageToRunLengthMatrixFilter<TImageType,
  THistogramFrequencyContainer >::HistogramType *
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::GetOutput() const
{
  const HistogramType *output =
    static_cast<const HistogramType *>( this->ProcessObject::GetOutput( 0 ) );
  return output;
}

template<class TImageType, class THistogramFrequencyContainer>
typename ScalarImageToRunLengthMatrixFilter<TImageType,
  THistogramFrequencyContainer>::DataObjectPointer
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::MakeOutput( unsigned int itkNotUsed( idx ) )
{
  typename HistogramType::Pointer output = HistogramType::New();
  return static_cast<DataObject *>( output );
}

template<class TImageType, class THistogramFrequencyContainer>
void
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::GenerateData()
{
  HistogramType *output =
    static_cast<HistogramType *>( this->ProcessObject::GetOutput( 0 ) );

  // First, create an appropriate histogram with the right number of bins
  // and mins and maxes correct for the image type.
  typename HistogramType::SizeType size( output->GetMeasurementVectorSize() );

  size.Fill( this->m_NumberOfBinsPerAxis );
  this->m_LowerBound[0] = this->m_Min;
  this->m_LowerBound[1] = this->m_MinDistance;
  this->m_UpperBound[0] = this->m_Max;
  this->m_UpperBound[1] = this->m_MaxDistance;
  output->Initialize( size, this->m_LowerBound, this->m_UpperBound );

  MeasurementVectorType run( output->GetMeasurementVectorSize() );

  // Iterate over all of those pixels and offsets, adding each
  // distance/intensity pair to the histogram

  typedef ConstNeighborhoodIterator<ImageType> NeighborhoodIteratorType;
  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );
  NeighborhoodIteratorType neighborIt( radius,
    this->GetInput(), this->GetInput()->GetRequestedRegion() );

  typename OffsetVector::ConstIterator offsets;
  for( offsets = this->GetOffsets()->Begin();
    offsets != this->GetOffsets()->End(); offsets++ )
    {
    typedef Image<bool, ImageDimension> BoolImageType;
    typename BoolImageType::Pointer alreadyVisitedImage = BoolImageType::New();
    alreadyVisitedImage->CopyInformation( this->GetInput() );
    alreadyVisitedImage->SetRegions( this->GetInput()->GetRequestedRegion() );
    alreadyVisitedImage->Allocate();
    alreadyVisitedImage->FillBuffer( false );

    neighborIt.GoToBegin();
    OffsetType offset = offsets.Value();

    for( neighborIt.GoToBegin(); !neighborIt.IsAtEnd(); ++neighborIt )
      {
      const PixelType centerPixelIntensity = neighborIt.GetCenterPixel();
      IndexType centerIndex = neighborIt.GetIndex();
      if( centerPixelIntensity < this->m_Min ||
        centerPixelIntensity > this->m_Max ||
        alreadyVisitedImage->GetPixel( centerIndex ) || ( this->GetMaskImage() &&
        this->GetMaskImage()->GetPixel( centerIndex ) !=
        this->m_InsidePixelValue ) )
        {
        continue; // don't put a pixel in the histogram if the value
                  // is out-of-bounds or is outside the mask.
        }

      MeasurementType centerBinMin = this->GetOutput()->
        GetBinMinFromValue( 0, centerPixelIntensity );
      MeasurementType centerBinMax = this->GetOutput()->
        GetBinMaxFromValue( 0, centerPixelIntensity );

      IndexType index = centerIndex;
      PixelType pixelIntensity = this->GetInput()->GetPixel( index );
      while( pixelIntensity >= centerBinMin &&
        pixelIntensity <= centerBinMax &&
        !alreadyVisitedImage->GetPixel( index ) )
        {
        alreadyVisitedImage->SetPixel( index, true );
        index += offset;
        if( this->GetInput()->GetRequestedRegion().IsInside( index ) )
          {
          pixelIntensity = this->GetInput()->GetPixel( index );
          }
        else
          {
          break;
          }
        }

      PointType centerPoint;
      this->GetInput()->TransformIndexToPhysicalPoint(
        centerIndex, centerPoint );
      PointType point;
      this->GetInput()->TransformIndexToPhysicalPoint( index, point );

      run[0] = centerPixelIntensity;
      run[1] = centerPoint.EuclideanDistanceTo( point );

      if( run[1] >= this->m_MinDistance && run[1] <= this->m_MaxDistance )
        {
        output->IncreaseFrequencyOfMeasurement( run, 1 );
        }
      }
    }
}

template<class TImageType, class THistogramFrequencyContainer>
void
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::SetPixelValueMinMax( PixelType min, PixelType max )
{
  if( this->m_Min != min || this->m_Max != max )
    {
    itkDebugMacro( "setting Min to " << min << "and Max to " << max );
    this->m_Min = min;
    this->m_Max = max;
    this->Modified();
    }
}

template<class TImageType, class THistogramFrequencyContainer>
void
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::SetDistanceValueMinMax( RealType min, RealType max )
{
  if( this->m_MinDistance != min || this->m_MaxDistance != max )
    {
    itkDebugMacro( "setting MinDistance to " << min << "and MaxDistance to "
      << max );
    this->m_MinDistance = min;
    this->m_MaxDistance = max;
    this->Modified();
    }
}

template<class TImageType, class THistogramFrequencyContainer>
void
ScalarImageToRunLengthMatrixFilter<TImageType, THistogramFrequencyContainer>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );
  os << indent << "Offsets: " << this->GetOffsets() << std::endl;
  os << indent << "Min: " << this->m_Min << std::endl;
  os << indent << "Max: " << this->m_Max << std::endl;
  os << indent << "Min distance: " << this->m_MinDistance << std::endl;
  os << indent << "Max distance: " << this->m_MaxDistance << std::endl;
  os << indent << "NumberOfBinsPerAxis: " << this->m_NumberOfBinsPerAxis
    << std::endl;
  os << indent << "InsidePixelValue: " << this->m_InsidePixelValue << std::endl;
}

} // end of namespace Statistics
} // end of namespace itk


#endif
