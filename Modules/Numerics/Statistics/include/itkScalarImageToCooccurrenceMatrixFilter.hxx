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
#ifndef itkScalarImageToCooccurrenceMatrixFilter_hxx
#define itkScalarImageToCooccurrenceMatrixFilter_hxx

#include "itkScalarImageToCooccurrenceMatrixFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{
template< typename TImageType, typename THistogramFrequencyContainer >
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >::ScalarImageToCooccurrenceMatrixFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );

  // constant for a coocurrence matrix.
  const unsigned int measurementVectorSize = 2;

  HistogramType *output = const_cast< HistogramType * >( this->GetOutput() );

  output->SetMeasurementVectorSize(measurementVectorSize);

  //initialize parameters
  this->m_LowerBound.SetSize(measurementVectorSize);
  this->m_UpperBound.SetSize(measurementVectorSize);

  this->m_LowerBound.Fill( NumericTraits< PixelType >::NonpositiveMin() );
  this->m_UpperBound.Fill(NumericTraits< PixelType >::max() + 1);

  this->m_Min = NumericTraits< PixelType >::NonpositiveMin();
  this->m_Max = NumericTraits< PixelType >::max();

  //mask inside pixel value
  this->m_InsidePixelValue = NumericTraits< PixelType >::OneValue();

  this->m_NumberOfBinsPerAxis = DefaultBinsPerAxis;
  this->m_Normalize = false;
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >
::SetOffset(const OffsetType offset)
{
  OffsetVectorPointer offsetVector = OffsetVector::New();

  offsetVector->push_back(offset);
  this->SetOffsets(offsetVector);
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >
::SetInput(const ImageType *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< ImageType * >( image ) );
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >
::SetMaskImage(const ImageType *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1,
                                    const_cast< ImageType * >( image ) );
}

template< typename TImageType, typename THistogramFrequencyContainer >
const TImageType *
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >
::GetInput() const
{
  return itkDynamicCastInDebugMode< const ImageType * >( this->GetPrimaryInput() );
}

template< typename TImageType, typename THistogramFrequencyContainer >
const TImageType *
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >
::GetMaskImage() const
{
  return static_cast< const ImageType * >( this->ProcessObject::GetInput(1) );
}

template< typename TImageType, typename THistogramFrequencyContainer >
const typename ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                                      THistogramFrequencyContainer >::HistogramType *
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >
::GetOutput() const
{
  const HistogramType *output =
    static_cast< const HistogramType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< typename TImageType, typename THistogramFrequencyContainer >
typename ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                                THistogramFrequencyContainer >::DataObjectPointer
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >
::MakeOutput( DataObjectPointerArraySizeType itkNotUsed(idx) )
{
  return HistogramType::New().GetPointer();
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >::GenerateData(void)
{
  HistogramType *output =
    static_cast< HistogramType * >( this->ProcessObject::GetOutput(0) );

  const ImageType *input = this->GetInput();

  // At this point input must be non-ITK_NULLPTR because the ProcessObject
  // checks the number of required input to be non-ITK_NULLPTR pointers before
  // calling this GenerateData() method.

  // First, create an appropriate histogram with the right number of bins
  // and mins and maxes correct for the image type.
  typename HistogramType::SizeType size( output->GetMeasurementVectorSize() );
  size.Fill(m_NumberOfBinsPerAxis);
  output->Initialize(size, m_LowerBound, m_UpperBound);

  // Next, find the minimum radius that encloses all the offsets.
  unsigned int minRadius = 0;
  typename OffsetVector::ConstIterator offsets;
  for ( offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); offsets++ )
    {
    for ( unsigned int i = 0; i < offsets.Value().GetOffsetDimension(); i++ )
      {
      unsigned int distance = itk::Math::abs(offsets.Value()[i]);
      if ( distance > minRadius )
        {
        minRadius = distance;
        }
      }
    }

  RadiusType radius;
  radius.Fill(minRadius);

  const ImageType *maskImage = ITK_NULLPTR;

  // Check if a mask image has been provided
  //
  if ( this->GetNumberOfIndexedInputs() > 1 )
    {
    maskImage = this->GetMaskImage();
    }

  // Now fill in the histogram
  if ( maskImage != ITK_NULLPTR )
    {
    this->FillHistogramWithMask(radius, input->GetRequestedRegion(), maskImage);
    }
  else
    {
    this->FillHistogram( radius, input->GetRequestedRegion() );
    }

  // Normalizse the histogram if requested
  if ( m_Normalize )
    {
    this->NormalizeHistogram();
    }
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >::FillHistogram(RadiusType radius,
                                                                                     RegionType region)
{
  // Iterate over all of those pixels and offsets, adding each
  // co-occurrence pair to the histogram

  const ImageType *input = this->GetInput();

  HistogramType *output =
    static_cast< HistogramType * >( this->ProcessObject::GetOutput(0) );

  typedef ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  NeighborhoodIteratorType neighborIt;
  neighborIt = NeighborhoodIteratorType(radius, input, region);

  MeasurementVectorType cooccur( output->GetMeasurementVectorSize() );

  for ( neighborIt.GoToBegin(); !neighborIt.IsAtEnd(); ++neighborIt )
    {
    const PixelType centerPixelIntensity = neighborIt.GetCenterPixel();
    if ( centerPixelIntensity < m_Min
         || centerPixelIntensity > m_Max )
      {
      continue; // don't put a pixel in the histogram if the value
                // is out-of-bounds.
      }

    typename OffsetVector::ConstIterator offsets;
    typename HistogramType::IndexType index;
    for ( offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); offsets++ )
      {
      bool            pixelInBounds;
      const PixelType pixelIntensity =
        neighborIt.GetPixel(offsets.Value(), pixelInBounds);

      if ( !pixelInBounds )
        {
        continue; // don't put a pixel in the histogram if it's out-of-bounds.
        }

      if ( pixelIntensity < m_Min
           || pixelIntensity > m_Max )
        {
        continue; // don't put a pixel in the histogram if the value
                  // is out-of-bounds.
        }

      // Now make both possible co-occurrence combinations and increment the
      // histogram with them.

      cooccur[0] = centerPixelIntensity;
      cooccur[1] = pixelIntensity;
      output->GetIndex( cooccur, index );
      output->IncreaseFrequencyOfIndex( index, 1 );

      cooccur[1] = centerPixelIntensity;
      cooccur[0] = pixelIntensity;
      output->GetIndex( cooccur, index );
      output->IncreaseFrequencyOfIndex( index, 1 );
      }
    }
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >::FillHistogramWithMask(RadiusType radius,
                                                                                             RegionType region,
                                                                                             const ImageType *maskImage)
{
  // Iterate over all of those pixels and offsets, adding each
  // co-occurrence pair to the histogram

  const ImageType *input = this->GetInput();

  HistogramType *output =
    static_cast< HistogramType * >( this->ProcessObject::GetOutput(0) );

  // Iterate over all of those pixels and offsets, adding each
  // co-occurrence pair to the histogram
  typedef ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  NeighborhoodIteratorType neighborIt, maskNeighborIt;
  neighborIt = NeighborhoodIteratorType(radius, input, region);
  maskNeighborIt = NeighborhoodIteratorType(radius, maskImage, region);

  MeasurementVectorType cooccur( output->GetMeasurementVectorSize() );
  typename HistogramType::IndexType index;
  for ( neighborIt.GoToBegin(), maskNeighborIt.GoToBegin();
        !neighborIt.IsAtEnd(); ++neighborIt, ++maskNeighborIt )
    {
    if ( maskNeighborIt.GetCenterPixel() != m_InsidePixelValue )
      {
      continue; // Go to the next loop if we're not in the mask
      }

    const PixelType centerPixelIntensity = neighborIt.GetCenterPixel();

    if ( centerPixelIntensity < this->GetMin()
         || centerPixelIntensity > this->GetMax() )
      {
      continue; // don't put a pixel in the histogram if the value
                // is out-of-bounds.
      }

    typename OffsetVector::ConstIterator offsets;
    for ( offsets = this->GetOffsets()->Begin(); offsets != this->GetOffsets()->End(); offsets++ )
      {
      if ( maskNeighborIt.GetPixel( offsets.Value() ) != m_InsidePixelValue )
        {
        continue; // Go to the next loop if we're not in the mask
        }

      bool            pixelInBounds;
      const PixelType pixelIntensity =
        neighborIt.GetPixel(offsets.Value(), pixelInBounds);

      if ( !pixelInBounds )
        {
        continue; // don't put a pixel in the histogram if it's out-of-bounds.
        }

      if ( pixelIntensity < this->GetMin()
           || pixelIntensity > this->GetMax() )
        {
        continue; // don't put a pixel in the histogram if the value
                  // is out-of-bounds.
        }

      // Now make both possible co-occurrence combinations and increment the
      // histogram with them.

      cooccur[0] = centerPixelIntensity;
      cooccur[1] = pixelIntensity;
      output->GetIndex( cooccur, index );
      output->IncreaseFrequencyOfIndex(index, 1);


      cooccur[1] = centerPixelIntensity;
      cooccur[0] = pixelIntensity;
      output->GetIndex( cooccur, index );
      output->IncreaseFrequencyOfIndex(index, 1);
      }
    }
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >::NormalizeHistogram(void)
{
  HistogramType *output =
    static_cast< HistogramType * >( this->ProcessObject::GetOutput(0) );

  typename HistogramType::AbsoluteFrequencyType totalFrequency =
    output->GetTotalFrequency();

  typename HistogramType::Iterator hit = output->Begin();
  while ( hit != output->End() )
    {
    hit.SetFrequency(hit.GetFrequency() / totalFrequency);
    ++hit;
    }
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >::SetPixelValueMinMax(PixelType min, PixelType max)
{
  itkDebugMacro("setting Min to " << min << "and Max to " << max);
  m_Min = min;
  m_Max = max;
  m_LowerBound.Fill(min);
  m_UpperBound.Fill(max + 1);
  this->Modified();
}

template< typename TImageType, typename THistogramFrequencyContainer >
void
ScalarImageToCooccurrenceMatrixFilter< TImageType,
                                       THistogramFrequencyContainer >::PrintSelf(std::ostream & os,
                                                                                 Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Offsets: " << this->GetOffsets() << std::endl;
  os << indent << "Min: " << this->GetMin() << std::endl;
  os << indent << "Max: " << this->GetMax() << std::endl;
  os << indent << "NumberOfBinsPerAxis: " << this->GetNumberOfBinsPerAxis() << std::endl;
  os << indent << "Normalize: " << this->GetNormalize() << std::endl;
  os << indent << "InsidePixelValue: " << this->GetInsidePixelValue() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
