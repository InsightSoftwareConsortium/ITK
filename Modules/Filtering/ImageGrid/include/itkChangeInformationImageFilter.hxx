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
#ifndef itkChangeInformationImageFilter_hxx
#define itkChangeInformationImageFilter_hxx

#include "itkChangeInformationImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkContinuousIndex.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage >
ChangeInformationImageFilter< TInputImage >
::ChangeInformationImageFilter()
{
  m_ReferenceImage = ITK_NULLPTR;

  m_ChangeSpacing = false;
  m_ChangeOrigin = false;
  m_ChangeDirection = false;
  m_ChangeRegion = false;

  m_CenterImage = false;
  m_UseReferenceImage = false;

  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  m_OutputDirection.SetIdentity();
  m_OutputOffset.Fill(0);
}

template< typename TInputImage >
void
ChangeInformationImageFilter< TInputImage >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  unsigned int i;

  typename TInputImage::RegionType outputRegion;
  typename TInputImage::SizeType inputSize;
  typename TInputImage::SizeType outputSize;
  typename TInputImage::IndexType outputIndex;
  typename TInputImage::IndexType inputIndex;
  PointType     origin;
  SpacingType   spacing;
  DirectionType direction;

  itkDebugMacro("GenerateOutputInformation Start");

  // Get pointers to the input and output
  typename Superclass::OutputImagePointer output = this->GetOutput();
  typename Superclass::InputImagePointer input =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !output || !input )
    {
    return;
    }

  inputIndex = input->GetLargestPossibleRegion().GetIndex();

  // Default is to copy input's information
  output->CopyInformation(input);

  // Output size is always the same as input size
  inputSize = input->GetLargestPossibleRegion().GetSize();
  outputSize = inputSize;

  // Establish the source of the image information
  if ( m_UseReferenceImage && m_ReferenceImage )
    {
    outputIndex = m_ReferenceImage->GetLargestPossibleRegion().GetIndex();
    origin = m_ReferenceImage->GetOrigin();
    spacing = m_ReferenceImage->GetSpacing();
    direction = m_ReferenceImage->GetDirection();
    m_Shift = outputIndex - inputIndex;

    // reset outputIndex to the input index since we add m_Shift to
    // outputIndex latter on
    outputIndex = input->GetLargestPossibleRegion().GetIndex();
    }
  else
    {
    outputIndex = input->GetLargestPossibleRegion().GetIndex();
    origin = m_OutputOrigin;
    spacing = m_OutputSpacing;
    direction = m_OutputDirection;
    m_Shift = m_OutputOffset;
    }

  // Change the output spacing
  if ( m_ChangeSpacing )
    {
    output->SetSpacing(spacing);
    }

  // Change the output origin
  if ( m_ChangeOrigin )
    {
    output->SetOrigin(origin);
    }

  // Change the output direction
  if ( m_ChangeDirection )
    {
    output->SetDirection(direction);
    }

  // Center the image by changing its origin
  // The center of the image is computed using the index to point
  // transformation. This ensures that the computation will work for
  // both images and oriented images.
  if ( m_CenterImage )
    {
    typename TInputImage::PointType centerPoint;
    ContinuousIndex<SpacePrecisionType, ImageDimension > centerIndex;

    for ( i = 0; i < ImageDimension; i++ )
      {
      centerIndex[i] = static_cast< double >( ( outputSize[i] - 1 ) / 2.0 );
      }
    output->TransformContinuousIndexToPhysicalPoint(centerIndex, centerPoint);
    for ( i = 0; i < ImageDimension; i++ )
      {
      origin[i] = output->GetOrigin()[i] - centerPoint[i];
      }
    output->SetOrigin(origin);
    }

  // Change the output's largest possible region
  if ( m_ChangeRegion )
    {
    outputRegion.SetSize(outputSize);
    outputRegion.SetIndex(outputIndex + m_Shift);
    output->SetLargestPossibleRegion(outputRegion);
    }
  else
    {
    m_Shift.Fill(0);
    }
  itkDebugMacro("GenerateOutputInformation End");
}

template< typename TInputImage >
void
ChangeInformationImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  if ( this->GetInput() )
    {
    typename TInputImage::RegionType region;
    region.SetSize( this->GetOutput()->GetRequestedRegion().GetSize() );
    region.SetIndex(this->GetOutput()->GetRequestedRegion().GetIndex() - m_Shift);
    InputImageType * input = const_cast< InputImageType * >( this->GetInput() );
    input->SetRequestedRegion (region);
    }
}

template< typename TInputImage >
void
ChangeInformationImageFilter< TInputImage >
::GenerateData()
{
  // Get pointers to the input and output
  OutputImageType * output = this->GetOutput();
  const InputImageType * input = this->GetInput();

  InputImageType * nonConstInput = const_cast< InputImageType * >( input );

  // No need to copy the bulk data
  output->SetPixelContainer( nonConstInput->GetPixelContainer() );

  // Shift the output's buffer region
  typename TInputImage::RegionType region;
  region.SetSize( input->GetBufferedRegion().GetSize() );
  region.SetIndex(input->GetBufferedRegion().GetIndex() + m_Shift);

  output->SetBufferedRegion(region);
}

/**
 *
 */
template< typename TInputImage >
void
ChangeInformationImageFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "CenterImage: " << ( m_CenterImage ? "On" : "Off" ) << std::endl;
  os << indent << "ChangeSpacing: " << ( m_ChangeSpacing ? "On" : "Off" ) << std::endl;
  os << indent << "ChangeOrigin: " << ( m_ChangeOrigin ? "On" : "Off" ) << std::endl;
  os << indent << "ChangeDirection: " << ( m_ChangeDirection ? "On" : "Off" ) << std::endl;
  os << indent << "ChangeRegion: " << ( m_ChangeRegion ? "On" : "Off" ) << std::endl;
  os << indent << "UseReferenceImage: " << ( m_UseReferenceImage ? "On" : "Off" ) << std::endl;
  if ( m_ReferenceImage )
    {
    os << indent << "ReferenceImage: " << m_ReferenceImage.GetPointer() << std::endl;
    }
  else
    {
    os << indent << "ReferenceImage: 0" << std::endl;
    }
  os << indent << "OutputSpacing: [";
  if ( ImageDimension >= 1 )
    {
    os << m_OutputSpacing[0];
    }
  for ( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_OutputSpacing[j];
    }
  os << "]" << std::endl;

  os << indent << "OutputOrigin: [";
  if ( ImageDimension >= 1 )
    {
    os << m_OutputOrigin[0];
    }
  for ( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_OutputOrigin[j];
    }
  os << "]" << std::endl;

  os << indent << "OutputDirection:" << std::endl;
  os << m_OutputDirection << std::endl;

  os << indent << "OutputOffset: [";
  os << m_OutputOffset << std::endl;
}
} // end namespace itk

#endif
