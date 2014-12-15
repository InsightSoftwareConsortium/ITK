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

#ifndef itkLevelSetDomainMapImageFilter_hxx
#define itkLevelSetDomainMapImageFilter_hxx

#include "itkLevelSetDomainMapImageFilter.h"

namespace itk
{
template < typename TInputImage, typename TOutputImage >
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >
::LevelSetDomainMapImageFilter()
{
  this->Superclass::SetNumberOfRequiredInputs ( 1 );
  this->Superclass::SetNumberOfRequiredOutputs ( 1 );
  this->m_InputImage = ITK_NULLPTR;
  this->m_OutputImage = ITK_NULLPTR;
}


template < typename TInputImage, typename TOutputImage >
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >
::~LevelSetDomainMapImageFilter()
{
}


template < typename TInputImage, typename TOutputImage >
const typename LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::DomainMapType &
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >
::GetDomainMap() const
{
  return this->m_DomainMap;
}

template < typename TInputImage, typename TOutputImage >
typename LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::InputImageRegionType
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >
::ComputeConsistentRegion( const InputImageRegionType & inputRegion ) const
{
  bool regionWasModified = false;

  InputImageRegionType subRegion = inputRegion;

  do
    {
    regionWasModified = false;

    InputConstIteratorType  iIt( this->m_InputImage, subRegion );
    OutputConstIteratorType oIt( this->m_OutputImage, subRegion );

    iIt.GoToBegin();
    oIt.GoToBegin();

    const InputImagePixelType firstCornerPixelValue = iIt.Get();
    const InputImageIndexType & firstCornerIndex = iIt.GetIndex();

    while( !iIt.IsAtEnd() )
      {
      const OutputImagePixelType segmentPixel = oIt.Get();
      const InputImagePixelType nextPixel = iIt.Get();

      if ( ( nextPixel != firstCornerPixelValue ) ||
           ( segmentPixel != NumericTraits< OutputImagePixelType >::ZeroValue() ) )
        {
        const InputImageIndexType & stopIdx = iIt.GetIndex();
        InputImageSizeType sizeOfRegion;
        for( unsigned int i = 0; i < ImageDimension; i++ )
          {
          sizeOfRegion[i] = stopIdx[i] - firstCornerIndex[i] + 1;
          }
        subRegion.SetSize(sizeOfRegion);

        regionWasModified = true;
        break;
        }

      ++iIt;
      ++oIt;
      }
    }
  while( regionWasModified );

  return subRegion;
}


template < typename TInputImage, typename TOutputImage >
void
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::
GenerateData()
{
  // Clear any prior contents.
  this->m_DomainMap.clear();

  this->m_InputImage =  this->GetInput();
  const InputImageRegionType & region = this->m_InputImage->GetLargestPossibleRegion();
  const InputImageSizeType size = region.GetSize();

  this->m_OutputImage = this->GetOutput();
  this->m_OutputImage->SetBufferedRegion( region );
  this->m_OutputImage->Allocate();
  this->m_OutputImage->FillBuffer( NumericTraits< OutputImagePixelType >::ZeroValue() );

  InputImageIndexType end;

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    end[i] = size[i] - 1;
    }

  IdentifierType segmentId = NumericTraits<IdentifierType>::OneValue();

  InputConstIteratorType iIt( this->m_InputImage, region );
  OutputIndexIteratorType oIt( this->m_OutputImage, region );

  iIt.GoToBegin();
  oIt.GoToBegin();

  while( !iIt.IsAtEnd() )
    {
    const InputImageIndexType & startIdx     = iIt.GetIndex();
    InputImageIndexType stopIdx              = startIdx;
    const InputImagePixelType & inputPixel   = iIt.Get();
    const OutputImagePixelType & outputPixel = oIt.Get();

    // outputPixel is null when it has not been processed yet,
    // or there is nothing to be processed
    if ( ( !inputPixel.empty() ) && ( outputPixel == NumericTraits<OutputImagePixelType>::ZeroValue() ) )
      {
      InputImageRegionType subRegion;
      InputImageSizeType sizeOfRegion;

      for( unsigned int i = 0; i < ImageDimension; i++ )
        {
        bool sameOverlappingLevelSetIds = true;
        stopIdx = startIdx;
        while ( ( sameOverlappingLevelSetIds ) && ( stopIdx[i] <= end[i] ) )
          {
          const InputImagePixelType & nextPixel = this->m_InputImage->GetPixel( stopIdx );
          const OutputImagePixelType & currentOutputPixel = this->m_OutputImage->GetPixel( stopIdx );

          // Check if the input list pixels are different, or
          // the output image already has been assigned to another region
          if ( ( nextPixel != inputPixel ) ||
               ( currentOutputPixel != NumericTraits< OutputImagePixelType >::ZeroValue() ) )
            {
            sameOverlappingLevelSetIds = false;
            }
          else
            {
            ++stopIdx[i];
            }
          }
        sizeOfRegion[i] = stopIdx[i] - startIdx[i];
        }

      subRegion.SetSize( sizeOfRegion );
      subRegion.SetIndex( startIdx );

      // Compute the consistent subregion
      subRegion = this->ComputeConsistentRegion( subRegion );

      this->m_DomainMap[segmentId] = LevelSetDomain( subRegion, inputPixel );

      OutputIndexIteratorType ooIt( this->m_OutputImage, subRegion );
      ooIt.GoToBegin();

      while( !ooIt.IsAtEnd() )
        {
        ooIt.Set( segmentId );
        ++ooIt;
        }
      ++segmentId;
      }
    ++iIt;
    ++oIt;
    }
}

template < typename TInputImage, typename TOutputImage >
void
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::
PrintSelf ( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf ( os,indent );
  os << indent << "DomainMap size: " << this->m_DomainMap.size() << std::endl;
}

} /* end namespace itk */

#endif
