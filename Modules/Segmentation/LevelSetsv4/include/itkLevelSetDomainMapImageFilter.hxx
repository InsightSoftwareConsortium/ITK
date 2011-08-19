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

#ifndef __itkLevelSetDomainMapImageFilter_hxx
#define __itkLevelSetDomainMapImageFilter_hxx

#include "itkLevelSetDomainMapImageFilter.h"

namespace itk
{
template < class TInputImage, class TOutputImage >
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >
::LevelSetDomainMapImageFilter()
{
  this->Superclass::SetNumberOfRequiredInputs ( 1 );
  this->Superclass::SetNumberOfRequiredOutputs ( 1 );

  this->Superclass::SetNthOutput ( 0, OutputImageType::New() );
}


template < class TInputImage, class TOutputImage >
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >
::~LevelSetDomainMapImageFilter()
{
}


template < class TInputImage, class TOutputImage >
typename LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::InputImageRegionType
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::
ComputeConsistentRegion( const InputImageRegionType & inputRegion ) const
{
  bool regionWasModified = false;

  const InputImageType  * input  = this->GetInput();
  const OutputImageType * output = this->GetOutput();

  InputImageRegionType subRegion = inputRegion;

  do
    {
    regionWasModified = false;

    InputConstIteratorType  iIt( input, subRegion );
    OutputConstIteratorType oIt( output, subRegion );

    iIt.GoToBegin();
    oIt.GoToBegin();

    const InputImagePixelType firstCornerPixelValue = iIt.Get();
    const InputImageIndexType & firstCornerIndex = iIt.GetIndex();

    while( !iIt.IsAtEnd() )
      {
      const OutputImagePixelType segmentPixel = oIt.Get();
      const InputImagePixelType nextPixel = iIt.Get();

      if ( ( nextPixel != firstCornerPixelValue ) ||
           ( segmentPixel != NumericTraits< OutputImagePixelType >::Zero ) )
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


template < class TInputImage, class TOutputImage >
void
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::
GenerateData()
{
  InputImageConstPointer input = this->GetInput();
  const InputImageRegionType & region = input->GetLargestPossibleRegion();
  const InputImageSizeType size = region.GetSize();

  OutputImagePointer output = this->GetOutput();
  output->CopyInformation( input );
  output->SetRegions( region );
  output->Allocate();
  output->FillBuffer( NumericTraits< OutputImagePixelType >::Zero );

  InputImageIndexType end;

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    end[i] = size[i] - 1;
    }

  IdentifierType segmentId = NumericTraits<IdentifierType>::One;

  InputConstIteratorType iIt( input, region );
  OutputIndexIteratorType oIt( output, region );

  iIt.GoToBegin();
  oIt.GoToBegin();

  while( !iIt.IsAtEnd() )
    {
    const InputImageIndexType & startIdx = iIt.GetIndex();
    InputImageIndexType stopIdx = startIdx;
    const InputImagePixelType & inputPixel = iIt.Get();
    const OutputImagePixelType & outputPixel = oIt.Get();

    // outputPixel is null when it has not been processed yet,
    // or there is nothing to be processed
    if ( ( !inputPixel.empty() ) && ( outputPixel == NumericTraits<OutputImagePixelType>::Zero ) )
      {
      InputImageRegionType subRegion;
      InputImageSizeType sizeOfRegion;

      for( unsigned int i = 0; i < ImageDimension; i++ )
        {
        bool sameOverlappingLevelSetIds = true;
        stopIdx = startIdx;
        while ( ( sameOverlappingLevelSetIds ) && ( stopIdx[i] <= end[i] ) )
          {
          const InputImagePixelType & nextPixel = input->GetPixel( stopIdx );
          const OutputImagePixelType & currentOutputPixel = output->GetPixel( stopIdx );

          // Check if the input list pixels are different, or
          // the output image already has been assigned to another region
          if ( ( nextPixel != inputPixel ) ||
               ( currentOutputPixel != NumericTraits< OutputImagePixelType >::Zero ) )
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

      this->m_LevelSetMap[segmentId] = LevelSetDomain( subRegion, inputPixel );

      OutputIndexIteratorType ooIt( output, subRegion );
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

  this->GraftOutput ( output );
}

template < class TInputImage, class TOutputImage >
void
LevelSetDomainMapImageFilter< TInputImage, TOutputImage >::
PrintSelf ( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf ( os,indent );
  os << indent << "Class Name:        " << this->GetNameOfClass() << std::endl;
}

} /* end namespace itk */

#endif
