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
#ifndef itkTileImageFilter_hxx
#define itkTileImageFilter_hxx
#include "itkTileImageFilter.h"

#include "itkMacro.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkPasteImageFilter.h"
#include "itkImportImageContainer.h"
#include "itkNumericTraitsRGBPixel.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
TileImageFilter< TInputImage, TOutputImage >
::TileImageFilter()
{
  m_Layout.Fill(0);
  m_DefaultPixelValue = NumericTraits< OutputPixelType >::ZeroValue();
}

template< typename TInputImage, typename TOutputImage >
void
TileImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  typename TOutputImage::Pointer output = this->GetOutput();


  typedef Image< InputPixelType, OutputImageDimension > TempImageType;

  // Allocate the output and initialize to default value
  this->AllocateOutputs();
  output->FillBuffer(m_DefaultPixelValue);

  ImageRegionIterator< TileImageType > it( m_TileImage, m_TileImage->GetBufferedRegion() );
  it.GoToBegin();

  SizeValueType numPastes = 0;
  while ( !it.IsAtEnd() )
    {
    if ( it.Get().m_ImageNumber >= 0 )
      {
      ++numPastes;
      }
    ++it;
    }
  const float progressContrib = 1.0f/numPastes;

  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    if ( it.Get().m_ImageNumber >= 0 )
      {
      typename PasteImageFilter< TOutputImage,
                                 TempImageType >::Pointer paste = PasteImageFilter< TOutputImage, TempImageType >::New();
      paste->SetDestinationImage(output);
      paste->InPlaceOn();

      progress->RegisterInternalFilter(paste, progressContrib);

      // Create a temporary image that has the same dimensions as the
      // output image. The additional dimensions are set to 1. The
      // temporary image will use the same container as the input
      // image. This way we avoid copying the data.
      typename TempImageType::Pointer tempImage = TempImageType::New();
      tempImage->CopyInformation( output );

      OutputSizeType  tempSize;
      OutputIndexType tempIndex;
      for ( unsigned int i = 0; i < InputImageDimension; i++ )
        {
        tempSize[i] = this->GetInput(it.Get().m_ImageNumber)->GetBufferedRegion().GetSize()[i];
        tempIndex[i] = this->GetInput(it.Get().m_ImageNumber)->GetBufferedRegion().GetIndex()[i];
        }
      for ( unsigned int i = InputImageDimension; i < OutputImageDimension; i++ )
        {
        tempSize[i] = 1;
        tempIndex[i] = 0;
        }
      OutputImageRegionType tempRegion(tempIndex, tempSize);
      tempImage->SetRegions( tempRegion );

      const TInputImage * inputImage = this->GetInput( it.Get().m_ImageNumber );

      typedef ImportImageContainer< SizeValueType, InputPixelType > PixelContainerType;

      tempImage->SetPixelContainer( const_cast< PixelContainerType * >( inputImage->GetPixelContainer() ) );

      paste->SetSourceImage(tempImage);
      paste->SetDestinationIndex( it.Get().m_Region.GetIndex() );
      paste->SetSourceRegion(tempRegion);
      paste->Update();
      output = paste->GetOutput();
      }
    ++it;
    }
  this->GraftOutput(output);
}

template< typename TInputImage, typename TOutputImage >
void
TileImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  for ( unsigned int i = 0; i < this->GetNumberOfIndexedInputs(); i++ )
    {
    TInputImage *input = const_cast< TInputImage * >( this->GetInput(i) );
    if ( input )
      {
      input->SetRequestedRegionToLargestPossibleRegion();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
TileImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // Supply spacing, origin and LargestPossibleRegion for the output.
  // This method does most of the work for this filter. In addition to
  // the spacing, origin and region, this method computes the source
  // region and destination index for multiple invocations of the
  // PasteImageFilter.
  OutputImagePointer outputPtr = this->GetOutput();
  InputImagePointer  inputPtr = const_cast< TInputImage * >( this->GetInput() );

  if ( !outputPtr || !inputPtr )
    {
    return;
    }

  // Spacing(Origin): use the spacing(origin) of the first input for
  // all of the matching output dimensions. For remaining dimensions
  // use 1.0 (0.0).
  SpacePrecisionType spacing[OutputImageDimension];
  SpacePrecisionType origin[OutputImageDimension];

  for ( unsigned i = 0; i < OutputImageDimension; i++ )
    {
    if ( i < InputImageDimension )
      {
      spacing[i] = this->GetInput(0)->GetSpacing()[i];
      origin[i] = this->GetInput(0)->GetOrigin()[i];
      }
    else
      {
      spacing[i] = 1.0;
      origin[i] = 0.0;
      }
    }

  outputPtr->SetSpacing (spacing);
  outputPtr->SetOrigin (origin);

  // Create an image that has one element per tile. Each element of
  // this tile image will hold a class that defines the image number
  // for the tile, the size of the image and the destination index for
  // the image in the final composed image.
  typedef ImageRegion< OutputImageDimension > RegionType;
  m_TileImage = TileImageType::New();

  // Determine the last dimension for the tile image. This dimension will
  // be large enough to accommodate left-over images.
  OutputSizeType  outputSize; outputSize.Fill(1);
  OutputIndexType outputIndex; outputIndex.Fill(0);

  if ( m_Layout[OutputImageDimension - 1] == 0 )
    {
    int used = 1;
    for ( unsigned int d = 0; d < OutputImageDimension - 1; d++ )
      {
      used *= m_Layout[d];
      }
    outputSize[OutputImageDimension - 1] = ( static_cast<SizeValueType>( this->GetNumberOfIndexedInputs() ) - 1 ) / used + 1;
    if ( outputSize[OutputImageDimension - 1] < 1 )
      {
      outputSize[OutputImageDimension - 1] = 1;
      }
    m_Layout[OutputImageDimension - 1] = outputSize[OutputImageDimension - 1];
    }

  OutputSizeType  tileSize; tileSize.Fill(1);
  OutputIndexType tileIndex; tileIndex.Fill(0);

  for ( unsigned int i = 0; i < OutputImageDimension; i++ )
    {
    tileSize[i] = m_Layout[i];
    }

  // Determine the size of the output. Each "row" size is determined
  // and the maximum size for each "row" will be the size for that
  // dimension.
  RegionType tileRegion(tileIndex, tileSize);
  m_TileImage->SetRegions(tileRegion);
  m_TileImage->Allocate();

  // Initialize the tile image with an image number for each tile. If
  // there is no corresponding input image for a tile, then set the
  // image number to -1.
  ImageRegionIteratorWithIndex< TileImageType > it( m_TileImage, m_TileImage->GetBufferedRegion() );

  it.GoToBegin();
  unsigned int input = 0;
  TileInfo     info;
  while ( !it.IsAtEnd() )
    {
    if ( input < this->GetNumberOfIndexedInputs() )
      {
      info.m_ImageNumber = input;
      it.Set (info);
      }
    else
      {
      info.m_ImageNumber = -1;
      it.Set (info);
      }
    ++input;
    ++it;
    }

  // Find the size of the largest cell for each "row" in each dimension.
  ImageLinearConstIteratorWithIndex< TileImageType > tit( m_TileImage, m_TileImage->GetRequestedRegion() );
  int                                                value;

  std::vector< std::vector< int > > sizes, offsets;

  sizes.resize(OutputImageDimension);
  offsets.resize(OutputImageDimension);
  for ( unsigned int i = 0; i < OutputImageDimension; i++ )
    {
    offsets[i].resize(m_Layout[i]);
    sizes[i].resize(m_Layout[i]);
    for ( unsigned int l = 0; l < m_Layout[i]; l++ )
      {
      sizes[i][l] = 1;
      }
    }
  for ( unsigned int i = 0; i < OutputImageDimension; i++ )
    {
    tit.SetDirection(i);
    tit.GoToBegin();
    while ( !tit.IsAtEnd() )
      {
      int dsize;
      int count = 0;
      while ( !tit.IsAtEndOfLine() )
        {
        value = tit.Get().m_ImageNumber;
        if ( value != -1 )
          {
          if ( i < InputImageDimension )
            {
            dsize = this->GetInput(value)->GetLargestPossibleRegion().GetSize()[i];
            if ( dsize > sizes[i][count] )
              {
              sizes[i][count] = dsize;
              }
            }
          }
        ++tit;
        ++count;
        }
      tit.NextLine();
      }
    }

  // Convert the sizes to offsets.
  for ( unsigned int i = 0; i < OutputImageDimension; i++ )
    {
    offsets[i][0] = 0;
    for ( unsigned int t = 0; t < m_Layout[i] - 1; t++ )
      {
      offsets[i][t + 1] = offsets[i][t] + sizes[i][t];
      }
    // The size for each dimension is the value of the last offset
    outputSize[i] = offsets[i][m_Layout[i] - 1] + sizes[i][m_Layout[i] - 1];
    }

  // Now create an region for each tile that has an image
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    value = it.Get().m_ImageNumber;
    if ( value >= 0 )
      {
      typename TileImageType::IndexType tileIndex2 = it.GetIndex();

      OutputSizeType  regionSize;
      OutputIndexType regionIndex;
      for ( unsigned int i = 0; i < OutputImageDimension; i++ )
        {
        regionIndex[i] = offsets[i][tileIndex2[i]];
        if ( i < InputImageDimension )
          {
          regionSize[i] = this->GetInput(value)->GetLargestPossibleRegion().GetSize()[i];
          }
        else
          {
          regionSize[i] = 1;
          }
        }
      OutputImageRegionType region(regionIndex, regionSize);
      info = it.Get();
      info.m_Region = region;
      it.Set(info);
      }
    ++it;
    }

  typename TOutputImage::RegionType outputLargestPossibleRegion;

  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputIndex);
  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template< typename TInputImage, typename TOutputImage >
void
TileImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "DefaultPixelValue: "
            << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_DefaultPixelValue )
            << std::endl;
  os << "Layout: " << m_Layout << std::endl;
}
} // end namespace itk
#endif
