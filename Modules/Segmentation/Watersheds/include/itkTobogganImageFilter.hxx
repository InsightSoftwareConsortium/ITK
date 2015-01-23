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
#ifndef itkTobogganImageFilter_hxx
#define itkTobogganImageFilter_hxx

#include "itkTobogganImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
template< typename TInputImage >
TobogganImageFilter< TInputImage >
::TobogganImageFilter()
{}

template< typename TInputImage >
void
TobogganImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage >
void
TobogganImageFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage >
void
TobogganImageFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TInputImage >
void
TobogganImageFilter< TInputImage >
::GenerateData()
{
  InputImageConstPointer inputImage  = static_cast< InputImageConstPointer >( this->GetInput() );
  OutputImagePointer     outputImage = this->GetOutput();

  OutputImagePixelType z = NumericTraits< OutputImagePixelType >::ZeroValue();
  OutputImagePixelType CurrentLabel = NumericTraits< OutputImagePixelType >::ZeroValue();

  CurrentLabel += 2;

  // Zero the output
  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();
  outputImage->FillBuffer (z);

  typedef ImageRegionConstIterator< InputImageType >  InputIterator;
  typedef ImageRegionConstIterator< OutputImageType > OutputIterator;

  InputIterator  inIt( inputImage, inputImage->GetRequestedRegion() );
  OutputIterator outIt( outputImage, outputImage->GetRequestedRegion() );

  // Walk through the image
  while ( !inIt.IsAtEnd() )
    {
    // If we find an unlabeled pixel, start labeling
    if ( outIt.Get() == z )
      {
      // Start labeling
      std::vector< IndexType > Visited;
      InputImagePixelType      MinimumNeighborValue = inIt.Get();

      OutputImagePixelType MinimumNeighborClass;
      OutputImagePixelType LabelForRegion = CurrentLabel;
      IndexType            MinimumNeighborIndex;
      IndexType            CurrentPositionIndex;
      unsigned int         Dimension;
      unsigned int         i;
      int                  t;
      bool                 FoundMinimum = false;

      CurrentPositionIndex = outIt.GetIndex();
      // This is the first pixel we've visited
      Visited.clear();
      Visited.push_back (CurrentPositionIndex);
      itkDebugMacro (<< "Found unlabeled pixel at: " << CurrentPositionIndex
                     << " Value: " << MinimumNeighborValue);
      // Search along a steepest descent path to a local minimum
      do
        {
        // We've touched this pixel
        outputImage->SetPixel (CurrentPositionIndex, 1);
        MinimumNeighborIndex = CurrentPositionIndex;
        // DirectionImage->PutPixel ( CurrentPositionIndex, 1 );
        // Check the face connected neighbors
        for ( Dimension = 0; Dimension < ImageDimension; Dimension++ )
          {
          for ( t = 1; t >= -1; t = t - 2 )
            {
            IndexType NeighborIndex;
            NeighborIndex = CurrentPositionIndex;
            NeighborIndex[Dimension] += t;
            if ( outputImage->GetRequestedRegion().IsInside (NeighborIndex) )
              {
              // This is a valid index to check
              // Possibilities:
              // If NeighborClass == 0  -> Not been checked yet, check it
              // If NeighborClass == 1  -> Currently added to region , just
              // ignore
              // If NeighborClass > 1   -> Found a new neighbor, but only if
              // it's minimum
              OutputImagePixelType NeighborClass;
              NeighborClass = outputImage->GetPixel (NeighborIndex);
              // See if we've already touched it
              if ( NeighborClass != 1 )
                {
                InputImagePixelType NeighborValue = inputImage->GetPixel (NeighborIndex);
                if ( NeighborValue < MinimumNeighborValue )
                  {
                  MinimumNeighborValue = inputImage->GetPixel (NeighborIndex);
                  MinimumNeighborIndex = NeighborIndex;
                  }
                }
              }
            }
          }
        FoundMinimum = false;
        if ( MinimumNeighborIndex != CurrentPositionIndex )
          {
          // Add the neighbor to the Visited list
          Visited.push_back (MinimumNeighborIndex);
          CurrentPositionIndex = MinimumNeighborIndex;
          }
        else
          {
          FoundMinimum = true;
          }
        // Get the true class of this pixel
        MinimumNeighborClass = outputImage->GetPixel (MinimumNeighborIndex);
        itkDebugMacro(<< "\tFound Neighbor at: " << MinimumNeighborIndex
                      << " Value: " << MinimumNeighborValue
                      << " Class: " << MinimumNeighborClass);
        // we've slid into a different class
        if ( MinimumNeighborClass > 1 )
          {
          FoundMinimum = true;
          }
        }
      while ( !FoundMinimum );

      if ( MinimumNeighborClass == 1 )
        {
        // We need to flood fill from the last position we found
        // stored in CurrentPositionIndex.  Connect any pixels
        // having the same value as the minimum we found.
        // Conditions:
        //   NeighborClass == 0 -> add it to visited, and to the open list, mark
        // as visited
        //   NeighborClass == LabelForRegion -> OK, just ignore
        //   otherwise, we have a problem

        std::vector< IndexType > OpenList;
        OpenList.clear();
        OpenList.push_back (CurrentPositionIndex);
        itkDebugMacro (<< "\tFinished slide at: " << CurrentPositionIndex
                       << " Value: " << MinimumNeighborValue
                       << " Class: " << MinimumNeighborClass);
        while ( OpenList.size() )
          {
          // Pop the last one off
          IndexType SeedIndex = OpenList.back();
          OpenList.pop_back();
          Visited.push_back (SeedIndex);
          itkDebugMacro (<< "Flood fill, looking at " << SeedIndex);
          // Look at the neighbors
          InputImagePixelType SeedValue;
          SeedValue = inputImage->GetPixel (SeedIndex);
          for ( Dimension = 0; Dimension < ImageDimension; Dimension++ )
            {
            for ( t = -1; t <= 1; t = t + 2 )
              {
              IndexType NeighborIndex;
              NeighborIndex = SeedIndex;
              NeighborIndex[Dimension] += t;
              if ( outputImage->GetRequestedRegion().IsInside (NeighborIndex) )
                {
                if ( inputImage->GetPixel (NeighborIndex) <= SeedValue )
                  {
                  // Found a match, check it's class
                  OutputImagePixelType NeighborClass = outputImage->GetPixel (NeighborIndex);
                  // We've never seen this pixel before, so add it to the open
                  // list
                  if ( NeighborClass == z )
                    {
                    // Add it to the open list
                    OpenList.push_back (NeighborIndex);
                    outputImage->SetPixel (NeighborIndex, 1);
                    }
                  if ( NeighborClass > 1 )
                    {
                    MinimumNeighborClass = NeighborClass;
                    }
                  }
                }
              }
            }
          }
        }

      // The minimum neighbor class will always be >= 1
      if ( MinimumNeighborClass == 1 )
        {
        // If we found an unlabeled minimum,
        // Set the LabelForRegion, and increment CurrentLabel
        LabelForRegion = CurrentLabel;
        CurrentLabel++;
        }
      if ( MinimumNeighborClass > 1 )
        {
        // We bumped into another region, equivalent to finding the minimum
        LabelForRegion = MinimumNeighborClass;
        CurrentPositionIndex = MinimumNeighborIndex;
        }
      itkDebugMacro (<< "Filling in: " << static_cast< unsigned int >( Visited.size() )
                     << " with: " << LabelForRegion);
      // Loop over all the visited positions, setting their label
      for ( i = 0; i < Visited.size(); i++ )
        {
        outputImage->SetPixel (Visited[i], LabelForRegion);
        }
      }
    // On to the next pixel
    ++inIt;
    ++outIt;
    }
}
} // end namespace itk

#endif
