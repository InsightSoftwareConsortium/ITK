/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTobogganImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTobogganImageFilter_txx
#define __itkTobogganImageFilter_txx

#include "itkTobogganImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk {

template<class TInputImage>
TobogganImageFilter<TInputImage>
::TobogganImageFilter()
{
}

template<class TInputImage>
void
TobogganImageFilter<TInputImage>
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

template<class TInputImage>
void
TobogganImageFilter<TInputImage>
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

  

template<class TInputImage>
void
TobogganImageFilter<TInputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template<class TInputImage>
void
TobogganImageFilter<TInputImage>
::GenerateData( )
{
  InputImagePointer inputImage  = (InputImageType*) this->GetInput();
  OutputImagePointer outputImage = this->GetOutput();
  
  OutputImagePixelType z = NumericTraits<OutputImagePixelType>::Zero;
  OutputImagePixelType CurrentLabel = NumericTraits<OutputImagePixelType>::Zero;
  CurrentLabel += 2;
  
  // Zero the output
  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();
  outputImage->FillBuffer ( z );

  typedef ImageRegionConstIterator<InputImageType> InputIterator;
  typedef ImageRegionConstIterator<OutputImageType> OutputIterator;
  
  InputIterator  inIt(inputImage, inputImage->GetRequestedRegion() );
  OutputIterator outIt(outputImage, outputImage->GetRequestedRegion() );

  // Walk through the image
  while ( !inIt.IsAtEnd() )
    {
    // If we find an unlabeled pixel, start labeling
    if ( outIt.Get() == z )
      {
      // Start labeling
      std::vector<IndexType> Visited;
      InputImagePixelType MinimumNeighborValue = inIt.Get();
      OutputImagePixelType MinimumNeighborClass = CurrentLabel;
      OutputImagePixelType LabelForRegion = CurrentLabel;
      IndexType MinimumNeighborIndex;
      IndexType CurrentPositionIndex;
      unsigned int Dimension;
      unsigned int i;
      int t;
      bool FoundMinimum = false;

      CurrentPositionIndex = outIt.GetIndex();
      // This is the first pixel we've visited
      Visited.clear();
      Visited.push_back ( CurrentPositionIndex );
      itkDebugMacro ( << "Found unlabeled pixel at: " << CurrentPositionIndex
                      << " Value: " << MinimumNeighborValue );
      // Search along a steepest descent path to a local minimum
      do
        {
        // We've touched this pixel
        outputImage->SetPixel ( CurrentPositionIndex, 1 );
        MinimumNeighborIndex = CurrentPositionIndex;
        MinimumNeighborClass = 0 ;
        // DirectionImage->PutPixel ( CurrentPositionIndex, 1 );
        // Check the face connected neighbors
        for ( Dimension = 0; Dimension < ImageDimension; Dimension++ )
          {
          for ( t = 1; t >= -1; t = t-2 )
            {
            IndexType NeighborIndex;
            NeighborIndex = CurrentPositionIndex;
            NeighborIndex[Dimension] += t;
            if ( outputImage->GetRequestedRegion().IsInside ( NeighborIndex ) )
              {
              // This is a valid index to check
              // Possibilities:
              // If NeighborClass == 0  -> Not been checked yet, check it
              // If NeighborClass == 1  -> Currently added to region , just ignore
              // If NeighborClass > 1   -> Found a new neighbor, but only if it's minimum
              OutputImagePixelType NeighborClass;
              NeighborClass = outputImage->GetPixel ( NeighborIndex );
              // See if we've already touched it
              if ( NeighborClass != 1 )
                {
                InputImagePixelType NeighborValue = inputImage->GetPixel ( NeighborIndex );
                if ( NeighborValue < MinimumNeighborValue )
                  {
                  MinimumNeighborValue = inputImage->GetPixel ( NeighborIndex );
                  MinimumNeighborIndex = NeighborIndex;
                  MinimumNeighborClass = NeighborClass;
                  }
//                 else if ( NeighborValue == MinimumNeighborValue && NeighborClass )
//                   {
//                   // If it's classified already, compare, otherwise do nothing
//                   // Check to see if the neighbor class is less than the minimum
//                   // if so, move there
//                   // Also move if MinimumNeighborClass == 0
//                   if ( NeighborClass < MinimumNeighborClass || MinimumNeighborClass == 0 )
//                     {
//                     MinimumNeighborValue = inputImage->GetPixel ( NeighborIndex );
//                     MinimumNeighborIndex = NeighborIndex;
//                     MinimumNeighborClass = NeighborClass;
//                     }
//                   }
                }
              }
            }
          }
        itkDebugMacro( << "\tFound Neighbor at: " << MinimumNeighborIndex
                       << " Value: " << MinimumNeighborValue
                       << " Class: " << MinimumNeighborClass );
        FoundMinimum = false;
        if ( MinimumNeighborIndex != CurrentPositionIndex )
          {
          // Add the neighbor to the Visited list
          Visited.push_back ( MinimumNeighborIndex );
          CurrentPositionIndex = MinimumNeighborIndex;
          }
        else
          {
          FoundMinimum = true;
          }
        // Get the true class of this pixel
        MinimumNeighborClass = outputImage->GetPixel ( MinimumNeighborIndex );
        // we've slid into a different class
        if ( MinimumNeighborClass > 1 )
          {
          FoundMinimum = true;
          }
        } while ( !FoundMinimum );


      if ( MinimumNeighborClass == 1 )
        {
        // We need to flood fill from the last position we found
        // stored in CurrentPositionIndex.  Connect any pixels
        // having the same value as the minimum we found.
        // Conditions:
        //   NeighborClass == 0 -> add it to visited, and to the open list, mark as visited
        //   NeighborClass == LabelForRegion -> OK, just ignore
        //   otherwise, we have a problem
        
        std::vector<IndexType> OpenList;
        OpenList.clear();
        OpenList.push_back ( CurrentPositionIndex );
        itkDebugMacro ( << "\tFinished slide at: " << CurrentPositionIndex
                        << " Value: " << MinimumNeighborValue
                        << " Class: " << MinimumNeighborClass );
        while ( OpenList.size() )
          {
          // Pop the last one off
          IndexType SeedIndex = OpenList.back();
          OpenList.pop_back();
          Visited.push_back ( SeedIndex );
          itkDebugMacro ( << "Flood fill, looking at " << SeedIndex );
          // Look at the neighbors
          InputImagePixelType SeedValue;
          SeedValue = inputImage->GetPixel ( SeedIndex );
          for ( Dimension = 0; Dimension < ImageDimension; Dimension++ )
            {
            for ( t = -1; t <= 1; t = t+2 )
              {
              IndexType NeighborIndex;
              NeighborIndex = SeedIndex;
              NeighborIndex[Dimension] += t;
              if ( outputImage->GetRequestedRegion().IsInside ( NeighborIndex ) )
                {
                if ( inputImage->GetPixel ( NeighborIndex ) <= SeedValue )
                  {
                  // Found a match, check it's class
                  OutputImagePixelType NeighborClass = outputImage->GetPixel ( NeighborIndex );
                  // We've never seen this pixel before, so add it to the open list
                  if ( NeighborClass == z )
                    {
                    // Add it to the open list
                    OpenList.push_back ( NeighborIndex );
                    outputImage->SetPixel ( NeighborIndex, 1 );
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
      itkDebugMacro (  << "Filling in: " << static_cast<unsigned int>( Visited.size() )
                       << " with: " << LabelForRegion );
      // Loop over all the visited positions, setting their label
      for ( i = 0; i < Visited.size(); i++ )
        {
        outputImage->SetPixel ( Visited[i], LabelForRegion );
        }
      }
    // On to the next pixel      
    ++inIt;
    ++outIt;
    }
}

}// end namespace itk
  

#endif
