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

static void foo() { int i; i = 1; i = i+ 1; }
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
  foo ();
  InputImagePointer inputImage  = (InputImageType*) this->GetInput();
  OutputImagePointer outputImage = this->GetOutput();
  
  OutputImagePixelType z = NumericTraits<OutputImagePixelType>::Zero;
  OutputImagePixelType CurrentLabel = NumericTraits<OutputImagePixelType>::Zero;
  CurrentLabel += 2;
  
  // Zero the output
  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();
  outputImage->FillBuffer ( z );

  // Zero the buffer
  typedef typename OutputImageType::OffsetType::OffsetValueType OffsetValueType;
  typedef Image<OffsetValueType, ImageDimension> DirectionImageType;
  DirectionImageType::Pointer DirectionImage = DirectionImageType::New();

  typename DirectionImageType::RegionType tempRegion;
  tempRegion.SetSize( outputImage->GetLargestPossibleRegion().GetSize() );
  DirectionImage->SetLargestPossibleRegion( tempRegion );
  DirectionImage->SetBufferedRegion( tempRegion );
  DirectionImage->SetRequestedRegion( tempRegion );
  DirectionImage->Allocate();
  DirectionImage->FillBuffer ( 0 );

  typedef ImageRegionConstIterator<InputImageType> InputIterator;
  typedef ImageRegionConstIterator<OutputImageType> OutputIterator;
  
  InputIterator  inIt(inputImage, inputImage->GetRequestedRegion() );
  OutputIterator outIt(outputImage, outputImage->GetRequestedRegion() );
  OffsetValueType d;
  
  while ( !inIt.IsAtEnd() )
    {
    if ( outIt.Get() == z )
      {
      // Start labeling
      //std::vector<IndexType> Visited;
      InputImagePixelType MinimumNeighborValue = inIt.Get();
      OutputImagePixelType LabelForRegion = CurrentLabel;
      IndexType MinimumNeighborIndex;
      IndexType CurrentPositionIndex;
      unsigned int Dimension;
      int t;
      bool FoundNewClass = false;
      bool FoundMinimum = false;

      // cerr << "Starting loop at " << inIt.GetIndex()
      // << " Value is: " << MinimumNeighborValue << endl;
      

      CurrentPositionIndex = outIt.GetIndex();
      do
        {
        // We've touched this pixel
        outputImage->SetPixel ( CurrentPositionIndex, 1 );
        MinimumNeighborIndex = CurrentPositionIndex;
        // DirectionImage->PutPixel ( CurrentPositionIndex, 1 );
        // Check the face connected neighbors
        for ( Dimension = 0; Dimension < ImageDimension; Dimension++ )
          {
          for ( t = -1; t <= 1; t = t+2 )
            {
            IndexType NeighborIndex;
            NeighborIndex = CurrentPositionIndex;
            NeighborIndex[Dimension] += t;
            if ( outputImage->GetRequestedRegion().IsInside ( NeighborIndex ) )
              {
              OutputImagePixelType NeighborClass;
              NeighborClass = outputImage->GetPixel ( NeighborIndex );
              // See if we've already touched it
              if ( NeighborClass != 1 )
               { 
               if ( inputImage->GetPixel ( NeighborIndex ) <= MinimumNeighborValue )
                 {
                 MinimumNeighborValue = inputImage->GetPixel ( NeighborIndex );
                 MinimumNeighborIndex = NeighborIndex;
                 }
               }
              }
            }
          }
        // Go through again, looking for a minimum w/an existing class
        for ( Dimension = 0; Dimension < ImageDimension; Dimension++ )
          {
          for ( t = -1; t <= 1; t = t+2 )
            {
            IndexType NeighborIndex;
            NeighborIndex = CurrentPositionIndex;
            NeighborIndex[Dimension] += t;
            if ( outputImage->GetRequestedRegion().IsInside ( NeighborIndex ) )
              {
              OutputImagePixelType NeighborClass;
              NeighborClass = outputImage->GetPixel ( NeighborIndex );
              // See if we've already touched it
              if ( NeighborClass > 1 && inputImage->GetPixel ( NeighborIndex ) == MinimumNeighborValue )
                {
                MinimumNeighborIndex = NeighborIndex;
                }
              }
            }
          }
        
        // Put the offset from the neighbor pixel to the current pixel
        // in the current pixel
        d = DirectionImage->ComputeOffset ( MinimumNeighborIndex ) - 
          DirectionImage->ComputeOffset ( CurrentPositionIndex );

        // cerr << "\tPutting direction " << d << " at " << CurrentPositionIndex << endl;
        DirectionImage->SetPixel ( CurrentPositionIndex, d );
        
        CurrentPositionIndex = MinimumNeighborIndex;
        
        FoundMinimum = ( d == 0 ) || ( outputImage->GetPixel ( CurrentPositionIndex ) != z );
        } while ( !FoundMinimum );

      LabelForRegion = outputImage->GetPixel ( CurrentPositionIndex );
      if ( LabelForRegion == 1 )
        {
        LabelForRegion = CurrentLabel;
        CurrentLabel++;
        }
      
      outputImage->SetPixel ( CurrentPositionIndex, LabelForRegion );
      OffsetValueType index, delta;
      index = outputImage->ComputeOffset ( outIt.GetIndex() );
      // cerr << "\tStarting slide back at " << outIt.GetIndex()  << " Label " << LabelForRegion << endl;
      do
        {
        outputImage->GetBufferPointer()[index] = LabelForRegion;
        // cerr << "\t\tIndex is " << index << endl;
        delta = DirectionImage->GetBufferPointer()[index];
        // cerr << "\t\tFound Delta of " << delta << endl;
        index = index + DirectionImage->GetBufferPointer()[index];
        // cerr << "\t\tValue at new index: " << outputImage->GetBufferPointer()[index] << endl;
        } while ( delta && index && outputImage->GetBufferPointer()[index] == 1 );
      }
    // On to the next pixel      
    ++inIt;
    ++outIt;
    }
}

}// end namespace itk
  

#endif
