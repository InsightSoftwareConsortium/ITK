/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledFunctionConditionalConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFloodFilledFunctionConditionalConstIterator_txx
#define _itkFloodFilledFunctionConditionalConstIterator_txx

#include "itkFloodFilledFunctionConditionalConstIterator.h"

namespace itk
{
template<class TImage, class TFunction>
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::FloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                   FunctionType *fnPtr,
                                   IndexType startIndex)
{
  m_Image = imagePtr;
  m_Function = fnPtr;
  m_StartIndex = startIndex;

  // Initialize the stack by adding the start index
  m_IndexStack.push(m_StartIndex);

  // Get the origin and spacing from the image in simple arrays
  m_ImageOrigin = m_Image->GetOrigin();
  m_ImageSpacing = m_Image->GetSpacing();
  m_ImageSize = m_Image->GetLargestPossibleRegion().GetSize().m_Size;

  // Build a temporary image of chars for use in the flood algorithm
  m_Image->SetRequestedRegionToLargestPossibleRegion();
  SizeType imageSize = m_Image->GetLargestPossibleRegion().GetSize();
  tempPtr = TTempImage::New();
  TTempImage::RegionType tempRegion;
  tempRegion.SetSize( imageSize );
  tempPtr->SetLargestPossibleRegion( tempRegion );
  tempPtr->SetBufferedRegion( tempRegion );
  tempPtr->SetRequestedRegion( tempRegion );
  tempPtr->Allocate();
  tempPtr->FillBuffer(NumericTraits<TTempImage::PixelType>::Zero);
  m_IsAtEnd = false;
}

template<class TImage, class TFunction>
void
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::DoFloodStep()
{
  m_FoundUncheckedNeighbor = false;
  m_IsValidIndex = true;

  do
    {
    // There are two cases for arriving at a pixel
    // 1) We were "referred" to this pixel by another pixel in a neighborhood check
    // 2) We're coming back to this pixel after checking its neighbors' neighbors
    // If we haven't yet visited this pixel, see if it's inside the function
    if( tempPtr->GetPixel(m_IndexStack.top() ) == 0)
      {
      // Is this pixel inside the function?
      if ( this->IsPixelIncluded( m_IndexStack.top() ) )
        {
        // if it is, mark as inside
        tempPtr->SetPixel( m_IndexStack.top(), 2);
        // kick out of this function, since we found a new pixel
        return;
        }
      else
        {
        // pixel is not inside the function, mark it
        tempPtr->SetPixel( m_IndexStack.top(), 1);
        // pop off this pixel
        m_IndexStack.pop();
        continue;
        }
    } // end if we haven't visited the pixel

    // Now look at all of this pixel's neighbors
    // For an image in n-dimensions, there are 2n neighbors
    // We're only interested in the nearest neighbors along each axis
    m_FoundUncheckedNeighbor = false;

    // i loop runs through all possible dimensions
    for(int i=0; i<NDimensions; i++)
      {
      IndexType tempIndex;
      m_IsValidIndex = true;

      // The j loop establishes either left or right neighbor (+-1)
      for(int j=-1; j<=1; j+=2)
        {
        // build the index of a neighbor
        for(int k=0; k<NDimensions; k++)
          {
          if( i!=k )
            tempIndex.m_Index[k] = m_IndexStack.top().m_Index[k];
          else
            {
            tempIndex.m_Index[k] = m_IndexStack.top().m_Index[k] + j;
            if( (tempIndex.m_Index[k] < 0) || 
                (tempIndex.m_Index[k] >= static_cast<long int>(m_ImageSize[k])) )
              {
              m_IsValidIndex = false;
              continue;
              }
            }
          } // end build the index of a neighbor

        // If this isn't a valid index, loop to the next one
        if(m_IsValidIndex==false)
          continue;

        // Now check the neighbor and see if it hasn't been examined yet
        if(tempPtr->GetPixel(tempIndex)==0 &&
           this->IsPixelIncluded(tempIndex)) // if pixel hasn't been checked and is inside
          {
          // push it onto the stack, this is the next pixel we'll look at
          m_FoundUncheckedNeighbor=true;
          m_IndexStack.push(tempIndex);
          continue;
          }

        } // end left/right neighbor loop

      // If we've found an unchecked neighbor, force its evaluation
      if(m_FoundUncheckedNeighbor==true)
        continue;
      
    } // end check all neighbors

    // If we made it this far - i.e. checked all neighbors - and none of them
    // were unexamined, it's time to go back to the pixel we were at before
    if(m_FoundUncheckedNeighbor==false)
      {
      // Mark the pixel as finished
      if( tempPtr->GetPixel( m_IndexStack.top() )==2 )
        {
        tempPtr->SetPixel( m_IndexStack.top(), 3);
        }
      
      // Move the stack up
      m_IndexStack.pop();
      }

    } while(!(m_IndexStack.empty())); // loop while there are pixels left on the stack

  // if we made it this far, the stack is now empy
  m_IsAtEnd = true;
}

} // end namespace itk

#endif
