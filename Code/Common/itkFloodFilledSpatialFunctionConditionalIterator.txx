/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledSpatialFunctionConditionalIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkFloodFilledSpatialFunctionConditionalIterator_txx
#define _itkFloodFilledSpatialFunctionConditionalIterator_txx

#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

namespace itk
{
template<class TImage, class TFunction>
FloodFilledSpatialFunctionConditionalIterator<TImage, TFunction>
::FloodFilledSpatialFunctionConditionalIterator(ImageType *imagePtr,
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

  m_IsAtEnd = FALSE;
};

template<class TImage, class TFunction>
void
FloodFilledSpatialFunctionConditionalIterator<TImage, TFunction>
::DoFloodStep()
{
  m_FoundUncheckedNeighbor = FALSE;
  m_IsValidIndex = TRUE;

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
        tempPtr->GetPixel( m_IndexStack.top() ) = 2;
        // kick out of this function, since we found a new pixel
        return;
        }
      else
        {
        // pixel is not inside the function, mark it
        tempPtr->GetPixel( m_IndexStack.top() ) = 1;
        // pop off this pixel
        m_IndexStack.pop();
        continue;
        }
    } // end if we haven't visited the pixel

    // Now look at all of this pixel's neighbors
    // For an image in n-dimensions, there are 2n neighbors
    // We're only interested in the nearest neighbors along each axis
    m_FoundUncheckedNeighbor = FALSE;

    // i loop runs through all possible dimensions
    for(int i=0; i<NDimensions; i++)
      {
      IndexType tempIndex;
      m_IsValidIndex = TRUE;

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
            if( (tempIndex.m_Index[k] < 0) || (tempIndex.m_Index[k] >= m_ImageSize[k]) )
              {
              m_IsValidIndex = FALSE;
              continue;
              }
            }
          } // end build the index of a neighbor

        // If this isn't a valid index, loop to the next one
        if(m_IsValidIndex==FALSE)
          continue;

        // Now check the neighbor and see if it hasn't been examined yet
        if(tempPtr->GetPixel(tempIndex)==0) // if pixel hasn't been checked
          {
          // push it onto the stack, this is the next pixel we'll look at
          m_FoundUncheckedNeighbor=TRUE;
          m_IndexStack.push(tempIndex);
          continue;
          }

        } // end left/right neighbor loop

      // If we've found an unchecked neighbor, force its evaluation
      if(m_FoundUncheckedNeighbor==TRUE)
        continue;
      
    } // end check all neighbors

    // If we made it this far - i.e. checked all neighbors - and none of them
    // were unexamined, it's time to go back to the pixel we were at before
    if(m_FoundUncheckedNeighbor==FALSE)
      {
      // Mark the pixel as finished
      if( tempPtr->GetPixel( m_IndexStack.top() )==2 )
        tempPtr->GetPixel( m_IndexStack.top() )= 3;

      // Move the stack up
      m_IndexStack.pop();
      }

    } while(!(m_IndexStack.empty())); // loop while there are pixels left on the stack

  // if we made it this far, the stack is now empy
  m_IsAtEnd = TRUE;
}

template<class TImage, class TFunction>
bool
FloodFilledSpatialFunctionConditionalIterator<TImage, TFunction>
::IsPixelIncluded(IndexType index)
{
  PositionType position;

  for (int ii = 0; ii < NDimensions; ++ii)
    position[ii] = index[ii]*m_ImageSpacing[ii]+m_ImageOrigin[ii];

  return m_Function->Evaluate(position);
}

} // end namespace itk

#endif
