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
#include "itkImageRegionConstIterator.h"

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
  m_StartIndices.push_back ( startIndex );

  // Set up the temporary image
  this->InitializeIterator();
}

template<class TImage, class TFunction>
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::FloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                              FunctionType *fnPtr,
                                              std::vector<IndexType>& startIndex)
{
  m_Image = imagePtr;
  m_Function = fnPtr;
  unsigned int i;
  for (i = 0; i < startIndex.size(); i++ )
    {
    m_StartIndices.push_back ( startIndex[i] );
    }

  // Set up the temporary image
  this->InitializeIterator();
}

template<class TImage, class TFunction>
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::FloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                              FunctionType *fnPtr)
{
  m_Image = imagePtr;
  m_Function = fnPtr;

  // Set up the temporary image
  this->InitializeIterator();
}

template<class TImage, class TFunction>
void
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::InitializeIterator()
{
  // Get the origin and spacing from the image in simple arrays
  m_ImageOrigin = m_Image->GetOrigin();
  m_ImageSpacing = m_Image->GetSpacing();
  m_ImageSize = m_Image->GetLargestPossibleRegion().GetSize().m_Size;

  // Build a temporary image of chars for use in the flood algorithm
  {
  SmartPointer<  TImage > image = const_cast< TImage * >( m_Image.GetPointer() );
  image->SetRequestedRegionToLargestPossibleRegion();
  }
  SizeType imageSize = m_Image->GetLargestPossibleRegion().GetSize();
  tempPtr = TTempImage::New();
  typename TTempImage::RegionType tempRegion;
  tempRegion.SetSize( imageSize );
  tempPtr->SetLargestPossibleRegion( tempRegion );
  tempPtr->SetBufferedRegion( tempRegion );
  tempPtr->SetRequestedRegion( tempRegion );
  tempPtr->Allocate();
  tempPtr->FillBuffer(NumericTraits<ITK_TYPENAME TTempImage::PixelType>::Zero);

  // Initialize the queue by adding the start index assuming one of
  // the m_StartIndices is "inside" This might not be true, in which
  // case it's up to the programmer to specify a correct starting
  // position later (using FindSeedPixel).  Must make sure that the
  // seed is inside the buffer before touching pixels.
  m_IsAtEnd = true;
  for ( unsigned int i = 0; i < m_StartIndices.size(); i++ )
    {
    if ( m_Image->GetBufferedRegion().IsInside ( m_StartIndices[i] ) )
      {
      m_IndexStack.push(m_StartIndices[i]);
      m_IsAtEnd = false;
      }
    }
}

template<class TImage, class TFunction>
void
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::FindSeedPixel()
{
  // Create an iterator that will walk the input image
  typedef typename itk::ImageRegionConstIterator<TImage> IRIType;
  IRIType it = IRIType(m_Image, m_Image->GetLargestPossibleRegion() );
  
  // Now we search the input image for the first pixel which is inside
  // the function of interest
  m_StartIndices.clear();
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    if( this->IsPixelIncluded( it.GetIndex() ) )
      {
      m_StartIndices.push_back ( it.GetIndex() );

      // We need to reset the "beginning" now that we have a real seed
      this->GoToBegin();

      return;
      }
    }
}

template<class TImage, class TFunction>
void
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::FindSeedPixels()
{
  // Create an iterator that will walk the input image
  typedef typename itk::ImageRegionConstIterator<TImage> IRIType;
  IRIType it = IRIType(m_Image, m_Image->GetLargestPossibleRegion() );
  
  // Now we search the input image for the first pixel which is inside
  // the function of interest
  m_StartIndices.clear();
  bool found = false;
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    if( this->IsPixelIncluded( it.GetIndex() ) )
      {
      m_StartIndices.push_back ( it.GetIndex() );
      found = true;
      }
    }
  if ( found )
    {
    // We need to reset the "beginning" now that we have a real seed
    this->GoToBegin();
    }
}

template<class TImage, class TFunction>
void
FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
::DoFloodStep()
{
  // The index in the front of the queue should always be
  // valid and be inside since this is what the iterator
  // uses in the Set/Get methods. This is ensured by the
  // GoToBegin() method.
 

  // Take the index in the front of the queue  
  const IndexType & topIndex = m_IndexStack.front();
  
  // Iterate through all possible dimensions
  // NOTE: Replace this with a ShapeNeighborhoodIterator
  for(unsigned int i=0; i<NDimensions; i++)
    {
    // The j loop establishes either left or right neighbor (+-1)
    for(int j=-1; j<=1; j+=2)
      {
      IndexType tempIndex;
      m_IsValidIndex = true;

      // build the index of a neighbor
      for(unsigned int k=0; k<NDimensions; k++)
        {
        if( i!=k )
          {
          tempIndex.m_Index[k] = topIndex[k];
          }
        else
          {
          tempIndex.m_Index[k] = topIndex[k] + j;
          if( (tempIndex.m_Index[k] < 0) || 
              (tempIndex.m_Index[k] >= static_cast<long int>(m_ImageSize[k])) )
            {
            m_IsValidIndex = false;
            continue;
            }
          }
        } // end build the index of a neighbor

      // If this is a valid index and have not been tested,
      // then test it.
      if( m_IsValidIndex && tempPtr->GetPixel( tempIndex )==0 )
        {
        // if it is inside, push it into the queue  
        if(  this->IsPixelIncluded( tempIndex ) )
          {
          m_IndexStack.push( tempIndex );
          tempPtr->SetPixel( tempIndex, 2); 
          }
        else  // If the pixel is outside
          {
          // Mark the pixel as outside and remove it from the queue.
          tempPtr->SetPixel( tempIndex, 1);
          }
        }
      } // end left/right neighbor loop
    } // end check all neighbors
  
  // Now that all the potential neighbors have been 
  // inserted we can get rid of the pixel in the front
  m_IndexStack.pop();
    
  if( m_IndexStack.empty() )
    {
    m_IsAtEnd = true;
    }


}


} // end namespace itk

#endif
