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
#ifndef itkShapedFloodFilledFunctionConditionalConstIterator_hxx
#define itkShapedFloodFilledFunctionConditionalConstIterator_hxx

#include "itkShapedFloodFilledFunctionConditionalConstIterator.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
template< typename TImage, typename TFunction >
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::ShapedFloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                                    FunctionType *fnPtr,
                                                    IndexType startIndex):
  m_FullyConnected(false)
{
  this->m_Image = imagePtr;
  m_Function = fnPtr;
  m_Seeds.push_back (startIndex);

  // Set up the temporary image
  this->InitializeIterator();
}

template< typename TImage, typename TFunction >
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::ShapedFloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                                    FunctionType *fnPtr,
                                                    std::vector< IndexType > & startIndex):
  m_Function(fnPtr),
  m_FullyConnected(false)
{
  this->m_Image = imagePtr; // can not be done in the initialization list

  unsigned int i;
  for ( i = 0; i < startIndex.size(); i++ )
    {
    m_Seeds.push_back (startIndex[i]);
    }

  // Set up the temporary image
  this->InitializeIterator();
}

template< typename TImage, typename TFunction >
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::ShapedFloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                                    FunctionType *fnPtr):
  m_FullyConnected(false)
{
  this->m_Image = imagePtr;
  m_Function = fnPtr;

  // Set up the temporary image
  this->InitializeIterator();
}

template< typename TImage, typename TFunction >
void
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::InitializeIterator()
{
  // Get the origin and spacing from the image in simple arrays
  m_ImageOrigin  = this->m_Image->GetOrigin();
  m_ImageSpacing = this->m_Image->GetSpacing();
  m_ImageRegion  = this->m_Image->GetBufferedRegion();

  // Build and setup the neighborhood iterator
  typename NeighborhoodIteratorType::RadiusType radius; radius.Fill(1);

  NeighborhoodIteratorType tmp_iter(radius, this->m_Image, m_ImageRegion);
  m_NeighborhoodIterator = tmp_iter;

  setConnectivity(&m_NeighborhoodIterator, m_FullyConnected);

  // Build a temporary image of chars for use in the flood algorithm
  m_TempPtr = TTempImage::New();
  typename TTempImage::RegionType tempRegion = this->m_Image->GetBufferedRegion();

  m_TempPtr->SetLargestPossibleRegion(tempRegion);
  m_TempPtr->SetBufferedRegion(tempRegion);
  m_TempPtr->SetRequestedRegion(tempRegion);
  m_TempPtr->Allocate(true); // initialize buffer to zero

  // Initialize the queue by adding the start index assuming one of
  // the m_Seeds is "inside" This might not be true, in which
  // case it's up to the programmer to specify a correct starting
  // position later (using FindSeedPixel).  Must make sure that the
  // seed is inside the buffer before touching pixels.
  this->m_IsAtEnd = true;
  for ( unsigned int i = 0; i < m_Seeds.size(); i++ )
    {
    if ( m_ImageRegion.IsInside (m_Seeds[i]) )
      {
      m_IndexStack.push(m_Seeds[i]);
      this->m_IsAtEnd = false;
      }
    }
}

template< typename TImage, typename TFunction >
void
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::FindSeedPixel()
{
  // Create an iterator that will walk the input image
  typedef typename itk::ImageRegionConstIterator< TImage > IRIType;
  IRIType it = IRIType( this->m_Image, this->m_Image->GetBufferedRegion() );

  // Now we search the input image for the first pixel which is inside
  // the function of interest
  m_Seeds.clear();
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    if ( this->IsPixelIncluded( it.GetIndex() ) )
      {
      m_Seeds.push_back ( it.GetIndex() );

      // We need to reset the "beginning" now that we have a real seed
      this->GoToBegin();

      return;
      }
    }
}

template< typename TImage, typename TFunction >
void
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::FindSeedPixels()
{
  // Create an iterator that will walk the input image
  typedef typename itk::ImageRegionConstIterator< TImage > IRIType;
  IRIType it = IRIType( this->m_Image, this->m_Image->GetBufferedRegion() );

  // Now we search the input image for the first pixel which is inside
  // the function of interest
  m_Seeds.clear();
  bool found = false;
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    if ( this->IsPixelIncluded( it.GetIndex() ) )
      {
      m_Seeds.push_back ( it.GetIndex() );
      found = true;
      }
    }
  if ( found )
    {
    // We need to reset the "beginning" now that we have a real seed
    this->GoToBegin();
    }
}

template< typename TImage, typename TFunction >
void
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::DoFloodStep()
{
  // The index in the front of the queue should always be
  // valid and be inside since this is what the iterator
  // uses in the Set/Get methods. This is ensured by the
  // GoToBegin() method.

  // Take the index in the front of the queue
  const IndexType & topIndex = m_IndexStack.front();

  // We are explicitly not calling set location since only offsets of
  // the neighborhood iterator are accessed.
  typename NeighborhoodIteratorType::ConstIterator neighborIt =
    m_NeighborhoodIterator.Begin();
  const typename NeighborhoodIteratorType::ConstIterator neighborEnd =
    m_NeighborhoodIterator.End();

  for (; neighborIt != neighborEnd; ++neighborIt )
    {
    const OffsetType & offset = neighborIt.GetNeighborhoodOffset();
    const IndexType    tempIndex = topIndex + offset;

    // If this is a valid index and have not been tested,
    // then test it.
    if ( m_ImageRegion.IsInside(tempIndex) )
      {
      if ( m_TempPtr->GetPixel(tempIndex) == 0 )
        {
        // if it is inside, push it into the queue
        if ( this->IsPixelIncluded(tempIndex) )
          {
          m_IndexStack.push(tempIndex);
          m_TempPtr->SetPixel(tempIndex, 2);
          }
        else  // If the pixel is outside
          {
          // Mark the pixel as outside and remove it from the queue.
          m_TempPtr->SetPixel(tempIndex, 1);
          }
        }
      }
    } // Finished traversing neighbors

  // Now that all the potential neighbors have been
  // inserted we can get rid of the pixel in the front
  m_IndexStack.pop();

  if ( m_IndexStack.empty() )
    {
    this->m_IsAtEnd = true;
    }
}

template< typename TImage, typename TFunction >
void
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::SetFullyConnected(const bool _arg)
{
  if ( this->m_FullyConnected != _arg )
    {
    this->m_FullyConnected = _arg;
    setConnectivity(&m_NeighborhoodIterator, m_FullyConnected);
    }
}

template< typename TImage, typename TFunction >
bool
ShapedFloodFilledFunctionConditionalConstIterator< TImage, TFunction >
::GetFullyConnected() const
{
  return this->m_FullyConnected;
}
} // end namespace itk

#endif
