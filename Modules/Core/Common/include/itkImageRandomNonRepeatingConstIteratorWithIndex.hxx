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
#ifndef itkImageRandomNonRepeatingConstIteratorWithIndex_hxx
#define itkImageRandomNonRepeatingConstIteratorWithIndex_hxx

#include "itkImageRandomNonRepeatingConstIteratorWithIndex.h"

namespace itk
{
/** Default constructor. Needed since we provide a cast constructor. */
template< typename TImage >
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::ImageRandomNonRepeatingConstIteratorWithIndex():ImageConstIteratorWithIndex< TImage >()
{
  m_NumberOfPixelsInRegion    = 0L;
  m_NumberOfSamplesRequested  = 0L;
  m_NumberOfSamplesDone       = 0L;
  m_Permutation = ITK_NULLPTR;
}

/** Constructor establishes an iterator to walk a particular image and a
 * particular region of that image. */
template< typename TImage >
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::ImageRandomNonRepeatingConstIteratorWithIndex(const ImageType *ptr, const RegionType & region):
  ImageConstIteratorWithIndex< TImage >(ptr, region)
{
  m_NumberOfPixelsInRegion   = region.GetNumberOfPixels();
  m_NumberOfSamplesRequested = 0L;
  m_NumberOfSamplesDone      = 0L;
  m_Permutation = new RandomPermutation(m_NumberOfPixelsInRegion);
}


//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template< typename TImage >
ImageRandomNonRepeatingConstIteratorWithIndex< TImage > &
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::operator=(const Self & it)
{
  if(this != &it)
    {
    this->ImageConstIteratorWithIndex< TImage >::operator=(it);
    if(m_Permutation)
      {
      *m_Permutation = *(it.m_Permutation);
      }
    else
      {
      m_NumberOfPixelsInRegion   = it.GetRegion().GetNumberOfPixels();
      m_NumberOfSamplesRequested = 0L;
      m_NumberOfSamplesDone      = 0L;
      m_Permutation = new RandomPermutation(m_NumberOfPixelsInRegion);
      }
    }
  return *this;
}

/**  Set the number of samples to extract from the region */
template< typename TImage >
void
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::SetNumberOfSamples(SizeValueType number)
{
  m_NumberOfSamplesRequested = number;
  if ( number > m_NumberOfPixelsInRegion ) { m_NumberOfSamplesRequested = m_NumberOfPixelsInRegion; }
}

/**  Set the number of samples to extract from the region */
template< typename TImage >
typename ImageRandomNonRepeatingConstIteratorWithIndex< TImage >::SizeValueType
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::GetNumberOfSamples(void) const
{
  return m_NumberOfSamplesRequested;
}

/** Reinitialize the seed of the random number generator */
template< typename TImage >
void
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::ReinitializeSeed()
{
  this->m_Permutation->ReinitializeSeed();
  this->m_Permutation->Shuffle();
}

template< typename TImage >
void
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::ReinitializeSeed(int seed)
{
  this->m_Permutation->ReinitializeSeed(seed);
  this->m_Permutation->Shuffle();
}

/** update the position */
template< typename TImage >
void
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::SetPriorityImage(const PriorityImageType *priorityImage)
{
  // should probably do error checking to be sure that the priority
  // image is the right size
  typedef SizeValueType PositionValueType;

  for (PositionValueType pixel = 0; pixel < m_NumberOfPixelsInRegion; pixel++ )
    {
    PositionValueType position = pixel;
    IndexType              positionIndex;
    for ( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
      {
      const SizeValueType sizeInThisDimension = this->m_Region.GetSize()[dim];
      const PositionValueType residual = position % sizeInThisDimension;
      positionIndex[dim] =  residual + this->m_BeginIndex[dim];
      position -= residual;
      position /= sizeInThisDimension;
      }
    //std::cout<<pixel<<" "<<positionIndex<<"
    // "<<priorityImage->GetPixel(positionIndex)std::endl;
    this->m_Permutation->SetPriority( pixel, priorityImage->GetPixel(positionIndex) );
    }
  this->m_Permutation->Shuffle();
}

/** update the position */
template< typename TImage >
void
ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
::UpdatePosition()
{
  typedef IndexValueType PositionValueType;

  PositionValueType position = ( *( this->m_Permutation ) )[m_NumberOfSamplesDone % m_NumberOfSamplesRequested];
  for ( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
    {
    const SizeValueType sizeInThisDimension = this->m_Region.GetSize()[dim];
    const PositionValueType residual = position % sizeInThisDimension;
    this->m_PositionIndex[dim] =  residual + this->m_BeginIndex[dim];
    position -= residual;
    position /= sizeInThisDimension;
    }

  this->m_Position = this->m_Image->GetBufferPointer() + this->m_Image->ComputeOffset(this->m_PositionIndex);
}
} // end namespace itk

#endif
