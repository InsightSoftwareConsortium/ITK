/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRandomNonRepeatingConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRandomNonRepeatingConstIteratorWithIndex_txx
#define _itkImageRandomNonRepeatingConstIteratorWithIndex_txx

#include "itkImageRandomNonRepeatingConstIteratorWithIndex.h"
#include "vnl/vnl_sample.h"

namespace itk
{


/** Default constructor. Needed since we provide a cast constructor. */
template<class TImage>
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::ImageRandomNonRepeatingConstIteratorWithIndex() : ImageConstIteratorWithIndex<TImage>()
{
  m_NumberOfPixelsInRegion    = 0L;
  m_NumberOfSamplesRequested  = 0L;
  m_NumberOfSamplesDone       = 0L;
  m_Permutation=NULL;
}

/** Constructor establishes an iterator to walk a particular image and a
 * particular region of that image. */
template<class TImage>
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::ImageRandomNonRepeatingConstIteratorWithIndex(const ImageType *ptr, const RegionType& region)
  : ImageConstIteratorWithIndex<TImage>( ptr, region )
{
  m_NumberOfPixelsInRegion   = region.GetNumberOfPixels();
  m_NumberOfSamplesRequested = 0L;
  m_NumberOfSamplesDone      = 0L;
  m_Permutation=new randomPermutation(m_NumberOfPixelsInRegion);
}

/**  Set the number of samples to extract from the region */
template<class TImage>
void
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::SetNumberOfSamples( unsigned long number )
{
  m_NumberOfSamplesRequested = number;
  if(number>m_NumberOfPixelsInRegion) m_NumberOfSamplesRequested=m_NumberOfPixelsInRegion;
}

/**  Set the number of samples to extract from the region */
template<class TImage>
unsigned long
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::GetNumberOfSamples( void ) const
{
  return m_NumberOfSamplesRequested;
}

/** Reinitialize the seed of the random number generator */
template<class TImage>
void
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::ReinitializeSeed()
{
  this->m_Permutation->ReinitializeSeed();
  this->m_Permutation->Shuffle();
}

template<class TImage>
void
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::ReinitializeSeed(int seed)
{
  this->m_Permutation->ReinitializeSeed(seed);
  this->m_Permutation->Shuffle();
}

/** update the position */
template<class TImage>
void
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::SetPriority(const itk::Image<unsigned long,ImageType::ImageDimension> * priorityImage) {
  // should probably do error checking to be sure that the priority
  // image is the right size
  IndexType positionIndex;
  unsigned long position,pixel;
  unsigned long residual;
  for(pixel=0;pixel<m_NumberOfPixelsInRegion;pixel++) {
    position=pixel;
    for( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
      {
  const unsigned long sizeInThisDimension = this->m_Region.GetSize()[dim];
  residual = position % sizeInThisDimension;
  positionIndex[dim] =  residual + this->m_BeginIndex[dim];
  position -= residual;
  position /= sizeInThisDimension;
      }
    //std::cout<<pixel<<" "<<positionIndex<<" "<<priorityImage->GetPixel(positionIndex)std::endl;
    this->m_Permutation->SetPriority(pixel,priorityImage->GetPixel(positionIndex));
  }
  this->m_Permutation->Shuffle();
}

/** update the position */
template<class TImage>
void
ImageRandomNonRepeatingConstIteratorWithIndex<TImage>
::UpdatePosition()
{
  unsigned long position = (*(this->m_Permutation))[m_NumberOfSamplesDone%m_NumberOfSamplesRequested];
  unsigned long residual;
  for( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
    {
    const unsigned long sizeInThisDimension = this->m_Region.GetSize()[dim];
    residual = position % sizeInThisDimension;
    this->m_PositionIndex[dim] =  residual + this->m_BeginIndex[dim];
    position -= residual;
    position /= sizeInThisDimension;
    }

  this->m_Position = this->m_Image->GetBufferPointer() + this->m_Image->ComputeOffset( this->m_PositionIndex );
}


} // end namespace itk

#endif
