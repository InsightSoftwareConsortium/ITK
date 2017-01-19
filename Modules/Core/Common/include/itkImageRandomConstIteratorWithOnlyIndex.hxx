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
#ifndef itkImageRandomConstIteratorWithOnlyIndex_hxx
#define itkImageRandomConstIteratorWithOnlyIndex_hxx

#include "itkImageRandomConstIteratorWithOnlyIndex.h"

namespace itk
{
/** Default constructor. Needed since we provide a cast constructor. */
template< typename TImage >
ImageRandomConstIteratorWithOnlyIndex< TImage >
::ImageRandomConstIteratorWithOnlyIndex():ImageConstIteratorWithOnlyIndex< TImage >()
{
  m_NumberOfPixelsInRegion    = 0L;
  m_NumberOfSamplesRequested  = 0L;
  m_NumberOfSamplesDone       = 0L;
  m_Generator = Statistics::MersenneTwisterRandomVariateGenerator::New();
}

/** Constructor establishes an iterator to walk a particular image and a
 * particular region of that image. */
template< typename TImage >
ImageRandomConstIteratorWithOnlyIndex< TImage >
::ImageRandomConstIteratorWithOnlyIndex(const ImageType *ptr, const RegionType & region):
  ImageConstIteratorWithOnlyIndex< TImage >(ptr, region)
{
  m_NumberOfPixelsInRegion   = region.GetNumberOfPixels();
  m_NumberOfSamplesRequested = 0L;
  m_NumberOfSamplesDone      = 0L;
  m_Generator = Statistics::MersenneTwisterRandomVariateGenerator::New();
}

/**  Set the number of samples to extract from the region */
template< typename TImage >
void
ImageRandomConstIteratorWithOnlyIndex< TImage >
::SetNumberOfSamples(SizeValueType number)
{
  m_NumberOfSamplesRequested = number;
}

/**  Set the number of samples to extract from the region */
template< typename TImage >
typename ImageRandomConstIteratorWithOnlyIndex< TImage >::SizeValueType
ImageRandomConstIteratorWithOnlyIndex< TImage >
::GetNumberOfSamples(void) const
{
  return m_NumberOfSamplesRequested;
}

/** Reinitialize the seed of the random number generator */
template< typename TImage >
void
ImageRandomConstIteratorWithOnlyIndex< TImage >
::ReinitializeSeed()
{
  m_Generator->SetSeed();
}

template< typename TImage >
void
ImageRandomConstIteratorWithOnlyIndex< TImage >
::ReinitializeSeed(int seed)
{
  m_Generator->SetSeed (seed);
}

/** Execute an acrobatic random jump */
template< typename TImage >
void
ImageRandomConstIteratorWithOnlyIndex< TImage >
::RandomJump()
{
  typedef IndexValueType PositionValueType;

  const PositionValueType randomPosition = static_cast< PositionValueType >( m_Generator->GetVariateWithOpenRange (static_cast< double >( m_NumberOfPixelsInRegion ) - 0.5) );
  PositionValueType position = randomPosition;
  PositionValueType residual;

  for ( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
    {
    const SizeValueType sizeInThisDimension = this->m_Region.GetSize()[dim];
    residual = position % sizeInThisDimension;
    this->m_PositionIndex[dim] =  residual + this->m_BeginIndex[dim];
    position -= residual;
    position /= sizeInThisDimension;
    }

}
} // end namespace itk

#endif
