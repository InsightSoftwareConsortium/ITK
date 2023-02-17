/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageRandomConstIteratorWithIndex_hxx
#define itkImageRandomConstIteratorWithIndex_hxx


namespace itk
{

template <typename TImage>
ImageRandomConstIteratorWithIndex<TImage>::ImageRandomConstIteratorWithIndex(const ImageType *  ptr,
                                                                             const RegionType & region)
  : ImageConstIteratorWithIndex<TImage>(ptr, region)
{
  m_NumberOfPixelsInRegion = region.GetNumberOfPixels();
  m_NumberOfSamplesRequested = 0L;
  m_NumberOfSamplesDone = 0L;
  m_Generator = Statistics::MersenneTwisterRandomVariateGenerator::New();
}

template <typename TImage>
void
ImageRandomConstIteratorWithIndex<TImage>::SetNumberOfSamples(SizeValueType number)
{
  m_NumberOfSamplesRequested = number;
}

template <typename TImage>
auto
ImageRandomConstIteratorWithIndex<TImage>::GetNumberOfSamples() const -> SizeValueType
{
  return m_NumberOfSamplesRequested;
}

template <typename TImage>
void
ImageRandomConstIteratorWithIndex<TImage>::ReinitializeSeed()
{
  m_Generator->SetSeed();
}

template <typename TImage>
void
ImageRandomConstIteratorWithIndex<TImage>::ReinitializeSeed(int seed)
{
  m_Generator->SetSeed(seed);
  // vnl_sample_reseed(seed);
}

template <typename TImage>
void
ImageRandomConstIteratorWithIndex<TImage>::RandomJump()
{
  using PositionValueType = IndexValueType;

  const PositionValueType randomPosition = static_cast<PositionValueType>(
    m_Generator->GetVariateWithOpenRange(static_cast<double>(m_NumberOfPixelsInRegion) - 0.5));
  /*
      vnl_sample_uniform(0.0f,
      static_cast<double>(m_NumberOfPixelsInRegion)-0.5) );
  */

  PositionValueType position = randomPosition;
  PositionValueType residual;

  for (unsigned int dim = 0; dim < TImage::ImageDimension; ++dim)
  {
    const SizeValueType sizeInThisDimension = this->m_Region.GetSize()[dim];
    residual = position % sizeInThisDimension;
    this->m_PositionIndex[dim] = residual + this->m_BeginIndex[dim];
    position -= residual;
    position /= sizeInThisDimension;
  }

  this->m_Position = this->m_Image->GetBufferPointer() + this->m_Image->ComputeOffset(this->m_PositionIndex);
}
} // end namespace itk

#endif
