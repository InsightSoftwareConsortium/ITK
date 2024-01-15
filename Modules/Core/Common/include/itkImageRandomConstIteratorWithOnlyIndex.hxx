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
#ifndef itkImageRandomConstIteratorWithOnlyIndex_hxx
#define itkImageRandomConstIteratorWithOnlyIndex_hxx


namespace itk
{

template <typename TImage>
ImageRandomConstIteratorWithOnlyIndex<TImage>::ImageRandomConstIteratorWithOnlyIndex(const ImageType *  ptr,
                                                                                     const RegionType & region)
  : ImageConstIteratorWithOnlyIndex<TImage>(ptr, region)
  , m_NumberOfPixelsInRegion{ region.GetNumberOfPixels() }
{}

template <typename TImage>
void
ImageRandomConstIteratorWithOnlyIndex<TImage>::ReinitializeSeed()
{
  m_Generator->SetSeed();
}

template <typename TImage>
void
ImageRandomConstIteratorWithOnlyIndex<TImage>::ReinitializeSeed(int seed)
{
  m_Generator->SetSeed(seed);
}

template <typename TImage>
void
ImageRandomConstIteratorWithOnlyIndex<TImage>::RandomJump()
{
  using PositionValueType = IndexValueType;

  auto position = static_cast<PositionValueType>(
    m_Generator->GetVariateWithOpenRange(static_cast<double>(m_NumberOfPixelsInRegion) - 0.5));

  const SizeType regionSize = this->m_Region.GetSize();

  for (unsigned int dim = 0; dim < TImage::ImageDimension; ++dim)
  {
    const SizeValueType     sizeInThisDimension = regionSize[dim];
    const PositionValueType residual = position % sizeInThisDimension;
    this->m_PositionIndex[dim] = residual + this->m_BeginIndex[dim];
    position -= residual;
    position /= sizeInThisDimension;
  }
}
} // end namespace itk

#endif
