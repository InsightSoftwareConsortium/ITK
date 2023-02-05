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
#ifndef itkRegionGrowImageFilter_hxx
#define itkRegionGrowImageFilter_hxx


namespace itk
{
template <typename TInputImage, typename TOutputImage>
RegionGrowImageFilter<TInputImage, TOutputImage>::RegionGrowImageFilter()
{
  m_GridSize.Fill(2);
  m_MaximumNumberOfRegions = 0;
}

template <typename TInputImage, typename TOutputImage>
void
RegionGrowImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MaximumNumberOfRegions: " << m_MaximumNumberOfRegions << std::endl;
  os << indent << "GridSize: " << static_cast<typename NumericTraits<GridSizeType>::PrintType>(m_GridSize) << std::endl;
}
} // namespace itk

#endif
