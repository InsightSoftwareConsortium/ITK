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
#ifndef itkSparseFieldLayer_hxx
#define itkSparseFieldLayer_hxx
#include <cmath>

namespace itk
{
template <typename TNodeType>
SparseFieldLayer<TNodeType>::SparseFieldLayer()
{
  m_HeadNode->Next = m_HeadNode.get();
  m_HeadNode->Previous = m_HeadNode.get();
  m_Size = 0;
}

template <typename TNodeType>
void
SparseFieldLayer<TNodeType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "HeadNode: " << m_HeadNode.get() << std::endl;
  os << indent << "Size: " << m_Size << std::endl;
}

template <typename TNodeType>
unsigned int
SparseFieldLayer<TNodeType>::Size() const
{
  return m_Size;
}

template <typename TNodeType>
auto
SparseFieldLayer<TNodeType>::SplitRegions(int num) const -> RegionListType
{
  const unsigned int size = Size();
  const auto    regionsize = static_cast<unsigned int>(std::ceil(static_cast<float>(size) / static_cast<float>(num)));
  ConstIterator position = Begin();
  const ConstIterator last = End();

  std::vector<RegionType> regionlist;
  regionlist.reserve(num);
  for (int i = 0; i < num; ++i)
  {
    unsigned int j = 0;
    RegionType   region;
    region.first = position;
    while ((j < regionsize) && (position != last))
    {
      ++j;
      ++position;
    }
    region.last = position;
    regionlist.push_back(region);
  }

  return regionlist;
}
} // end namespace itk

#endif
