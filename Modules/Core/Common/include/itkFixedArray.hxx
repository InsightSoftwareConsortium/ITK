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
#ifndef itkFixedArray_hxx
#define itkFixedArray_hxx

#include "itkNumericTraitsFixedArrayPixel.h"

namespace itk
{

template <typename TValue, unsigned int VLength>
FixedArray<TValue, VLength>::FixedArray(const ValueType & r)
{
  std::fill_n(m_InternalArray, VLength, r);
}

template <typename TValue, unsigned int VLength>
FixedArray<TValue, VLength>::FixedArray(const ValueType r[VLength])
{
  std::copy_n(r, VLength, m_InternalArray);
}

template <typename TValue, unsigned int VLength>
FixedArray<TValue, VLength> &
FixedArray<TValue, VLength>::operator=(const ValueType r[VLength])
{
  if (r != m_InternalArray)
  {
    std::copy_n(r, VLength, m_InternalArray);
  }
  return *this;
}

template <typename TValue, unsigned int VLength>
bool
FixedArray<TValue, VLength>::operator==(const FixedArray & r) const
{
  return std::equal(m_InternalArray, m_InternalArray + VLength, r.m_InternalArray);
}

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::Begin() -> Iterator
{
  return Iterator(m_InternalArray);
}

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::Begin() const -> ConstIterator
{
  return ConstIterator(m_InternalArray);
}

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::End() -> Iterator
{
  return Iterator(m_InternalArray + VLength);
}

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::End() const -> ConstIterator
{
  return ConstIterator(m_InternalArray + VLength);
}

#if !defined(ITK_LEGACY_REMOVE)

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::rBegin() -> ReverseIterator
{
  return ReverseIterator(m_InternalArray + VLength);
}

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::rBegin() const -> ConstReverseIterator
{
  return ConstReverseIterator(m_InternalArray + VLength);
}

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::rEnd() -> ReverseIterator
{
  return ReverseIterator(m_InternalArray);
}

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::rEnd() const -> ConstReverseIterator
{
  return ConstReverseIterator(m_InternalArray);
}

#endif // defined ( ITK_LEGACY_REMOVE )

template <typename TValue, unsigned int VLength>
auto
FixedArray<TValue, VLength>::Size() const -> SizeType
{
  return VLength;
}

template <typename TValue, unsigned int VLength>
void
FixedArray<TValue, VLength>::Fill(const ValueType & value)
{
  std::fill_n(m_InternalArray, VLength, value);
}

template <typename TValue, unsigned int VLength>
std::ostream &
operator<<(std::ostream & os, const FixedArray<TValue, VLength> & arr)
{
  os << '[';
  if (VLength == 1)
  {
    os << arr[0];
  }
  else
  {
    for (int i = 0; i < static_cast<int>(VLength) - 1; ++i)
    {
      os << arr[i] << ", ";
    }
    os << arr[VLength - 1];
  }
  os << ']';
  return os;
}
} // namespace itk

#endif
