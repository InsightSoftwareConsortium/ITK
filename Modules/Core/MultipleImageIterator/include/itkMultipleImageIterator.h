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

#ifndef itkMultipleImageIterator_h
#define itkMultipleImageIterator_h
#include <vector>
#include <itkImageRegionIterator.h>


namespace itk
{
/** \class MultipleImageIterator
 * \brief An wrapper around image iterators to iterate over several images simultaneously
 * All iterators must
 *  - point to images of the same type
 *  - be of the same size (number of values from begin to end)
 * \ingroup MultipleImageIterator */
template <typename TIterator>
class MultipleImageIterator
{
public:
  using Self = MultipleImageIterator;
  using IteratorType = TIterator;
  using ImageType = typename IteratorType::ImageType;
  /// Access one of the iterators
  IteratorType &
  operator[](const int i)
  {
    return m_Iterators[i];
  }
  /// Add a new iterator
  void
  AddIterator(const IteratorType & it)
  {
    m_Iterators.push_back(it);
  }
  /// Advance all iterators
  Self &
  operator++()
  {
    for (auto it = m_Iterators.begin(); it != m_Iterators.end(); ++it)
    {
      ++(*it);
    }
    return *this;
  }
  /// Rewind all iterators
  void
  GoToBegin()
  {
    for (auto it = m_Iterators.begin(); it != m_Iterators.end(); ++it)
    {
      it->GoToBegin();
    }
  }
  /** Check if the first iterator is at end. In debug mode, additionally check
   * that at least one iterator is present and that all iterators' IsAtEnd()
   * methods return the same thing */
  bool
  IsAtEnd()
  {
#ifdef NDEBUG
    return m_Iterators[0].IsAtEnd();
#else
    assert(m_Iterators.size());
    bool retval = m_Iterators[0].IsAtEnd();
    for (unsigned int i = 0; i < m_Iterators.size(); ++i)
      assert(m_Iterators[i].IsAtEnd() == retval);
    return retval;
#endif
  }
  /// Returns the number of iterators
  unsigned int
  Size() const
  {
    return m_Iterators.size();
  }

protected:
  std::vector<IteratorType> m_Iterators;
};
} // namespace itk
#endif // itkMultipleImageIterator_h
