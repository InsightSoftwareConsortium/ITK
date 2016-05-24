#ifndef __itkMultipleImageIterator_h
#define __itkMultipleImageIterator_h
#include <vector>
#include <itkImageRegionIterator.h>


namespace itk
{
/** class MultipleImageIterator
 * \brief An wrapper around image iterators to iterate over several images simultaneously
 * All iterators must
 *  - point to images of the same type
 *  - be of the same size (number of values from begin to end) */
template <typename TIterator>
class MultipleImageIterator
{
public:
  typedef MultipleImageIterator            Self;
  typedef TIterator                        IteratorType;
  typedef typename IteratorType::ImageType ImageType;
  /// Access one of the iterators
  IteratorType &
  operator[](const int i)
  {
    return m_iterators[i];
  }
  /// Add a new iterator
  void
  AddIterator(const IteratorType & it)
  {
    m_iterators.push_back(it);
  }
  /// Advance all iterators
  Self &
  operator++()
  {
    for (typename std::vector<IteratorType>::iterator it = m_iterators.begin(); it != m_iterators.end(); ++it)
    {
      ++(*it);
    }
    return *this;
  }
  /// Rewind all iterators
  void
  GoToBegin()
  {
    for (typename std::vector<IteratorType>::iterator it = m_iterators.begin(); it != m_iterators.end(); ++it)
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
    return m_iterators[0].IsAtEnd();
#else
    assert(m_iterators.size());
    bool retval = m_iterators[0].IsAtEnd();
    for (unsigned int i = 0; i < m_iterators.size(); ++i)
      assert(m_iterators[i].IsAtEnd() == retval);
    return retval;
#endif
  }
  /// Returns the number of iterators
  unsigned int
  Size() const
  {
    return m_iterators.size();
  }

protected:
  std::vector<IteratorType> m_iterators;
};
} // namespace itk
#endif
