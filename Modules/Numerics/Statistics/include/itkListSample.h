/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkListSample_h
#define itkListSample_h

#include "itkObjectFactory.h"
#include "itkFixedArray.h"
#include "itkSample.h"

#include <vector>

namespace itk
{
namespace Statistics
{
/** \class ListSample
 *  \brief This class is the native implementation of the a Sample with an STL container
 *
 * ListSample stores measurements in a list type structure (as opposed to a
 * Histogram, etc.).  ListSample allows duplicate measurements. ListSample is
 * not sorted.
 *
 * ListSample does not allow the user to specify the frequency of
 * a measurement directly.  The GetFrequency() methods returns 1 if
 * the measurement exists in the list, 0 otherwise.
 *
 * \sa Sample, Histogram
 * \ingroup ITKStatistics
 *
 * \sphinx
 * \sphinxexample{Numerics/Statistics/CreateListOfSampleMeasurements,Create List Of Sample Measurements}
 * \endsphinx
 */

template <typename TMeasurementVector>
class ITK_TEMPLATE_EXPORT ListSample : public Sample<TMeasurementVector>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ListSample);

  /** Standard class type alias. */
  using Self = ListSample;
  using Superclass = Sample<TMeasurementVector>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(ListSample, Sample);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedefs inherited from the superclass */
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;
  using MeasurementType = typename Superclass::MeasurementType;
  using AbsoluteFrequencyType = typename Superclass::AbsoluteFrequencyType;
  using TotalAbsoluteFrequencyType = typename Superclass::TotalAbsoluteFrequencyType;
  using InstanceIdentifier = typename Superclass::InstanceIdentifier;

  /** Value type of a measurement (component of the measurement
   * vector) */
  using ValueType = MeasurementVectorType;

  /** internal data container type */
  using InternalDataContainerType = std::vector<MeasurementVectorType>;

  /** Resize the container. Using Resize() and then SetMeasurementVector() is
   * about nine times faster than usign PushBack() continuously. Which means that
   * whenever the total number of Measurement vectors is known, the users
   * should prefer calling Resize() first and then set the values by calling
   * SetMeasurementVector(). On the other hand, if the number of measurement
   * vectors is not known from the beginning, then calling PushBack()
   * sequentially is a convenient option. */
  void
  Resize(InstanceIdentifier newsize);

  /** Removes all the elements in the Sample */
  void
  Clear();

  /** Inserts a measurement at the end of the list */
  void
  PushBack(const MeasurementVectorType & mv);

  /** Get the number of measurement vectors in the sample */
  InstanceIdentifier
  Size() const override;

  /** Get the measurement associated with the specified
   * InstanceIdentifier */
  const MeasurementVectorType &
  GetMeasurementVector(InstanceIdentifier instanceId) const override;

  /** Set a component a measurement to a particular value. */
  void
  SetMeasurement(InstanceIdentifier instanceId, unsigned int dim, const MeasurementType & value);

  /** Replace a measurement with a different measurement */
  void
  SetMeasurementVector(InstanceIdentifier instanceId, const MeasurementVectorType & mv);

  /** Get the frequency of a measurement. Returns 1 if the measurement
   * exist. */
  AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier instanceId) const override;

  /** Get the total frequency of the sample.  This is equivalent to
   * the size of the sample. */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const override;

  /** Method to graft another sample */
  void
  Graft(const DataObject * thatObject) override;

  /** \class ConstIterator
   * \brief Const Iterator
   * \ingroup ITKStatistics
   */
  class ConstIterator
  {
    friend class ListSample;

  public:
    ConstIterator(const ListSample * sample) { *this = sample->Begin(); }

    ConstIterator(const ConstIterator & iter)
    {
      m_Iter = iter.m_Iter;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
    }

    ConstIterator() = delete;

    ConstIterator &
    operator=(const ConstIterator & iter)
    {
      m_Iter = iter.m_Iter;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
      return *this;
    }

    AbsoluteFrequencyType
    GetFrequency() const
    {
      return 1;
    }

    const MeasurementVectorType &
    GetMeasurementVector() const
    {
      return static_cast<const MeasurementVectorType &>(*m_Iter);
    }

    InstanceIdentifier
    GetInstanceIdentifier() const
    {
      return m_InstanceIdentifier;
    }

    ConstIterator &
    operator++()
    {
      ++m_Iter;
      ++m_InstanceIdentifier;
      return *this;
    }

    bool
    operator!=(const ConstIterator & it) const
    {
      return (m_Iter != it.m_Iter);
    }

    bool
    operator==(const ConstIterator & it) const
    {
      return (m_Iter == it.m_Iter);
    }

  protected:
    // This method should only be available to the ListSample class
    ConstIterator(typename InternalDataContainerType::const_iterator iter, InstanceIdentifier iid)
    {
      m_Iter = iter;
      m_InstanceIdentifier = iid;
    }

  private:
    using InternalIterator = typename InternalDataContainerType::const_iterator;
    InternalIterator   m_Iter;
    InstanceIdentifier m_InstanceIdentifier;
  };

  /** \class Iterator
   * \brief Iterator
   * \ingroup ITKStatistics
   */
  class Iterator : public ConstIterator
  {
    friend class ListSample;

  public:
    Iterator() = delete;
    Iterator(const Self * sample) = delete;
    Iterator(typename InternalDataContainerType::const_iterator iter, InstanceIdentifier iid) = delete;
    Iterator(const ConstIterator & it) = delete;
    ConstIterator &
    operator=(const ConstIterator & it) = delete;

    Iterator(Self * sample)
      : ConstIterator(sample)
    {}

    Iterator(const Iterator & iter)
      : ConstIterator(iter)
    {}

    Iterator &
    operator=(const Iterator & iter)
    {
      this->ConstIterator::operator=(iter);
      return *this;
    }

  protected:
    Iterator(typename InternalDataContainerType::iterator iter, InstanceIdentifier iid)
      : ConstIterator(iter, iid)
    {}
  };

  /** returns an iterator that points to the beginning of the container */
  Iterator
  Begin()
  {
    Iterator iter(m_InternalContainer.begin(), 0);

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  Iterator
  End()
  {
    Iterator iter(m_InternalContainer.end(), static_cast<InstanceIdentifier>(m_InternalContainer.size()));

    return iter;
  }

  /** returns an iterator that points to the beginning of the container */
  ConstIterator
  Begin() const
  {
    ConstIterator iter(m_InternalContainer.begin(), 0);

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  ConstIterator
  End() const
  {
    ConstIterator iter(m_InternalContainer.end(), static_cast<InstanceIdentifier>(m_InternalContainer.size()));

    return iter;
  }

protected:
  ListSample() = default;
  ~ListSample() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  InternalDataContainerType m_InternalContainer;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkListSample.hxx"
#endif

#endif
