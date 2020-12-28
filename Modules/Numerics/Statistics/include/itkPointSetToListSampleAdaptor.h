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
#ifndef itkPointSetToListSampleAdaptor_h
#define itkPointSetToListSampleAdaptor_h

#include <typeinfo>

#include "itkPointSet.h"
#include "itkListSample.h"
#include "itkSmartPointer.h"

namespace itk
{
namespace Statistics
{
/**
 *\class PointSetToListSampleAdaptor
 *  \brief This class provides ListSample interface to ITK PointSet
 *
 * After calling SetPointSet(PointSet*) method to plug-in
 * the PointSet object, users can use Sample interfaces to access
 * PointSet data. This adaptor assumes that the PointsContainer is
 * actual storage for measurement vectors. In other words, PointSet's
 * dimension equals to the measurement vectors size. This class totally ignores
 * PointsDataContainer.
 *
 * \sa Sample, ListSample, PointSet
 * \ingroup ITKStatistics
 */

template <typename TPointSet>
class ITK_TEMPLATE_EXPORT PointSetToListSampleAdaptor : public ListSample<typename TPointSet::PointType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointSetToListSampleAdaptor);

  /** Standard class type aliases */
  using Self = PointSetToListSampleAdaptor;
  using Superclass = ListSample<typename TPointSet::PointType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToListSampleAdaptor, ListSample);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** PointSet type alias */
  using PointSetType = TPointSet;
  using PointSetPointer = typename TPointSet::Pointer;
  using PointSetConstPointer = typename TPointSet::ConstPointer;
  using PointsContainer = typename TPointSet::PointsContainer;
  using PointsContainerPointer = typename TPointSet::PointsContainerPointer;
  using PointsContainerConstPointer = typename TPointSet::PointsContainerConstPointer;
  using PointsContainerIteratorType = typename TPointSet::PointsContainerIterator;
  using PointsContainerConstIteratorType = typename TPointSet::PointsContainerConstIterator;
  using PointType = typename TPointSet::PointType;

  /** Superclass type alias for Measurement vector, measurement,
   * Instance Identifier, frequency, size, size element value */
  using MeasurementType = typename Superclass::MeasurementType;
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using AbsoluteFrequencyType = typename Superclass::AbsoluteFrequencyType;
  using TotalAbsoluteFrequencyType = typename Superclass::TotalAbsoluteFrequencyType;
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;
  using InstanceIdentifier = typename Superclass::InstanceIdentifier;

  using ValueType = MeasurementVectorType;

  /** Method to set the point set */
  void
  SetPointSet(const TPointSet * pointSet);

  /** Method to get the point set */
  const TPointSet *
  GetPointSet();

  /** returns the number of measurement vectors in this container */
  InstanceIdentifier
  Size() const override;

  /** returns the measurement vector that is specified by the instance
   * identifier argument. */
  const MeasurementVectorType &
  GetMeasurementVector(InstanceIdentifier identifier) const override;

  /** returns 1 as other subclasses of ListSampleBase does */
  AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier id) const override;

  /** returns the size of this container */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const override;

  /**
   *\class ConstIterator
   * \ingroup ITKStatistics
   */
  class ConstIterator
  {
    friend class PointSetToListSampleAdaptor;

  public:
    ConstIterator(const PointSetToListSampleAdaptor * adaptor) { *this = adaptor->Begin(); }

    ConstIterator(const ConstIterator & iter)
    {
      m_Iter = iter.m_Iter;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
    }

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
      return (const MeasurementVectorType &)m_Iter.Value();
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
    ConstIterator(PointsContainerConstIteratorType iter, InstanceIdentifier iid)
    {
      m_Iter = iter;
      m_InstanceIdentifier = iid;
    }

  private:
    ConstIterator() = delete;
    PointsContainerConstIteratorType m_Iter;
    InstanceIdentifier               m_InstanceIdentifier;
  };

  /**
   *\class Iterator
   * \ingroup ITKStatistics
   */
  class Iterator : public ConstIterator
  {
    friend class PointSetToListSampleAdaptor;

  public:
    Iterator(Self * adaptor)
      : ConstIterator(adaptor)
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

    Iterator() = delete;
    Iterator(const Self * adaptor) = delete;
    Iterator(PointsContainerConstIteratorType iter, InstanceIdentifier iid) = delete;
    Iterator(const ConstIterator & it) = delete;
    ConstIterator &
    operator=(const ConstIterator & it) = delete;

  protected:
    Iterator(PointsContainerIteratorType iter, InstanceIdentifier iid)
      : ConstIterator(iter, iid)
    {}
  };

  /** returns an iterator that points to the beginning of the container */
  Iterator
  Begin()
  {
    PointsContainerPointer nonConstPointsDataContainer = const_cast<PointsContainer *>(m_PointsContainer.GetPointer());
    Iterator               iter(nonConstPointsDataContainer->Begin(), 0);

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  Iterator
  End()
  {
    PointsContainerPointer nonConstPointsDataContainer = const_cast<PointsContainer *>(m_PointsContainer.GetPointer());

    Iterator iter(nonConstPointsDataContainer->End(), m_PointsContainer->Size());

    return iter;
  }

  /** returns an iterator that points to the beginning of the container */
  ConstIterator
  Begin() const
  {
    ConstIterator iter(m_PointsContainer->Begin(), 0);

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  ConstIterator
  End() const
  {
    ConstIterator iter(m_PointsContainer->End(), m_PointsContainer->Size());

    return iter;
  }

protected:
  PointSetToListSampleAdaptor();

  ~PointSetToListSampleAdaptor() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** the PointSet data source pointer */
  PointSetConstPointer m_PointSet;

  /** the points container which will be actually used for storing
   * measurement vectors */
  PointsContainerConstPointer m_PointsContainer;

  /** temporary points for conversions */
  mutable PointType m_TempPoint;
}; // end of class PointSetToListSampleAdaptor
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSetToListSampleAdaptor.hxx"
#endif

#endif
