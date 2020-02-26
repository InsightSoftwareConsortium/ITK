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
#ifndef itkImageToNeighborhoodSampleAdaptor_h
#define itkImageToNeighborhoodSampleAdaptor_h

#include <typeinfo>
#include <vector>
#include <iostream>

#include "itkImage.h"
#include "itkListSample.h"
#include "itkSmartPointer.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMacro.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"

namespace itk
{
namespace Statistics
{

/**
 *\class ImageToNeighborhoodSampleAdaptor
 *  \brief This class provides ListSample interface to ITK Image
 *
 * After calling SetImage( const Image * ) method to plug in the image object,
 * users can use Sample interfaces to access Image neighborhoods. The resulting data
 * are a list of measurement vectors where each measurement vector has one element,
 * an itkConstNeighborhoodIterator.
 *
 * This class handles images with scalar, fixed array or variable length vector pixel types.
 *
 *
 * \sa Sample, ListSample, Neighborhood
 * \ingroup ITKStatistics
 */

template <typename TImage, typename TBoundaryCondition>
class ITK_TEMPLATE_EXPORT ImageToNeighborhoodSampleAdaptor
  : public ListSample<std::vector<ConstNeighborhoodIterator<TImage, TBoundaryCondition>>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToNeighborhoodSampleAdaptor);

  /** Standard class type aliases */
  using Self = ImageToNeighborhoodSampleAdaptor;

  using Superclass = ListSample<std::vector<ConstNeighborhoodIterator<TImage, TBoundaryCondition>>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToNeighborhoodSampleAdaptor, ListSample);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Image type alias */
  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using IndexType = typename ImageType::IndexType;
  using OffsetType = typename ImageType::OffsetType;
  using OffsetValueType = typename ImageType::OffsetValueType;
  using PixelType = typename ImageType::PixelType;
  using PixelContainerConstPointer = typename ImageType::PixelContainerConstPointer;
  using RegionType = typename ImageType::RegionType;
  using OffsetTableType = typename RegionType::OffsetTableType;
  using SizeType = typename ImageType::SizeType;
  using ImageIteratorType = ImageRegionIteratorWithIndex<TImage>;
  /** Neighborhood Iterator type alias support */
  using NeighborhoodIteratorType = ConstNeighborhoodIterator<TImage, TBoundaryCondition>;
  using NonConstNeighborhoodIteratorType = NeighborhoodIterator<TImage, TBoundaryCondition>;
  using NeighborhoodType = typename NeighborhoodIteratorType::NeighborhoodType;
  using NeighborhoodRadiusType = typename NeighborhoodIteratorType::RadiusType;
  using NeighborhoodIndexType = typename NeighborhoodIteratorType::IndexType;
  using NeighborhoodSizeType = typename NeighborhoodIteratorType::SizeType;

  /** Superclass type alias for Measurement vector, measurement,
   * Instance Identifier, frequency, size, size element value */
  using MeasurementVectorType = typename std::vector<ConstNeighborhoodIterator<TImage, TBoundaryCondition>>;
  using ValueType = typename MeasurementVectorType::value_type;
  using MeasurementType = ValueType;

  using AbsoluteFrequencyType = typename Superclass::AbsoluteFrequencyType;
  using TotalAbsoluteFrequencyType = typename Superclass::TotalAbsoluteFrequencyType;
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;
  using InstanceIdentifier = typename Superclass::InstanceIdentifier;

  /** Method to set the image */
  void
  SetImage(const TImage * image);

  /** Method to get the image */
  const TImage *
  GetImage() const;

  /** Method to set the radius */
  void
  SetRadius(const NeighborhoodRadiusType & radius);

  /** Method to get the radius */
  NeighborhoodRadiusType
  GetRadius() const;

  /** Method to set the region */
  void
  SetRegion(const RegionType & region);

  /** Method to get the region */
  RegionType
  GetRegion() const;

  void
  SetUseImageRegion(const bool & flag);

  /** Method to get UseImageRegion flag */
  itkGetConstMacro(UseImageRegion, bool);

  /** Convenience methods to turn on/off the UseImageRegion flag */
  itkBooleanMacro(UseImageRegion);


  /** returns the number of measurement vectors in this container */
  InstanceIdentifier
  Size() const override;

  /** method to return measurement vector for a specified id */
  const MeasurementVectorType &
  GetMeasurementVector(InstanceIdentifier id) const override;

  /** method to return frequency for a specified id */
  AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier id) const override;

  /** method to return the total frequency */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const override;

  /**
   *\class ConstIterator
   *  \brief Const Iterator
   *  \ingroup ITKStatistics
   */
  class ConstIterator
  {
    friend class ImageToNeighborhoodSampleAdaptor;

  public:
    ConstIterator() = delete;

    ConstIterator(const ImageToNeighborhoodSampleAdaptor * adaptor) { *this = adaptor->Begin(); }

    ConstIterator(const ConstIterator & iter)
    {
      m_MeasurementVectorCache = iter.m_MeasurementVectorCache;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
    }

    ConstIterator &
    operator=(const ConstIterator & iter)
    {
      m_MeasurementVectorCache = iter.m_MeasurementVectorCache;
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
      return this->m_MeasurementVectorCache;
    }

    InstanceIdentifier
    GetInstanceIdentifier() const
    {
      return m_InstanceIdentifier;
    }

    ConstIterator &
    operator++()
    {
      ++(m_MeasurementVectorCache[0]);
      ++m_InstanceIdentifier;
      return *this;
    }

    bool
    operator!=(const ConstIterator & it)
    {
      return (m_MeasurementVectorCache[0] != it.m_MeasurementVectorCache[0]);
    }

    bool
    operator==(const ConstIterator & it)
    {
      return (m_MeasurementVectorCache[0] == it.m_MeasurementVectorCache[0]);
    }

  protected:
    // This method should only be available to the ListSample class
    ConstIterator(NeighborhoodIteratorType iter, InstanceIdentifier iid)
    {
      this->m_MeasurementVectorCache.clear();
      this->m_MeasurementVectorCache.push_back(iter);
      m_InstanceIdentifier = iid;
    }

  private:
    mutable MeasurementVectorType m_MeasurementVectorCache;
    InstanceIdentifier            m_InstanceIdentifier;
  };

  /**
   *\class Iterator
   *  \brief Iterator
   *  \ingroup ITKStatistics
   */
  class Iterator : public ConstIterator
  {

    friend class ImageToNeighborhoodSampleAdaptor;

  public:
    Iterator() = delete;
    Iterator(const Self * adaptor) = delete;
    Iterator(const ConstIterator & it) = delete;
    ConstIterator &
    operator=(const ConstIterator & it) = delete;

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

  protected:
    // This copy constructor is actually used in Iterator Begin()!
    Iterator(NeighborhoodIteratorType iter, InstanceIdentifier iid)
      : ConstIterator(iter, iid)
    {}
  };

  /** returns an iterator that points to the beginning of the container */
  Iterator
  Begin()
  {
    NeighborhoodIteratorType nIterator(m_Radius, m_Image, m_Region);
    nIterator.GoToBegin();
    Iterator iter(nIterator, 0);
    return iter;
  }

  /** returns an iterator that points to the end of the container */
  Iterator
  End()
  {
    NeighborhoodIteratorType nIterator(m_Radius, m_Image, m_Region);
    nIterator.GoToEnd();
    Iterator iter(nIterator, m_Region.GetNumberOfPixels());
    return iter;
  }


  /** returns an iterator that points to the beginning of the container */
  ConstIterator
  Begin() const
  {
    NeighborhoodIteratorType nIterator(m_Radius, m_Image, m_Region);
    nIterator.GoToBegin();
    ConstIterator iter(nIterator, 0);
    return iter;
  }

  /** returns an iterator that points to the end of the container */
  ConstIterator
  End() const
  {
    NeighborhoodIteratorType nIterator(m_Radius, m_Image, m_Region);
    nIterator.GoToEnd();
    ConstIterator iter(nIterator, m_Region.GetNumberOfPixels());
    return iter;
  }

protected:
  ImageToNeighborhoodSampleAdaptor();
  ~ImageToNeighborhoodSampleAdaptor() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ImageConstPointer             m_Image;
  mutable MeasurementVectorType m_MeasurementVectorInternal;
  mutable InstanceIdentifier    m_InstanceIdentifierInternal;
  mutable IndexType             m_NeighborIndexInternal;
  NeighborhoodRadiusType        m_Radius;
  RegionType                    m_Region;
  bool                          m_UseImageRegion{ true };
  OffsetTableType               m_OffsetTable;

}; // end of class ImageToNeighborhoodSampleAdaptor

} // end of namespace Statistics

template <typename TImage, typename TBoundaryCondition>
std::ostream &
operator<<(std::ostream & os, const std::vector<itk::ConstNeighborhoodIterator<TImage, TBoundaryCondition>> & mv);

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToNeighborhoodSampleAdaptor.hxx"
#endif

#endif
