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
#ifndef itkJointDomainImageToListSampleAdaptor_h
#define itkJointDomainImageToListSampleAdaptor_h

#include "itkPoint.h"
#include "itkPixelTraits.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkListSample.h"

namespace itk
{
namespace Statistics
{
/** \class ImageJointDomainTraits
 *  \brief This class provides the type definition for the measurement
 *  vector in the joint domain (range domain -- pixel values + spatial
 *  domain -- pixel's physical coordinates).
 *
 * \sa JointDomainImageToListSampleAdaptor
 * \ingroup ITKStatistics
 */
template <typename TImage>
struct ImageJointDomainTraits
{
  using Self = ImageJointDomainTraits;
  using PixelTraitsType = PixelTraits<typename TImage::PixelType>;
  using RangeDomainMeasurementType = typename PixelTraitsType::ValueType;

  static constexpr unsigned int ImageDimension = TImage::ImageDimension;
  static constexpr unsigned int Dimension = TImage::ImageDimension + PixelTraitsType::Dimension;

  using CoordinateRepType = float;
  using PointType = Point<CoordinateRepType, Self::ImageDimension>;
  using JoinTraitsType = JoinTraits<RangeDomainMeasurementType, CoordinateRepType>;
  using MeasurementType = typename JoinTraitsType::ValueType;

  using MeasurementVectorType = FixedArray<MeasurementType, Self::Dimension>;
}; // end of ImageJointDomainTraits

/** \class JointDomainImageToListSampleAdaptor
 *  \brief This adaptor returns measurement vectors composed of an
 *  image pixel's range domain value (pixel value) and spatial domain
 *  value (pixel's physical coordinates).
 *
 * This class is a derived class of the ListSample class. This class
 * overrides the GetMeasurementVector method. The GetMeasurementVector
 * returns a measurement vector that consist of a pixel's physical
 * coordinates and intensity value. For example, if the image
 * dimension is 3, and the pixel value is two component vector, the
 * measurement vector is a 5 component vector. The first three
 * component will be x, y, z physical coordinates (not index) and the
 * rest two component is the pixel values. The type of component is
 * float or which is determined by the ImageJointDomainTraits
 * class. When the pixel value type is double, the component value
 * type of a measurement vector is double. In other case, the
 * component value type is float because the physical coordinate value
 * type is float. Since the measurement vector is a composition of
 * spatial domain and range domain, for many statistical analysis, we
 * want to normalize the values from both domains. For this purpose,
 * there is the SetNormalizationFactors method. With the above example
 * (5 component measurement vector), you can specify a 5 component
 * normalization factor array. With such factors, the
 * GetMeasurementVector method returns a measurement vector whose each
 * component is divided by the corresponding component of the factor array.
 *
 * \sa Sample, ListSample, ImageToListSampleAdaptor
 * \ingroup ITKStatistics
 */

template <typename TImage>
class ITK_TEMPLATE_EXPORT JointDomainImageToListSampleAdaptor
  : public ListSample<typename ImageJointDomainTraits<TImage>::MeasurementVectorType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(JointDomainImageToListSampleAdaptor);

  /** Standard class type aliases */
  using Self = JointDomainImageToListSampleAdaptor;

  using Superclass = ListSample<typename ImageJointDomainTraits<TImage>::MeasurementVectorType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ImageJointDomainTraitsType = ImageJointDomainTraits<TImage>;

  using MeasurementVectorType = typename ImageJointDomainTraitsType::MeasurementVectorType;
  using MeasurementType = typename ImageJointDomainTraitsType::MeasurementType;
  using RangeDomainMeasurementType = typename ImageJointDomainTraitsType::RangeDomainMeasurementType;
  using PointType = typename ImageJointDomainTraitsType::PointType;
  using CoordinateRepType = typename ImageJointDomainTraitsType::CoordinateRepType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(JointDomainImageToListSampleAdaptor, ListSample);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** the number of components in a measurement vector */
  static constexpr unsigned int MeasurementVectorSize = ImageJointDomainTraitsType::Dimension;

  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;

  /** type alias for Measurement vector, measurement,
   * Instance Identifier, frequency, size, size element value */
  using AbsoluteFrequencyType = typename Superclass::AbsoluteFrequencyType;
  using TotalAbsoluteFrequencyType = typename Superclass::TotalAbsoluteFrequencyType;
  using InstanceIdentifier = typename Superclass::InstanceIdentifier;

  /** Image type alias */
  using ImageType = TImage;
  using ImageIteratorType = ImageRegionIterator<ImageType>;
  using ImageConstIteratorType = ImageRegionConstIterator<ImageType>;

  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using PixelType = typename ImageType::PixelType;
  using PixelContainerConstPointer = typename ImageType::PixelContainerConstPointer;
  using ImageIndexType = typename ImageType::IndexType;
  using ImageSizeType = typename ImageType::SizeType;
  using ImageRegionType = typename ImageType::RegionType;
  using ValueType = MeasurementVectorType;

  /** Method to set the image */
  void
  SetImage(const TImage * image);

  /** Method to get the image */
  const TImage *
  GetImage() const;

  /** returns the number of measurement vectors in this container */
  InstanceIdentifier
  Size() const override;

  /** Get frequency */
  AbsoluteFrequencyType
  GetFrequency(InstanceIdentifier id) const override;

  /** Get total frequency */
  TotalAbsoluteFrequencyType
  GetTotalFrequency() const override;

  static constexpr unsigned int RangeDomainDimension = itk::PixelTraits<typename TImage::PixelType>::Dimension;

  using RangeDomainMeasurementVectorType = FixedArray<RangeDomainMeasurementType, Self::RangeDomainDimension>;

  using InstanceIdentifierVectorType = std::vector<InstanceIdentifier>;
  using NormalizationFactorsType = FixedArray<float, Self::MeasurementVectorSize>;

  /** Sets the normalization factors */
  void
  SetNormalizationFactors(NormalizationFactorsType & factors);

  /** Gets the measurement vector specified by the instance
   * identifier. This method overrides superclass method. */
  const MeasurementVectorType &
  GetMeasurementVector(InstanceIdentifier id) const override;

  /** Method to set UsePixelContainer flag */
  itkSetMacro(UsePixelContainer, bool);
  itkGetConstMacro(UsePixelContainer, bool);
  itkBooleanMacro(UsePixelContainer);

  //  void PrintSelf(std::ostream& os, Indent indent) const override;

  /** \class ConstIterator
   * \brief Const Iterator
   * \ingroup ITKStatistics
   */
  class ConstIterator
  {
    friend class JointDomainImageToListSampleAdaptor;

  public:
    ConstIterator() = delete;

    ConstIterator(const JointDomainImageToListSampleAdaptor * adaptor) { *this = adaptor->Begin(); }

    ConstIterator(const ConstIterator & iter)
    {
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
      m_Adaptor = iter.m_Adaptor;
    }

    ConstIterator &
    operator=(const ConstIterator & iter)
    {
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
      m_MeasurementVectorCache = m_Adaptor->GetMeasurementVector(m_InstanceIdentifier);
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
      ++m_InstanceIdentifier;
      return *this;
    }

    bool
    operator!=(const ConstIterator & it)
    {
      return (m_InstanceIdentifier != it.m_InstanceIdentifier);
    }

    bool
    operator==(const ConstIterator & it)
    {
      return (m_InstanceIdentifier == it.m_InstanceIdentifier);
    }

  protected:
    // This method should only be available to the ListSample class
    ConstIterator(const JointDomainImageToListSampleAdaptor * adaptor, InstanceIdentifier iid)
    {
      m_Adaptor = adaptor;
      m_InstanceIdentifier = iid;
    }

  private:
    mutable MeasurementVectorType               m_MeasurementVectorCache;
    InstanceIdentifier                          m_InstanceIdentifier;
    const JointDomainImageToListSampleAdaptor * m_Adaptor;
  };

  /** \class Iterator
   * \brief Iterator
   * \ingroup ITKStatistics
   */
  class Iterator : public ConstIterator
  {
    friend class JointDomainImageToListSampleAdaptor;

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
    Iterator(const JointDomainImageToListSampleAdaptor * adaptor, InstanceIdentifier iid)
      : ConstIterator(adaptor, iid)
    {}
  };

  /** returns an iterator that points to the beginning of the container */
  Iterator
  Begin()
  {
    Iterator iter(this, 0);

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  Iterator
  End()
  {
    Iterator iter(this, m_Image->GetPixelContainer()->Size());

    return iter;
  }

  /** returns an iterator that points to the beginning of the container */
  ConstIterator
  Begin() const
  {
    ConstIterator iter(this, 0);

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  ConstIterator
  End() const
  {
    ConstIterator iter(this, m_Image->GetPixelContainer()->Size());

    return iter;
  }

protected:
  JointDomainImageToListSampleAdaptor();
  ~JointDomainImageToListSampleAdaptor() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  NormalizationFactorsType                 m_NormalizationFactors;
  mutable MeasurementVectorType            m_TempVector;
  mutable PointType                        m_TempPoint;
  mutable ImageIndexType                   m_TempIndex;
  mutable RangeDomainMeasurementVectorType m_TempRangeVector;
  ImageConstPointer                        m_Image;
  bool                                     m_UsePixelContainer;

  PixelContainerConstPointer m_PixelContainer;
}; // end of class JointDomainImageToListSampleAdaptor
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkJointDomainImageToListSampleAdaptor.hxx"
#endif

#endif
