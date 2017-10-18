/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

namespace itk {
namespace Statistics {

/** \class ImageToNeighborhoodSampleAdaptor
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

  template < typename TImage, typename TBoundaryCondition >
class ITK_TEMPLATE_EXPORT ImageToNeighborhoodSampleAdaptor :
  public ListSample< std::vector< ConstNeighborhoodIterator< TImage, TBoundaryCondition > > >
{
public:
  /** Standard class typedefs */
  typedef ImageToNeighborhoodSampleAdaptor               Self;

  typedef ListSample< std::vector< ConstNeighborhoodIterator< TImage, TBoundaryCondition > > >
                                                         Superclass;

  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer<const Self>                       ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToNeighborhoodSampleAdaptor, ListSample);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Image typedefs */
  typedef TImage                                          ImageType;
  typedef typename ImageType::Pointer                     ImagePointer;
  typedef typename ImageType::ConstPointer                ImageConstPointer;
  typedef typename ImageType::IndexType                   IndexType;
  typedef typename ImageType::OffsetType                  OffsetType;
  typedef typename ImageType::OffsetValueType             OffsetValueType;
  typedef typename ImageType::PixelType                   PixelType;
  typedef typename ImageType::PixelContainerConstPointer  PixelContainerConstPointer;
  typedef typename ImageType::RegionType                  RegionType;
  typedef typename RegionType::OffsetTableType            OffsetTableType;
  typedef typename ImageType::SizeType                    SizeType;
  typedef ImageRegionIteratorWithIndex< TImage >          ImageIteratorType;
  /** Neighborhood Iterator typedef support */
  typedef ConstNeighborhoodIterator< TImage, TBoundaryCondition > NeighborhoodIteratorType;
  typedef NeighborhoodIterator< TImage, TBoundaryCondition >      NonConstNeighborhoodIteratorType;
  typedef typename NeighborhoodIteratorType::NeighborhoodType     NeighborhoodType;
  typedef typename NeighborhoodIteratorType::RadiusType           NeighborhoodRadiusType;
  typedef typename NeighborhoodIteratorType::IndexType            NeighborhoodIndexType;
  typedef typename NeighborhoodIteratorType::SizeType             NeighborhoodSizeType;

  /** Superclass typedefs for Measurement vector, measurement,
   * Instance Identifier, frequency, size, size element value */
  typedef typename std::vector< ConstNeighborhoodIterator< TImage, TBoundaryCondition > >
                                                     MeasurementVectorType;
  typedef typename MeasurementVectorType::value_type ValueType;
  typedef ValueType                                  MeasurementType;

  typedef typename Superclass::AbsoluteFrequencyType      AbsoluteFrequencyType;
  typedef typename Superclass::TotalAbsoluteFrequencyType TotalAbsoluteFrequencyType;
  typedef typename Superclass::MeasurementVectorSizeType  MeasurementVectorSizeType;
  typedef typename Superclass::InstanceIdentifier         InstanceIdentifier;

  /** Method to set the image */
  void SetImage(const TImage* image);

  /** Method to get the image */
  const TImage* GetImage() const;

  /** Method to set the radius */
  void SetRadius(const NeighborhoodRadiusType& radius);

  /** Method to get the radius */
  NeighborhoodRadiusType GetRadius() const;

  /** Method to set the region */
  void SetRegion(const RegionType& region);

  /** Method to get the region */
  RegionType GetRegion() const;

  void SetUseImageRegion(const bool& flag);

  /** Method to get UseImageRegion flag */
  itkGetConstMacro( UseImageRegion, bool );

  /** Convenience methods to turn on/off the UseImageRegion flag */
  itkBooleanMacro( UseImageRegion );


  /** returns the number of measurement vectors in this container */
  InstanceIdentifier Size() const ITK_OVERRIDE;

  /** method to return measurement vector for a specified id */
  virtual const MeasurementVectorType & GetMeasurementVector(InstanceIdentifier id) const ITK_OVERRIDE;

  /** method to return frequency for a specified id */
  AbsoluteFrequencyType GetFrequency(InstanceIdentifier  id) const ITK_OVERRIDE;

  /** method to return the total frequency */
  TotalAbsoluteFrequencyType GetTotalFrequency() const ITK_OVERRIDE;

  /** \class ConstIterator
   *  \brief Const Iterator
   *  \ingroup ITKStatistics
   */
  class ConstIterator
    {
    friend class ImageToNeighborhoodSampleAdaptor;
    public:

    ConstIterator( const ImageToNeighborhoodSampleAdaptor * adaptor )
      {
      *this = adaptor->Begin();
      }

    ConstIterator(const ConstIterator &iter)
      {
        m_MeasurementVectorCache = iter.m_MeasurementVectorCache;
        m_InstanceIdentifier = iter.m_InstanceIdentifier;
      }

    ConstIterator& operator=( const ConstIterator & iter )
      {
        m_MeasurementVectorCache = iter.m_MeasurementVectorCache;
        m_InstanceIdentifier = iter.m_InstanceIdentifier;
        return *this;
      }

    AbsoluteFrequencyType GetFrequency() const
      {
      return 1;
      }

    const MeasurementVectorType & GetMeasurementVector() const
      {
        return this->m_MeasurementVectorCache;
      }

    InstanceIdentifier GetInstanceIdentifier() const
      {
      return m_InstanceIdentifier;
      }

    ConstIterator& operator++()
      {
        ++(m_MeasurementVectorCache[0]);
        ++m_InstanceIdentifier;
      return *this;
      }

    bool operator!=(const ConstIterator &it)
      {
      return (m_MeasurementVectorCache[0] != it.m_MeasurementVectorCache[0]);
      }

    bool operator==(const ConstIterator &it)
      {
      return (m_MeasurementVectorCache[0] == it.m_MeasurementVectorCache[0]);
      }

  protected:
    // This method should only be available to the ListSample class
    ConstIterator(
      NeighborhoodIteratorType iter,
      InstanceIdentifier iid)
      {
        this->m_MeasurementVectorCache.clear();
        this->m_MeasurementVectorCache.push_back(iter);
        m_InstanceIdentifier = iid;
      }

  private:
    ConstIterator() ITK_DELETED_FUNCTION;
    mutable MeasurementVectorType     m_MeasurementVectorCache;
    InstanceIdentifier                m_InstanceIdentifier;
    };

 /** \class Iterator
   *  \brief Iterator
   *  \ingroup ITKStatistics
   */
  class Iterator : public ConstIterator
  {

  friend class ImageToNeighborhoodSampleAdaptor;

  public:

    Iterator(Self * adaptor):ConstIterator(adaptor)
      {
      }

    Iterator(const Iterator &iter):ConstIterator( iter )
      {
      }

    Iterator& operator =(const Iterator & iter)
      {
      this->ConstIterator::operator=( iter );
      return *this;
      }

#if !(defined(_MSC_VER) && (_MSC_VER <= 1200))
  protected:
#endif
    //This copy constructor is actually used in Iterator Begin()!
    Iterator(NeighborhoodIteratorType iter, InstanceIdentifier iid):ConstIterator( iter, iid )
      {
      }

  private:
    // To ensure const-correctness these method must not be in the public API.
    // The are not implemented, since they should never be called.
    Iterator() ITK_DELETED_FUNCTION;
    Iterator(const Self * adaptor) ITK_DELETED_FUNCTION;
    Iterator(const ConstIterator & it) ITK_DELETED_FUNCTION;
    ConstIterator& operator=(const ConstIterator& it) ITK_DELETED_FUNCTION;
  };

  /** returns an iterator that points to the beginning of the container */
  Iterator Begin()
    {
    NeighborhoodIteratorType nIterator( m_Radius, m_Image, m_Region);
    nIterator.GoToBegin();
    Iterator iter(nIterator, 0);
    return iter;
    }

  /** returns an iterator that points to the end of the container */
  Iterator End()
    {
    NeighborhoodIteratorType nIterator( m_Radius, m_Image, m_Region);
    nIterator.GoToEnd();
    Iterator iter(nIterator, m_Region.GetNumberOfPixels());
    return iter;
    }


  /** returns an iterator that points to the beginning of the container */
  ConstIterator Begin() const
    {
    NeighborhoodIteratorType nIterator( m_Radius, m_Image, m_Region);
    nIterator.GoToBegin();
    ConstIterator iter(nIterator, 0);
    return iter;
    }

  /** returns an iterator that points to the end of the container */
  ConstIterator End() const
    {
    NeighborhoodIteratorType nIterator( m_Radius, m_Image, m_Region);
    nIterator.GoToEnd();
    ConstIterator iter(nIterator, m_Region.GetNumberOfPixels());
    return iter;
    }

protected:
  ImageToNeighborhoodSampleAdaptor();
  virtual ~ImageToNeighborhoodSampleAdaptor() ITK_OVERRIDE {}
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToNeighborhoodSampleAdaptor);

  ImageConstPointer                  m_Image;
  mutable MeasurementVectorType      m_MeasurementVectorInternal;
  mutable InstanceIdentifier         m_InstanceIdentifierInternal;
  mutable IndexType                  m_NeighborIndexInternal;
  NeighborhoodRadiusType             m_Radius;
  RegionType                         m_Region;
  bool                               m_UseImageRegion;
  OffsetTableType                    m_OffsetTable;

}; // end of class ImageToNeighborhoodSampleAdaptor

} // end of namespace Statistics

template <typename TImage, typename TBoundaryCondition>
  std::ostream & operator<<(std::ostream &os,
                            const std::vector< itk::ConstNeighborhoodIterator<TImage, TBoundaryCondition> > &mv);

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToNeighborhoodSampleAdaptor.hxx"
#endif

#endif
