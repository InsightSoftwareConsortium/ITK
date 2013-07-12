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
#ifndef __itkImageToListSampleAdaptor_h
#define __itkImageToListSampleAdaptor_h

#include <typeinfo>

#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkListSample.h"
#include "itkSmartPointer.h"
#include "itkImageRegionIterator.h"
#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
/** \class ImageToListSampleAdaptor
 *  \brief This class provides ListSample interface to ITK Image
 *
 * After calling SetImage( const Image * ) method to plug in the image object,
 * users can use Sample interfaces to access Image data. The resulting data
 * are a list of measurement vectors.
 *
 * The measurment vector type is determined from the image pixel type. This class
 * handles images with scalar, fixed array or variable length vector pixel types.
 *
 * \sa Sample, ListSample
 * \ingroup ITKStatistics
 *
 * \wiki
 * \wikiexample{Statistics/ImageToListSampleAdaptor,Create a list of samples from an image without duplicating the data}
 * \endwiki
 */

template< class TImage >
class ImageToListSampleAdaptor:
  public ListSample< typename MeasurementVectorPixelTraits< typename TImage::PixelType >::MeasurementVectorType >
{
public:
  /** Standard class typedefs */
  typedef ImageToListSampleAdaptor Self;

  typedef ListSample< typename MeasurementVectorPixelTraits<
                        typename TImage::PixelType >::MeasurementVectorType >
  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToListSampleAdaptor, ListSample);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Image typedefs */
  typedef TImage                                         ImageType;
  typedef typename ImageType::Pointer                    ImagePointer;
  typedef typename ImageType::ConstPointer               ImageConstPointer;
  typedef typename ImageType::IndexType                  IndexType;
  typedef typename ImageType::PixelType                  PixelType;
  typedef typename ImageType::PixelContainerConstPointer PixelContainerConstPointer;

  /** Image Iterator typedef support */
  typedef ImageRegionIterator< ImageType >          ImageIteratorType;
  typedef ImageRegionConstIterator< ImageType >     ImageConstIteratorType;
  typedef PixelTraits< typename TImage::PixelType > PixelTraitsType;


  /** Superclass typedefs for Measurement vector, measurement,
   * Instance Identifier, frequency, size, size element value */
  typedef MeasurementVectorPixelTraits< PixelType >                   MeasurementPixelTraitsType;
  typedef typename MeasurementPixelTraitsType::MeasurementVectorType  MeasurementVectorType;

  typedef MeasurementVectorTraitsTypes< MeasurementVectorType > MeasurementVectorTraitsType;
  typedef typename MeasurementVectorTraitsType::ValueType       MeasurementType;

  typedef typename Superclass::AbsoluteFrequencyType      AbsoluteFrequencyType;
  typedef typename Superclass::TotalAbsoluteFrequencyType TotalAbsoluteFrequencyType;
  typedef typename Superclass::MeasurementVectorSizeType  MeasurementVectorSizeType;
  typedef typename Superclass::InstanceIdentifier         InstanceIdentifier;

  typedef MeasurementVectorType ValueType;

  /** Method to set the image */
  void SetImage(const TImage *image);

  /** Method to get the image */
  const TImage * GetImage() const;

  /** returns the number of measurement vectors in this container */
  InstanceIdentifier Size() const;

  /** method to return measurement vector for a specified id */
  virtual const MeasurementVectorType & GetMeasurementVector(InstanceIdentifier id) const;

  virtual MeasurementVectorSizeType GetMeasurementVectorSize() const
  {
    // some filter are expected that this method returns something even if the
    // input is not set. This won't be the right value for a variable length vector
    // but it's better than an exception.
    if( m_Image.IsNull() )
      {
      return Superclass::GetMeasurementVectorSize();
      }
    else
      {
      return m_Image->GetNumberOfComponentsPerPixel();
      }
  }

  /** method to return frequency for a specified id */
  AbsoluteFrequencyType GetFrequency(InstanceIdentifier id) const;

  /** method to return the total frequency */
  TotalAbsoluteFrequencyType GetTotalFrequency() const;

  /** \class ConstIterator
   *  \brief Const Iterator
   * \ingroup ITKStatistics
   */
  class ConstIterator
  {
    friend class ImageToListSampleAdaptor;

public:

    ConstIterator(const ImageToListSampleAdaptor *adaptor)
    {
      *this = adaptor->Begin();
    }

    ConstIterator(const ConstIterator & iter)
    {
      m_Iter = iter.m_Iter;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
    }

    ConstIterator & operator=(const ConstIterator & iter)
    {
      m_Iter = iter.m_Iter;
      m_InstanceIdentifier = iter.m_InstanceIdentifier;
      return *this;
    }

    AbsoluteFrequencyType GetFrequency() const
    {
      return 1;
    }

    const MeasurementVectorType & GetMeasurementVector() const
    {
      MeasurementVectorTraits::Assign( this->m_MeasurementVectorCache, m_Iter.Get() );
      return this->m_MeasurementVectorCache;
    }

    InstanceIdentifier GetInstanceIdentifier() const
    {
      return m_InstanceIdentifier;
    }

    ConstIterator & operator++()
    {
      ++m_Iter;
      ++m_InstanceIdentifier;
      return *this;
    }

    bool operator!=(const ConstIterator & it)
    {
      return ( m_Iter != it.m_Iter );
    }

    bool operator==(const ConstIterator & it)
    {
      return ( m_Iter == it.m_Iter );
    }

protected:
    // This method should only be available to the ListSample class
    ConstIterator(
      ImageConstIteratorType iter,
      InstanceIdentifier iid)
    {
      m_Iter = iter;
      m_InstanceIdentifier = iid;
    }

    // This method is purposely not implemented
    ConstIterator();

private:
    ImageConstIteratorType        m_Iter;
    mutable MeasurementVectorType m_MeasurementVectorCache;
    InstanceIdentifier            m_InstanceIdentifier;
  };

  /** \class Iterator
   *  \brief Iterator
   * \ingroup ITKStatistics
   */
  class Iterator:public ConstIterator
  {
    friend class ImageToListSampleAdaptor;

public:

    Iterator(Self *adaptor):ConstIterator(adaptor)
    {}

    Iterator(const Iterator & iter):ConstIterator(iter)
    {}

    Iterator & operator=(const Iterator & iter)
    {
      this->ConstIterator::operator=(iter);
      return *this;
    }

protected:
    // To ensure const-correctness these method must not be in the public API.
    // The are purposly not implemented, since they should never be called.
    Iterator();
    Iterator(const Self *adaptor);
    Iterator(ImageConstIteratorType iter, InstanceIdentifier iid);
    Iterator(const ConstIterator & it);
    ConstIterator & operator=(const ConstIterator & it);

    Iterator( ImageIteratorType iter, InstanceIdentifier iid):ConstIterator(iter, iid)
    {}

private:
  };

  /** returns an iterator that points to the beginning of the container */
  Iterator Begin()
  {
    ImagePointer           nonConstImage = const_cast< ImageType * >( m_Image.GetPointer() );
    ImageIteratorType imageIterator( nonConstImage, nonConstImage->GetLargestPossibleRegion() );
    imageIterator.GoToBegin();
    Iterator               iter(imageIterator, 0);
    return iter;
  }

  /** returns an iterator that points to the end of the container */
  Iterator End()
  {
    ImagePointer           nonConstImage = const_cast< ImageType * >( m_Image.GetPointer() );
    ImageIteratorType imageIterator( nonConstImage, nonConstImage->GetLargestPossibleRegion() );
    imageIterator.GoToEnd();
    Iterator          iter( imageIterator, m_Image->GetPixelContainer()->Size() );

    return iter;
  }

  /** returns an iterator that points to the beginning of the container */
  ConstIterator Begin() const
  {
    ImageConstIteratorType imageConstIterator( m_Image, m_Image->GetLargestPossibleRegion() );
    imageConstIterator.GoToBegin();
    ConstIterator          iter(imageConstIterator, 0);

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  ConstIterator End() const
  {
    ImageConstIteratorType imageConstIterator( m_Image, m_Image->GetLargestPossibleRegion() );
    imageConstIterator.GoToEnd();
    ConstIterator          iter( imageConstIterator, m_Image->GetPixelContainer()->Size() );

    return iter;
  }

protected:
  ImageToListSampleAdaptor();
  virtual ~ImageToListSampleAdaptor() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  ImageToListSampleAdaptor(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented

  ImageConstPointer             m_Image;
  mutable MeasurementVectorType m_MeasurementVectorInternal;

};  // end of class ImageToListSampleAdaptor
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToListSampleAdaptor.hxx"
#endif

#endif
