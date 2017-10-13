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
#ifndef itkVectorContainerToListSampleAdaptor_h
#define itkVectorContainerToListSampleAdaptor_h

#include <typeinfo>

#include "itkListSample.h"
#include "itkSmartPointer.h"
#include "itkVectorContainer.h"

namespace itk
{
namespace Statistics
{
/** \class VectorContainerToListSampleAdaptor
 *  \brief This class provides ListSample interface to ITK VectorContainer
 *
 * After calling SetVectorContainer(VectorContainer*) method to plug-in
 * the VectorContainer object, users can use Sample interfaces to access
 * VectorContainer data. This adaptor assumes that the VectorContainer is
 * actual storage for measurment vectors. In other words, VectorContainer's
 * element dimension equals to the measurement vectors size.
 *
 * \sa Sample, ListSample, VectorContainer
 *
 * \ingroup ITKStatistics
 */

template< typename TVectorContainer >
class ITK_TEMPLATE_EXPORT VectorContainerToListSampleAdaptor:
  public ListSample< typename TVectorContainer::Element >
{
public:
  /** Standard class typedefs */
  typedef VectorContainerToListSampleAdaptor                   Self;
  typedef ListSample<typename TVectorContainer::Element>       Superclass;
  typedef SmartPointer< Self >                                 Pointer;
  typedef SmartPointer< const Self >                           ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( VectorContainerToListSampleAdaptor, ListSample );

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** the number of components in a measurement vector */
  itkStaticConstMacro( MeasurementVectorSize, unsigned int,
                       TVectorContainer::Element::Dimension );

  /** VectorContainer typedefs */
  typedef TVectorContainer                          VectorContainerType;
  typedef typename TVectorContainer::Pointer        VectorContainerPointer;
  typedef typename TVectorContainer::ConstPointer   VectorContainerConstPointer;
  typedef typename TVectorContainer::Iterator       VectorContainerIterator;
  typedef typename TVectorContainer::ConstIterator  VectorContainerConstIterator;

  /** Superclass typedefs for Measurement vector, measurement,
   * Instance Identifier, frequency, size, size element value */
  typedef typename Superclass::MeasurementType            MeasurementType;
  typedef typename Superclass::MeasurementVectorType      MeasurementVectorType;
  typedef typename Superclass::AbsoluteFrequencyType      AbsoluteFrequencyType;
  typedef typename Superclass::TotalAbsoluteFrequencyType TotalAbsoluteFrequencyType;
  typedef typename Superclass::MeasurementVectorSizeType  MeasurementVectorSizeType;
  typedef typename Superclass::InstanceIdentifier         InstanceIdentifier;

  typedef MeasurementVectorType ValueType;

  /** Get/Set Method for the point set */
  itkSetObjectMacro( VectorContainer, VectorContainerType );
  itkGetConstObjectMacro(VectorContainer, VectorContainerType );

  /** returns the number of measurement vectors in this container */
  InstanceIdentifier Size() const ITK_OVERRIDE;

  /** returns the measurement vector that is specified by the instance
   * identifier argument. */
  const MeasurementVectorType & GetMeasurementVector( InstanceIdentifier ) const ITK_OVERRIDE;

  /** returns 1 as other subclasses of ListSampleBase does */
  AbsoluteFrequencyType GetFrequency( InstanceIdentifier ) const ITK_OVERRIDE;

  /** returns the size of this container */
  TotalAbsoluteFrequencyType GetTotalFrequency() const ITK_OVERRIDE;

  /** \class ConstIterator
   * \ingroup ITKStatistics
   */
  class ConstIterator
  {
    friend class VectorContainerToListSampleAdaptor;
  public:

    ConstIterator(const VectorContainerToListSampleAdaptor *adaptor)
    {
      *this = adaptor->Begin();
    }

    ConstIterator(const ConstIterator & iter)
    {
      this->m_Iter = iter.m_Iter;
      this->m_InstanceIdentifier = iter.m_InstanceIdentifier;
    }

    ConstIterator & operator=(const ConstIterator & iter)
    {
      this->m_Iter = iter.m_Iter;
      this->m_InstanceIdentifier = iter.m_InstanceIdentifier;
      return *this;
    }

    AbsoluteFrequencyType GetFrequency() const
    {
      return 1;
    }

    const MeasurementVectorType & GetMeasurementVector() const
    {
      return ( const MeasurementVectorType & )m_Iter.Value();
    }

    InstanceIdentifier GetInstanceIdentifier() const
    {
      return this->m_InstanceIdentifier;
    }

    ConstIterator & operator++()
    {
      ++m_Iter;
      ++m_InstanceIdentifier;
      return *this;
    }

    bool operator!=( const ConstIterator & it )
    {
      return ( this->m_Iter != it.m_Iter );
    }

    bool operator==( const ConstIterator & it )
    {
      return ( this->m_Iter == it.m_Iter );
    }

  protected:
    // This method should only be available to the ListSample class
    ConstIterator( VectorContainerConstIterator iter,
      InstanceIdentifier iid )
    {
      this->m_Iter = iter;
      this->m_InstanceIdentifier = iid;
    }

  private:
    ConstIterator() ITK_DELETED_FUNCTION;
    VectorContainerConstIterator      m_Iter;
    InstanceIdentifier                m_InstanceIdentifier;
  };

  /** \class Iterator
   * \ingroup ITKStatistics
   */
  class Iterator:public ConstIterator
  {
    friend class VectorContainerToListSampleAdaptor;
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
    Iterator( VectorContainerIterator iter, InstanceIdentifier iid )
      :ConstIterator( iter, iid )
    {}

  private:
    // To ensure const-correctness these method must not be in the public API.
    // The are not implemented, since they should never be called.
    Iterator() ITK_DELETED_FUNCTION;
    Iterator( const Self *adaptor ) ITK_DELETED_FUNCTION;
    Iterator( VectorContainerConstIterator iter, InstanceIdentifier iid ) ITK_DELETED_FUNCTION;
    Iterator( const ConstIterator & it) ITK_DELETED_FUNCTION;
    ConstIterator & operator=( const ConstIterator & it ) ITK_DELETED_FUNCTION;
  };

  /** returns an iterator that points to the beginning of the container */
  Iterator Begin()
  {
    VectorContainerPointer nonConstVectorDataContainer =
      const_cast< VectorContainerType * >( this->m_VectorContainer.GetPointer() );
    Iterator iter( nonConstVectorDataContainer->Begin(), 0 );

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  Iterator End()
  {
    VectorContainerPointer nonConstVectorDataContainer =
      const_cast<VectorContainerType *>( this->m_VectorContainer.GetPointer() );

    Iterator iter( nonConstVectorDataContainer->End(),
      this->m_VectorContainer->Size() );

    return iter;
  }

  /** returns an iterator that points to the beginning of the container */
  ConstIterator Begin() const
  {
    ConstIterator iter( this->m_VectorContainer->Begin(), 0 );

    return iter;
  }

  /** returns an iterator that points to the end of the container */
  ConstIterator End() const
  {
    ConstIterator iter( this->m_VectorContainer->End(),
      this->m_VectorContainer->Size() );
    return iter;
  }

protected:
  VectorContainerToListSampleAdaptor();

  virtual ~VectorContainerToListSampleAdaptor() ITK_OVERRIDE {}
  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorContainerToListSampleAdaptor);

  /** the points container which will be actually used for storing
   * measurement vectors */
  VectorContainerConstPointer  m_VectorContainer;

  /** temporary points for conversions */
  mutable typename VectorContainerType::Element m_TempPoint;
};  // end of class VectorContainerToListSampleAdaptor
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorContainerToListSampleAdaptor.hxx"
#endif

#endif
