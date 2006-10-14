/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSample_h
#define __itkListSample_h

#include "itkObjectFactory.h"
#include "itkMacro.h"
#include "itkListSampleBase.h"
#include "itkFixedArray.h"
#include "itkSmartPointer.h"

#include <vector>

namespace itk{ 
namespace Statistics{

/** \class ListSample 
 *  \brief This class is the native implementation of the ListSampleBase
 *
 * ListSampleBase stores measurements in a list type structure (as
 * opposed to a Histogram, etc.).  ListSampleBase allows duplicate
 * measurements. ListSampleBase is not sorted.
 *
 * ListSampleBase does not allow the user to specify the frequency of
 * a measurement directly.  The GetFrequency() methods returns 1 if
 * the measurement exists in the list, 0 otherwise.
 *
 *\sa ListSampleBase, Histogram
 */

template< class TMeasurementVector >
class ITK_EXPORT ListSample : public ListSampleBase< TMeasurementVector >
{
public:
  /** Standard class typedef. */
  typedef ListSample  Self;
  typedef ListSampleBase< TMeasurementVector > Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard macros */
  itkTypeMacro(ListSample, ListSampleBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;
  
  /** Typedefs inherited from the superclass */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;
  typedef typename Superclass::MeasurementType MeasurementType;
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::TotalFrequencyType TotalFrequencyType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;
  typedef typename Superclass::SearchResultVectorType SearchResultVectorType ;

  /** Value type of a measurement (component of the measurement
   * vector) */
  typedef MeasurementVectorType ValueType ;


  /** internal data container type */
  typedef std::vector< MeasurementVectorType > InternalDataContainerType ;

  /** Resize the container. If this sample is connected to a Subsample or
   * MembershipSample, then this function won't change the size of 
   * this container, instead, it will throw exception. Therefore,
   * resize the container before using the sample in a Subsample or
   * MembershipSample. */
  void Resize( unsigned int n ) 
  { m_InternalContainer.resize(n) ; }

  /** Removes all the elements in the Sample*/
  void Clear() 
  { m_InternalContainer.clear() ; }

  /** Inserts a measurement at the end of the list */
  void PushBack( MeasurementVectorType mv )
  { m_InternalContainer.push_back( mv ) ; }

  /** Get the number of measurement vectors in the sample*/
  unsigned int Size() const
  { return static_cast<unsigned int>( m_InternalContainer.size() ); }

  /** Get the measurement associated with the specified
   * InstanceIdentifier */
  const MeasurementVectorType & GetMeasurementVector(const InstanceIdentifier &id) const;

  /** Set a component a measurement to a particular value. */
  void SetMeasurement(const InstanceIdentifier &id, 
                      const unsigned int &dim,
                      const MeasurementType &value) ;

  /** Replace a measurement with a different measurement */
  void SetMeasurementVector(const InstanceIdentifier &id, 
                            const MeasurementVectorType &mv) ;

  /** Get the frequency of a measurement. Returns 1 if the measurement
   * exist. */
  FrequencyType GetFrequency(const InstanceIdentifier &id) const ;

  /** Get the total frequency of the sample.  This is equivalent to
   * the size of the sample. */
  TotalFrequencyType GetTotalFrequency() const
  { return static_cast<TotalFrequencyType>( m_InternalContainer.size() ); }

  class ConstIterator;
 
  class Iterator
  {

    friend class ConstIterator;

  public:
    
    Iterator(){}
    
    Iterator(typename InternalDataContainerType::iterator iter, 
             InstanceIdentifier iid)
      :m_Iter(iter), m_InstanceIdentifier(iid)
    {}
    
    FrequencyType GetFrequency() const
    { return 1 ;}

    const MeasurementVectorType & GetMeasurementVector() const
    { return (MeasurementVectorType&) *m_Iter ;} 

    InstanceIdentifier GetInstanceIdentifier() const
    { return m_InstanceIdentifier ;}

    Iterator& operator++()
    { ++m_Iter ; ++m_InstanceIdentifier ; return *this ;}
    
    Iterator& operator--()
    { 
      --m_Iter ; 

      if ( m_InstanceIdentifier > 1 )
        m_InstanceIdentifier; 
      
      return *this ;
    }

    bool operator!=(const Iterator &it)
    { return (m_Iter != it.m_Iter) ;}
    
    bool operator==(const Iterator &it)
    { return (m_Iter == it.m_Iter) ;}
    
    Iterator& operator = (const Iterator & iter)
    { 
      m_Iter = iter.m_Iter; 
      m_InstanceIdentifier = iter.m_InstanceIdentifier ;
      return *this ;
    }

    Iterator(const Iterator &iter)
    {
      m_Iter = iter.m_Iter; 
      m_InstanceIdentifier = iter.m_InstanceIdentifier ;
    }
    
  private:
    typename InternalDataContainerType::iterator m_Iter ;
    InstanceIdentifier m_InstanceIdentifier ;
  } ;

 
  class ConstIterator
  {
  public:
    
    ConstIterator(){}
    
    ConstIterator(typename InternalDataContainerType::const_iterator iter, 
             InstanceIdentifier iid)
      :m_Iter(iter), m_InstanceIdentifier(iid)
    {}
    
    FrequencyType GetFrequency() const
    { return 1 ;}

    const MeasurementVectorType & GetMeasurementVector() const
    { return static_cast<const MeasurementVectorType&>(*m_Iter) ;} 

    InstanceIdentifier GetInstanceIdentifier() const
    { return m_InstanceIdentifier ;}

    ConstIterator& operator++()
    { ++m_Iter ; ++m_InstanceIdentifier ; return *this ;}
    
    ConstIterator& operator--()
    { 
      --m_Iter ; 

      if ( m_InstanceIdentifier > 1 )
        m_InstanceIdentifier; 
      
      return *this ;
    }

    bool operator!=(const ConstIterator &it)
    { return (m_Iter != it.m_Iter) ;}
    
    bool operator==(const ConstIterator &it)
    { return (m_Iter == it.m_Iter) ;}
    
    ConstIterator& operator = (const ConstIterator iter)
    { 
      m_Iter = iter.m_Iter; 
      m_InstanceIdentifier = iter.m_InstanceIdentifier ;
      return *this ;
    }

    ConstIterator& operator = (const Iterator & iter)
    { 
      m_Iter = iter.m_Iter; 
      m_InstanceIdentifier = iter.m_InstanceIdentifier ;
      return *this ;
    }


    ConstIterator(const ConstIterator &iter)
    {
      m_Iter = iter.m_Iter; 
      m_InstanceIdentifier = iter.m_InstanceIdentifier ;
    }

    ConstIterator(const Iterator &iter)
    {
      m_Iter = iter.m_Iter; 
      m_InstanceIdentifier = iter.m_InstanceIdentifier ;
    }
    
  private:
    typename InternalDataContainerType::const_iterator m_Iter ;
    InstanceIdentifier m_InstanceIdentifier ;
  } ;

  /** returns an iterator that points to the beginning of the container */
  Iterator Begin()
  { 
    Iterator iter(m_InternalContainer.begin(), 0);
    return iter; 
  }
  
  /** returns an iterator that points to the end of the container */
  Iterator End()        
  {
    Iterator iter(m_InternalContainer.end(), m_InternalContainer.size()); 
    return iter; 
  }

  /** returns an iterator that points to the beginning of the container */
  ConstIterator Begin() const
  { 
    ConstIterator iter(m_InternalContainer.begin(), 0);
    return iter; 
  }
  
  /** returns an iterator that points to the end of the container */
  ConstIterator End() const
  {
    ConstIterator iter(m_InternalContainer.end(), m_InternalContainer.size()); 
    return iter; 
  }
  
  virtual MeasurementVectorSizeType GetMeasurementVectorSize() const
    {
    if ( !this->Superclass::GetMeasurementVectorSize() && this->Size())
      { // determined from the length of the first vector in the sample
        // at run time
      return MeasurementVectorTraits::GetLength(this->GetMeasurementVector(0));
      }
    return this->Superclass::GetMeasurementVectorSize();
    }
 
protected:
  ListSample() ;
  virtual ~ListSample() {}
  void PrintSelf(std::ostream& os, Indent indent) const; 
  

private:
  ListSample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  InternalDataContainerType m_InternalContainer ;

};

} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkListSample.txx"
#endif

#endif
