/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
 * ListSample allows duplicates of measurement vectors. It's not sorted.
 * It doesn't allow users to set frequency. The GetFrequency(...) methods
 * returns 1 if a measurement vector exists, else 0.
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

  /** Standard macros */
  itkTypeMacro(ListSample, ListSampleBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;
  
  /** Superclass typedefs for Measurement vector, 
   * measurement, Instance Identifier, 
   * frequency, size, size element value */

  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::MeasurementType MeasurementType;
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;
  typedef MeasurementVectorType ValueType ;

  /** VMeasurementVectorSize template argument alias */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      Superclass::MeasurementVectorSize);

  /** internal data container type */
  typedef std::vector< MeasurementVectorType > InternalDataContainerType ;

  /** resize the container, if this sample is connected to Subsample or
   * MembershipSample, then this function won't change the size of 
   * this container, instead, it will throw exception. Before, use this sample
   * with Subsample or MembershipSample, set the size*/
  void Resize( unsigned int n ) 
  { m_InternalContainer.resize(n) ; }

  /** inserts a new element at the end */
  void PushBack( MeasurementVectorType &mv )
  { m_InternalContainer.push_back( mv ) ; }

  /** returns the number of measurement vectors in this container*/
  unsigned int Size() const
  { return static_cast<unsigned int>( m_InternalContainer.size() ); }

  /** returns the measurement vector that is specified by the instance
   * identifier argument. */
  MeasurementVectorType GetMeasurementVector(const InstanceIdentifier &id) ;

  /** sets the "dim" dimensional component value of the measurement vector
   * that is specified by "id". */
  void SetMeasurement(const InstanceIdentifier &id, 
                      const unsigned int &dim,
                      const MeasurementType &value) ;

  /** set the measurement vector value that is specified by the instance
   * identifier argument. */
  void SetMeasurementVector(const InstanceIdentifier &id, 
                            const MeasurementVectorType &mv) ;

  /** returns 1 as other subclasses of ListSampleBase does */
  FrequencyType GetFrequency(const InstanceIdentifier &id) const ;

  /** returns the size of this container */
  FrequencyType GetTotalFrequency() const
  { return static_cast<FrequencyType>( m_InternalContainer.size() ); }

  /** iterator support */
  class Iterator;
  friend class Iterator;
  
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
  
  class Iterator
  {
  public:
    
    Iterator(){}
    
    Iterator(typename InternalDataContainerType::iterator iter, 
             InstanceIdentifier iid)
      :m_Iter(iter), m_InstanceIdentifier(iid)
    {}
    
    FrequencyType GetFrequency() const
    { return 1 ;}

    MeasurementVectorType GetMeasurementVector()
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
    
    Iterator& operator = (const Iterator &iter)
    { 
      m_Iter = iter.m_Iter; 
      m_InstanceIdentifier = iter.m_InstanceIdentifier ;
      return iter ;
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
