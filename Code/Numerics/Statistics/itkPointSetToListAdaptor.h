/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkPointSetToListAdaptor.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToListAdaptor_h
#define __itkPointSetToListAdaptor_h

#include <typeinfo>

#include "itkPointSet.h"
#include "itkListSample.h"
#include "itkSmartPointer.h"

namespace itk{ 
namespace Statistics{

/** \class PointSetToListAdaptor
 *  \brief This class provides ListSample interfaces to ITK Image
 *
 * After calling SetPointSet(PointSet::Pointer) method to plug-in 
 * the PointSet object,
 * users can use Sample interfaces to access PointSet data.
 *
 * \sa Sample, ListSample
 */

template < class TPointSet >
class ITK_EXPORT PointSetToListAdaptor :
    public ListSample< typename TPointSet::PointType >
{
public:
  /** Standard class typedefs */
  typedef PointSetToListAdaptor Self;
  typedef ListSample< typename TPointSet::PointType > Superclass ;
  typedef SmartPointer< Self > Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToListAdaptor, ListSample) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;
  
  /** the number of components in a measurement vector */
  enum { MeasurementVectorSize = TPointSet::PointDimension } ;

  /** PointSet typedefs */
  typedef TPointSet PointSetType;
  typedef typename TPointSet::Pointer PointSetPointer ;
  typedef typename TPointSet::PointIdentifier InstanceIdentifier;
  typedef typename TPointSet::PointsContainerPointer PointsContainerPointer ;
  typedef typename TPointSet::PointsContainerIterator PointsContainerIterator ;
  typedef typename TPointSet::PointType PointType ;


  /** Superclass typedefs for Measurement vector, measurement, 
   * Instance Identifier, frequency, size, size element value */
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef MeasurementVectorType ValueType ;
  typedef typename Superclass::FrequencyType FrequencyType ;

  /** Method to set the point set */
  void SetPointSet(PointSetPointer pointSet) ;

  /** Method to get the point set */
  PointSetPointer GetPointSet() ;

  /** returns the number of measurement vectors in this container*/
  unsigned int Size() const ;

  unsigned int Size(const unsigned int &dimension) const ;

  unsigned int GetNumberOfInstances() const ;

  MeasurementVectorType& GetMeasurementVector(const InstanceIdentifier &id) ;

  void SetMeasurement(const InstanceIdentifier &id, 
                      const unsigned int &dim,
                      const MeasurementType &value) ;

  FrequencyType GetFrequency(const InstanceIdentifier &id) const ;

  FrequencyType GetTotalFrequency(const unsigned int &dimension) const ;

  class Iterator;
  friend class Iterator;
  
  Iterator Begin()
  { 
    Iterator iter(m_PointsContainer->Begin());
    return iter; 
  }
  
  Iterator End()        
  {
    Iterator iter(m_PointsContainer->End()); 
    return iter; 
  }
  
  class Iterator
  {
  public:
    
    Iterator(){}
    
    Iterator(PointsContainerIterator iter)
      :m_Iter(iter)
    {}
    
    FrequencyType GetFrequency() const
    { return 1 ;}

    MeasurementVectorType& GetMeasurementVector()
    { return (MeasurementVectorType&) m_Iter.Value() ;} 

    InstanceIdentifier GetInstanceIdentifier() const
    { return m_Iter.Index() ;}

    Iterator& operator++()
    { ++m_Iter ; return *this ;}
    
    Iterator& operator--()
    { --m_Iter ; return *this ;}

    bool operator!=(const Iterator &it)
    { return (m_Iter != it.m_Iter) ;}
    
    bool operator==(const Iterator &it)
    { return (m_Iter == it.m_Iter) ;}
    
    Iterator& operator = (const Iterator &iter)
    { 
      m_Iter = iter.m_Iter; 
      return iter ;
    }

    Iterator(const Iterator &iter)
    { m_Iter = iter.m_Iter; }
    
  private:
    PointsContainerIterator m_Iter ;
  } ;
  
protected:
  PointSetToListAdaptor() {}
  virtual ~PointSetToListAdaptor() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

private:
  PointSetToListAdaptor(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  PointSetPointer m_PointSet ;
  PointsContainerPointer m_PointsContainer ;
  PointType m_TempPoint ;
} ; // end of class PointSetToListAdaptor

} // end of namespace Statistics
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToListAdaptor.txx"
#endif

#endif
