/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSubsample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSubsample_h
#define __itkSubsample_h

#include "itkSample.h"

namespace itk{ 
  namespace Statistics{

template< class TSample >
class ITK_EXPORT Subsample : 
      public Sample< typename TSample::MeasurementVectorType >
{
public:
  /** Standard class typedefs */
  typedef Subsample Self;
  typedef Sample< typename TSample::MeasurementVectorType > Superclass ;
  typedef SmartPointer< Self > Pointer ;

  /** Run-time type information (and related methods).*/
  itkTypeMacro(Subsample, Sample);

  /** standard New() method support */
  itkNewMacro(Self) ;

  /** Smart pointer to the actual sample data holder */
  typedef typename TSample::Pointer SamplePointer ;
  
  /** Typedefs for Measurement vector, measurement, Instance Identifier, 
   * frequency, size, size element value from the template argument TSample*/
  typedef typename TSample::MeasurementVectorType MeasurementVectorType;
  typedef typename TSample::MeasurementType MeasurementType;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier;
  typedef typename TSample::FrequencyType FrequencyType ;
  typedef MeasurementVectorType ValueType ;
//    typedef typename TSample::SizeType SizeType ;
//    typedef typename TSample::SizeValueType SizeValueType ;

  /** MeasurementVectorSize constant from super class */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  /** Type of the storage for instances that belong to the class 
   * represented by a Subsample object. A Subsample object stores
   * only the InstanceIdentifiers. The actual data is still in the Sample
   * object */
  typedef std::vector< InstanceIdentifier > InstanceIdentifierHolder ;

  /** Plug in the actual sample data */
  void SetSample(TSample* sample)
  { m_Sample = sample ; }

  TSample* GetSample()
  { return m_Sample ; } 

  void InitializeWithAllInstances()
  {
    m_IdHolder.resize(m_Sample->Size()) ;
    typename InstanceIdentifierHolder::iterator idIter = m_IdHolder.begin() ;
    typename TSample::Iterator iter = m_Sample->Begin() ;
    typename TSample::Iterator last = m_Sample->End() ;
    m_TotalFrequency = NumericTraits< FrequencyType >::Zero ;
    while (iter != last)
      {
        *idIter++ = iter.GetInstanceIdentifier() ;
        m_TotalFrequency += iter.GetFrequency() ;
        ++iter ;
      }
  }

  void AddInstance(InstanceIdentifier id)
  { 
    m_IdHolder.push_back(id) ; 
    m_TotalFrequency += m_Sample->GetFrequency(id) ;
  }

  /** returns SizeType object whose each element is the number of
   * elements in each dimension */
  unsigned int Size() const
  { 
    return static_cast<unsigned int>( m_IdHolder.size() );
  }

  void Clear()
  { 
    m_IdHolder.clear() ;
    m_TotalFrequency = NumericTraits< FrequencyType >::Zero ;
  }

  /** retunrs the measurement of the instance which is identified 
   * by the 'id' */
  MeasurementVectorType& GetMeasurementVector(const InstanceIdentifier &id)
  { return m_Sample->GetMeasurementVector(id) ; }

  /** returns the frequency of the instance which is identified by the 'id' */
  FrequencyType GetFrequency(const InstanceIdentifier &id) const
  { return m_Sample->GetFrequency(id) ; }
  
  /** returns the total frequency for the 'd' dimension */
  FrequencyType GetTotalFrequency() const
  { return m_TotalFrequency ; }
  
  void Swap(int index1, int index2) ;
  
  MeasurementVectorType& GetMeasurementVectorByIndex(int index) ;
  
  InstanceIdentifier GetInstanceIdentifier(int index) ;

  class Iterator;
  friend class Iterator;
  
  Iterator Begin()
  { 
    Iterator iter(m_IdHolder.begin(), this) ;
    return iter; 
  }
  
  Iterator  End()        
  {
    Iterator iter(m_IdHolder.end(), this) ; 
    return iter; 
  }
  
  class Iterator
  {
  public:
    Iterator(typename InstanceIdentifierHolder::iterator iter, 
             Self* classSample)
      :m_Iter(iter), m_Subsample(classSample),
       m_Sample(classSample->GetSample())
    {}
    
    FrequencyType GetFrequency() const
    { return  m_Sample->GetFrequency(*m_Iter) ; }
    
    MeasurementVectorType& GetMeasurementVector() 
    { return m_Sample->GetMeasurementVector(*m_Iter) ; } 
    
    InstanceIdentifier GetInstanceIdentifier() const   
    { return *m_Iter ; }
    
    Iterator& operator++() 
    { 
      ++m_Iter ;
      return *this ;
    }
    
    Iterator& operator+()
    { m_Iter += n; return *this ;}

    Iterator& operator+(int n)
    { m_Iter += n; return *this ;}
    
    Iterator& operator-(int n)
    { m_Iter -= n; return *this ;}

    bool operator!=(const Iterator& it) 
    { return (m_Iter != it.m_Iter) ; }
    
    bool operator==(const Iterator& it) 
    { return (m_Iter == it.m_Iter) ; }
    
    Iterator& operator=(const Iterator& iter)
    {
      m_Iter = iter.m_Iter;
      m_Subsample = iter.m_Subsample ;
      m_Sample = iter.m_Sample ;
      return *this ;
    }

    Iterator(const Iterator& iter)
    {
      m_Iter = iter.m_Iter;
      m_Subsample = iter.m_Subsample ;
      m_Sample = iter.m_Sample ;
    }
    
  private:
    // Iterator pointing to ImageToListAdaptor
    typename InstanceIdentifierHolder::iterator m_Iter ;  
    // Pointer to Subsample object
    Self* m_Subsample ;
    TSample* m_Sample ;
  } ;

protected:
  Subsample() ;
  virtual ~Subsample() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  Subsample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  TSample*               m_Sample ;
  InstanceIdentifierHolder    m_IdHolder ;
  unsigned int                m_ActiveDimension ;
  FrequencyType m_TotalFrequency ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSubsample.txx"
#endif

#endif







