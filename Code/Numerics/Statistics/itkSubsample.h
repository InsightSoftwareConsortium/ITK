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
      public Sample< typename TSample::MeasurementType, 
                     TSample::MeasurementVectorSize >
{
public:
  /** Standard class typedefs */
  typedef Subsample Self;
  typedef Sample< typename TSample::MeasurementType, 
                  TSample::MeasurementVectorSize > Superclass ;
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
  typedef typename TSample::SizeType SizeType ;
  typedef typename TSample::SizeValueType SizeValueType ;

  /** MeasurementVectorSize enum from super class */
  enum { MeasurementVectorSize = TSample::MeasurementVectorSize } ;

  /** Type of the storage for instances that belong to the class 
   * represented by a Subsample object. A Subsample object stores
   * only the InstanceIdentifiers. The actual data is still in the Sample
   * object */
  typedef std::vector< InstanceIdentifier > InstanceIdentifierHolder ;

  /** Plug in the actual sample data */
  void SetSample(SamplePointer sample)
  { m_Sample = sample ; }

  SamplePointer GetSample()
  { return m_Sample ; } 

  void AddInstance(InstanceIdentifier id)
  { m_IdHolder.push_back(id) ; }

  /** returns SizeType object whose each element is the number of
   * elements in each dimension */
  SizeType GetSize()
  { 
    SizeType size ;
    for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
      {
        size[i] = m_IdHolder.size() ;
      }
    return size ;
  }
  
  size_t GetNumberOfInstances() ;

  /** returns SizeValueType value that is the number of elements in the
   * 'dimension' dimension. */
  SizeValueType GetSize(unsigned int dimension) 
  { return m_IdHolder.size() ; }

  /** retunrs the measurement of the instance which is identified 
   * by the 'id' */
  MeasurementVectorType GetMeasurementVector(const InstanceIdentifier id)
  { return m_Sample->GetMeasurementVector(id) ; }

  /** returns the frequency of the instance which is identified by the 'id' */
  FrequencyType GetFrequency(const InstanceIdentifier id)
  { return m_Sample->GetFrequency(id) ; }

  /** returns the measurement element which is the 'n'-th element 
   * in the 'd' dimension of the measurement vector */
  MeasurementType GetMeasurement(const unsigned int d, const unsigned long n) 
  { return m_Sample->GetMeasurement(d, n) ; }
  
  /** returns the frequency of the 'n'-th element in the 'd' dimension  
   * of the measurement vector */
  FrequencyType GetFrequency(const unsigned int d, const unsigned long n)
  { return m_Sample->GetFrequency(d, n) ; }

  /** returns the total frequency for the 'd' dimension */
  FrequencyType GetTotalFrequency(const unsigned int d)
  { return m_Sample->GetTotalFrequency(d) ; }
  
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
    Iterator(typename InstanceIdentifierHolder::iterator iter, Pointer classSample)
      :m_Iter(iter), m_Subsample(classSample),
       m_Sample(classSample->GetSample())
    {}
    
    const FrequencyType GetFrequency() 
    { return  m_Sample->GetFrequency(*m_Iter) ; }
    
    MeasurementVectorType GetMeasurementVector()
    { return m_Sample->GetMeasurementVector(*m_Iter) ; } 
    
    MeasurementType GetMeasurement(int dim)
    { return m_Sample->GetMeasurement(dim, *m_Iter) ; }
    
    InstanceIdentifier GetInstanceIdentifier()   
    { return *m_Iter ; }
    
    Iterator& operator++() 
    { 
      ++m_Iter ;
      return *this ;
    }
    
    bool operator!=(const Iterator& it) 
    { return (m_Iter != it.m_Iter) ; }
    
    bool operator==(const Iterator& it) 
    { return (m_Iter == it.m_Iter) ; }
    
    Iterator& operator=(const Iterator& iter)
    {
      m_Iter = iter.m_Iter;
      m_Subsample = iter.m_Subsample ;
      m_Sample = iter.m_Sample ;
    }
    
  private:
    // Iterator pointing to ImageListSampleAdaptor
    typename InstanceIdentifierHolder::iterator m_Iter ;  
    // Pointer to Subsample object
    Pointer m_Subsample ;
    SamplePointer m_Sample ;
  } ;

protected:
  Subsample() ;
  virtual ~Subsample() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  Subsample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  SamplePointer m_Sample ;
  InstanceIdentifierHolder m_IdHolder ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSubsample.txx"
#endif

#endif







