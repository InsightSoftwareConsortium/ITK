/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMembershipSample_h
#define __itkMembershipSample_h

#include <map>
#include <set>

#include "itkSample.h"
#include "itkSubsample.h"

#include "itkExceptionObject.h"

namespace itk{ 
  namespace Statistics{

/** \class MembershipSample
 * \brief Container for storing the instance-identifiers of other sample with 
 * their associated class labels.
 *
 * This class does not store any measurement data. In a sense, you can
 * think it as an additional information to basic samples (such as Histogram,
 * PointSetListSampleAdaptor, and ImageListSampleAdaptor). The additional
 * information is a class label for a measurement vector. Obviously without
 * such basic types of sample, this one is meaningless. You can call any
 * basic methods that has been defined in the Sample class such as 
 * GetMeasurementVector and GetFrequency. You can query the class label for
 * an instance using an instance-identifier. Another new and important method 
 * is the GetClassSample method. With a given class label, it returns a
 * pointer to the Subsample object that has all the instance-identifiers 
 * of instances that belong to the class.
 *
 * This class is templated over the type of the basic sample. To use all
 * the method, you should first plug in a basic type sample using the
 * SetSample method
 */
 
template< class TSample >
class ITK_EXPORT MembershipSample : 
      public Sample< typename TSample::MeasurementType, 
                     TSample::MeasurementVectorSize >
{
public:
  /** Standard class typedefs. */
  typedef MembershipSample Self;
  typedef Sample< typename TSample::MeasurementType, 
                  TSample::MeasurementVectorSize > Superclass ;
  typedef SmartPointer< Self > Pointer ;

  /** Standard macros */ 
  itkTypeMacro(MembershipSample, Sample);
  itkNewMacro(Self) ;

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

  /** Smart pointer to the actual sample data holder */
  typedef typename TSample::Pointer SamplePointer ;

  /** Typedef for the storage that holds a class label for each instance.
   * The relationship between instances and class label is one-to-onw */
  typedef std::map< InstanceIdentifier, unsigned int > ClassLabelHolder ;

  /** Typedef for each subsample that stores instance identifers of instances
   * that belong to a class */
  typedef Subsample< TSample > ClassSampleType ;
  typedef typename ClassSampleType::Pointer ClassSamplePointer ;

  /** Plug in the actual sample data */
  void SetSample(SamplePointer sample) ;

  SamplePointer GetSample() ;

  unsigned int GetNumberOfClasses() ;

  bool ClassLabelExists(unsigned int classLabel) ;

  inline void AddInstance(unsigned int classLabel, InstanceIdentifier id) ;

  inline unsigned int GetClassLabel(InstanceIdentifier id) 
    throw (ExceptionObject) ;

  ClassSamplePointer GetClassSample(unsigned int classLabel) ;

  /** returns SizeType object whose each element is the number of
   * elements in each dimension */
  SizeType GetSize() ;
  
  /** returns SizeValueType value that is the number of elements in the
   * 'dimension' dimension. */
  SizeValueType GetSize(unsigned int dimension) ;

  /** retunrs the measurement of the instance which is identified by the 'id' */
  inline MeasurementVectorType GetMeasurementVector(const InstanceIdentifier 
                                                    id) ;

  /** returns the frequency of the instance which is identified by the 'id' */
  inline FrequencyType GetFrequency(const InstanceIdentifier id) ;

  /** returns the measurement element which is the 'n'-th element 
   * in the 'd' dimension of the measurement vector */
  inline MeasurementType GetMeasurement(const unsigned int d, 
                                        const unsigned long n) ;

  /** returns the frequency of the 'n'-th element in the 'd' dimension 
   * of the measurement vector */
  inline FrequencyType GetFrequency(const unsigned int d, 
                                    const unsigned long n) ;

  /** returns the total frequency for the 'd' dimension */
  FrequencyType GetTotalFrequency(const unsigned int d) ;
  
  class Iterator;
  friend class Iterator;
  
  Iterator Begin()
  { 
    Iterator iter(m_ClassLabelHolder.begin(), this) ;
    return iter; 
  }
  
  Iterator  End()        
  {
    Iterator iter(m_ClassLabelHolder.end(), this) ; 
    return iter; 
  }
  
  class Iterator
  {
  public:
    Iterator(typename ClassLabelHolder::iterator iter, Pointer membershipSample)
      :m_Iter(iter), m_MemebershipSample(membershipSample),
       m_Sample(membershipSample->GetSample())
    {}
    
    const FrequencyType GetFrequency() 
    { return  m_Sample->GetFrequency(m_Iter->first) ; }
    
    MeasurementVectorType GetMeasurementVector()
    { return m_Sample->GetMeasurementVector(m_Iter->first) ; } 
    
    MeasurementType GetMeasurement(int dim)
    { return m_Sample->GetMeasurement(dim, m_Iter->first) ; }
    
    InstanceIdentifier GetInstanceIdentifier()   
    { return m_Iter->first ; }

    unsigned int GetClassLabel()
    { return m_Iter->second ; }

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
      m_MemebershipSample = iter.m_MemebershipSample ;
      m_Sample = iter.m_Sample ;
    }
    
  private:
    // Iterator pointing to ImageListSampleAdaptor
    typename ClassLabelHolder::iterator m_Iter ;  
    // Pointer to MemebershipSample object
    Pointer m_MemebershipSample ;
    SamplePointer m_Sample ;
  } ;

protected:
  MembershipSample() ;
  virtual ~MembershipSample() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  
  
private:
  MembershipSample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  SamplePointer m_Sample ;
  unsigned int m_CurrentClassLabel ;
  ClassLabelHolder m_ClassLabelHolder ;
  std::set< unsigned int > m_ClassLabels ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMembershipSample.txx"
#endif

#endif







