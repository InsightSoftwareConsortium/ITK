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

#include "itk_hash_map.h"
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
 * PointSetListSampleAdaptor, and ImageToListAdaptor). The additional
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
    public Sample< typename TSample::MeasurementVectorType >
{
public:
  /** Standard class typedefs. */
  typedef MembershipSample Self;
  typedef Sample< typename TSample::MeasurementVectorType > Superclass ;
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
  //    typedef typename TSample::SizeType SizeType ;
  //    typedef typename TSample::SizeValueType SizeValueType ;
  
  /** MeasurementVectorSize enum from super class */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);
  
  /** vector of unique class labels that will be used for mapping internal
   * continuous class label with real class labels */
  typedef std::vector< unsigned int > UniqueClassLabelsType ;

  /** Typedef for the storage that holds a class label for each instance.
   * The relationship between instances and class label is one-to-one */
  typedef itk::hash_map< InstanceIdentifier, unsigned int> ClassLabelHolderType ;
  
  /** Typedef for each subsample that stores instance identifers of instances
   * that belong to a class */
  typedef Subsample< TSample > ClassSampleType ;
  
  /** Plug in the actual sample data */
  void SetSample(TSample* sample) ;
  
  TSample* GetSample() ;
  
  void SetNumberOfClasses(unsigned int numberOfClasses) ;
  
  unsigned int GetNumberOfClasses() const ;

  void AddInstance(const unsigned int &classLabel, const InstanceIdentifier &id) ;
  
  unsigned int GetClassLabel(const InstanceIdentifier &id) const ;

  int GetInternalClassLabel(const unsigned int classLabel ) const ;

  unsigned int GetClassSampleSize(const unsigned int &classLabel) const ;

  ClassSampleType* GetClassSample(const unsigned int &classLabel) ;
  
  ClassLabelHolderType* GetClassLabels()
  { return &m_ClassLabelHolder ; }

  /** returns the number of elements in each dimension */
  unsigned int Size(void) const ;
  
  /** retunrs the measurement of the instance which is identified 
   * by the 'id' */
  MeasurementVectorType& GetMeasurementVector(const InstanceIdentifier &id) ;
  
  /** returns the measurement element which is the 'n'-th element 
   * in the 'd' dimension of the measurement vector */
  MeasurementType& GetMeasurement(const InstanceIdentifier &id, 
                                  const unsigned int &dimension) ;

  /** returns the frequency of the instance which is identified by the 'id' */
  FrequencyType GetFrequency(const InstanceIdentifier &id) const ;
  
  /** returns the total frequency for the 'd' dimension */
  FrequencyType GetTotalFrequency() const ;

  void Resize(unsigned int n) 
  {
    m_ClassLabelHolder.resize(n) ;
  }

  class Iterator;
  friend class Iterator;
  
  Iterator Begin()
  { 
    Iterator iter(0, this) ;
    return iter; 
  }
  
  Iterator  End()        
  {
    Iterator iter(this->Size(), this) ; 
    return iter; 
  }
  
  class Iterator
  {
  public:
    Iterator(InstanceIdentifier id, Self* membershipSample)
      :m_Id(id), m_MembershipSample(membershipSample),
       m_Sample(membershipSample->GetSample())
    {}
    
    FrequencyType GetFrequency() const
    { return  m_Sample->GetFrequency(m_Id) ; }
    
    MeasurementVectorType& GetMeasurementVector()
    { return m_Sample->GetMeasurementVector(m_Id) ; } 
    
    InstanceIdentifier GetInstanceIdentifier() const
    { return m_Id ; }

    unsigned int GetClassLabel() const
    { return m_MembershipSample->GetClassLabel(m_Id) ; }

    Iterator& operator++() 
    { 
      ++m_Id ;
      return *this ;
    }
    
    bool operator!=(const Iterator& it) 
    { 
      if (m_Id != it.m_Id || 
          m_MembershipSample != it.m_MembershipSample ||
          m_Sample != it.m_Sample)
        {
          return true ;
        }
      else
        {
          return false ;
        }
    }

    bool operator==(const Iterator& it) 
    { 
      if (m_Id == it.m_Id && 
          m_MembershipSample == it.m_MembershipSample &&
          m_Sample == it.m_Sample)
        {
          return true ;
        }
      else
        {
          return false ;
        }
    }
    
    Iterator& operator=(const Iterator& it)
    {
      m_Id = it.m_Id;
      m_MembershipSample = it.m_MembershipSample ;
      m_Sample = it.m_Sample ;
      return *this ;
    }

    Iterator(const Iterator& it)
    {
      m_Id = it.m_Id;
      m_MembershipSample = it.m_MembershipSample ;
      m_Sample = it.m_Sample ;
    }
    
  private:
    // identifier for the instance
    InstanceIdentifier m_Id ;  
    // Pointer to MemebershipSample object
    Self* m_MembershipSample ;
    TSample* m_Sample ;
  } ;

protected:
  MembershipSample() ;
  virtual ~MembershipSample() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  
  
private:
  MembershipSample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  TSample*                        m_Sample ;
  unsigned int                    m_CurrentClassLabel ;
  UniqueClassLabelsType           m_UniqueClassLabels ;
  ClassLabelHolderType            m_ClassLabelHolder ;
  unsigned int                    m_NumberOfClasses ;
  std::vector< unsigned int >     m_ClassSampleSizes ;
  std::vector< typename ClassSampleType::Pointer > m_ClassSamples ;
} ; // end of class


} // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMembershipSample.txx"
#endif

#endif







