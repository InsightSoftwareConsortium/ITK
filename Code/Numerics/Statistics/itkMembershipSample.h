/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * 
 * Recent API changes:
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. Please use the function 
 * GetMeasurementVectorSize() instead.
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
  typedef SmartPointer< const Self > ConstPointer ;

  /** Standard macros */ 
  itkTypeMacro(MembershipSample, Sample);
  itkNewMacro(Self) ;
  
  /** Typedefs for Measurement vector, measurement, Instance Identifier, 
   * frequency, size, size element value from the template argument TSample*/
  typedef typename TSample::MeasurementVectorType MeasurementVectorType;
  typedef typename TSample::MeasurementType MeasurementType;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier;
  typedef typename TSample::FrequencyType FrequencyType ;
  
  
  /** vector of unique class labels that will be used for mapping internal
   * continuous class label with real class labels */
  typedef std::vector< unsigned int > UniqueClassLabelsType ;

  /** Typedef for the storage that holds a class label for each instance.
   * The relationship between instances and class label is one-to-one */
  typedef itk::hash_map< InstanceIdentifier, unsigned int> ClassLabelHolderType ;
  
  /** Typedef for each subsample that stores instance identifers of instances
   * that belong to a class */
  typedef Subsample< TSample > ClassSampleType ;
  typedef typename ClassSampleType::Pointer         ClassSamplePointer;
  typedef typename ClassSampleType::ConstPointer    ClassSampleConstPointer;
  
  /** Plug in the actual sample data */
  void SetSample(const TSample* sample) ;

  /** Returns the source sample pointer */
  const TSample* GetSample() const;
  
  /** Sets the number of classes (class labels) */
  void SetNumberOfClasses(unsigned int numberOfClasses) ;
  
  /** Gets the number of classes (class labels) */
  unsigned int GetNumberOfClasses() const ;

  /** Adds an instance from the source sample to this container. The
   * first argument is the class label for that instance. The second
   * argument is the instance identifier from the source identifier that
   * is going to be included this container. */
  void AddInstance(const unsigned int &classLabel, const InstanceIdentifier &id) ;
  /** Gets the class label for the instance that has the instance
   *   identifier, id. */
  unsigned int GetClassLabel(const InstanceIdentifier &id) const ;

  /** Gets the internal continuous class label from the class labels that
   *   are used for AddInstance method. */ 
  int GetInternalClassLabel(const unsigned int classLabel ) const ;

  /** Gets the number of instances that belong to the class label in
   *   this container */
  unsigned int GetClassSampleSize(const unsigned int &classLabel) const ;

  /** Gets the Subsample that includes only the instances that belongs
   *   to the classLabel */
  const ClassSampleType* GetClassSample(const unsigned int &classLabel) const ;
  
  /** Gets the class labels that corresponding to the each instance in
   *   this container. */
  ClassLabelHolderType* GetClassLabels()
  { return &m_ClassLabelHolder ; }

  /** returns the number of elements in each dimension */
  unsigned int Size(void) const ;
  
  /** retunrs the measurement of the instance which is identified 
   * by the 'id' */
  const MeasurementVectorType & GetMeasurementVector(const InstanceIdentifier &id) const;
  
  /** returns the measurement element which is the 'n'-th element 
   * in the 'd' dimension of the measurement vector */
  MeasurementType GetMeasurement(const InstanceIdentifier &id, 
                                  const unsigned int &dimension) ;

  /** returns the frequency of the instance which is identified by the 'id' */
  FrequencyType GetFrequency(const InstanceIdentifier &id) const ;
  
  /** returns the total frequency for the 'd' dimension */
  FrequencyType GetTotalFrequency() const ;

  void Resize(unsigned int n) 
  {
    m_ClassLabelHolder.resize(n) ;
  }

 
  class ConstIterator
  {
  public:
    ConstIterator(InstanceIdentifier id, const Self* membershipSample)
      :m_Id(id), m_MembershipSample(membershipSample),
       m_Sample(membershipSample->GetSample())
    {}
    
    FrequencyType GetFrequency() const
    { return  m_Sample->GetFrequency(m_Id) ; }
    
    const MeasurementVectorType & GetMeasurementVector() const
    { return m_Sample->GetMeasurementVector(m_Id) ; } 
    
    InstanceIdentifier GetInstanceIdentifier() const
    { return m_Id ; }

    void SetClassLabel(unsigned int classLabel)
    { m_MembershipSample->AddInstance(classLabel, m_Id) ; }

    unsigned int GetClassLabel() const
    { return m_MembershipSample->GetClassLabel(m_Id) ; }

    ConstIterator& operator++() 
    { 
      ++m_Id ;
      return *this ;
    }
    
    bool operator!=(const ConstIterator& it) 
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

    bool operator==(const ConstIterator& it) 
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
    
    ConstIterator& operator=(const ConstIterator& it)
    {
      m_Id = it.m_Id;
      m_MembershipSample = it.m_MembershipSample ;
      m_Sample = it.m_Sample ;
      return *this ;
    }

    ConstIterator(const ConstIterator& it)
    {
      m_Id = it.m_Id;
      m_MembershipSample = it.m_MembershipSample ;
      m_Sample = it.m_Sample ;
    }
    
  private:
    // identifier for the instance
    InstanceIdentifier m_Id ;  
    // Pointer to MemebershipSample object
    const Self* m_MembershipSample ;
    const TSample* m_Sample ;
  } ;

  ConstIterator Begin() const
  { 
    ConstIterator iter(0, this) ;
    return iter; 
  }
  
  ConstIterator  End() const        
  {
    ConstIterator iter(this->Size(), this) ; 
    return iter; 
  }
 
protected:
  MembershipSample() ;
  virtual ~MembershipSample() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  
  
private:
  MembershipSample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  const TSample*                  m_Sample ;
  unsigned int                    m_CurrentClassLabel ;
  UniqueClassLabelsType           m_UniqueClassLabels ;
  ClassLabelHolderType            m_ClassLabelHolder ;
  unsigned int                    m_NumberOfClasses ;
  std::vector< unsigned int >     m_ClassSampleSizes ;
  std::vector< ClassSamplePointer > m_ClassSamples ;
} ; // end of class


} // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMembershipSample.txx"
#endif

#endif







