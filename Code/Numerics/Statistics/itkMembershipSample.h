/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

template< class TSample >
class ITK_EXPORT MembershipSample : 
      public Sample< typename TSample::MeasurementType, 
                     TSample::MeasurementVectorSize >
{
public:
  /*@{ Standard class typedefs. */
  typedef MembershipSample Self;
  typedef Sample< typename TSample::MeasurementType, 
                  TSample::MeasurementVectorSize > Superclass ;
  typedef SmartPointer< Self > Pointer ;
  //@}

  /*@{ Standard macros */ 
  itkTypeMacro(MembershipSample, Sample);
  itkNewMacro(Self) ;
  //@}

  /** Smart pointer to the actual sample data holder */
  typedef typename TSample::Pointer SamplePointer ;

  /** Typedef for the storage that holds a class label for each instance.
   * The relationship between instances and class label is one-to-onw */
  typedef std::map< InstanceIdentifier, unsigned int > ClassLabelHolder ;

  /** Typedef for each subsample that stores instance identifers of instances
   * that belong to a class */
  typedef Subsample< TSample > ClassSampleType ;
  typedef ClassSampleType::Pointer ClassSamplePointer ;

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
    Iterator(ClassLabelHolder::iterator iter, Pointer membershipSample)
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
    ClassLabelHolder::iterator m_Iter ;  
    // Pointer to MemebershipSample object
    Pointer m_MemebershipSample ;
    SamplePointer m_Sample ;
  } ;

protected:
  MembershipSample() ;
  virtual ~MembershipSample() {}
  MembershipSample(const Self&) {}
  void operator=(const Self&) {}
  
private:
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







