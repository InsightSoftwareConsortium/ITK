/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSubsample.h
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
 /**
  * Standard "Self" typedef.
  */
  typedef Subsample Self;

  /**
   * Standard smart pointer typedef
   */
  typedef SmartPointer< Self > Pointer ;

 /**
  * Standard Superclass typedef
  */
  typedef Sample< typename TSample::MeasurementType, 
                  TSample::MeasurementVectorSize > Superclass ;

 /** 
  * Run-time type information (and related methods).
  */
  itkTypeMacro(Subsample, Sample);

  /**
   * standard New() method support
   */
  itkNewMacro(Self) ;

  /**
   * Smart pointer to the actual sample data holder
   */
  typedef typename TSample::Pointer SamplePointer ;

  /**
   * Type of the storage for instances that belong to the class
   * represented by a Subsample object. A Subsample object stores
   * only the InstanceIdentifiers. The actual data is still in the Sample
   * object
   */
  typedef std::vector< InstanceIdentifier > InstanceIdentifierHolder ;

  /**
   * Plug in the actual sample data
   */
  void SetSample(SamplePointer sample)
  { m_Sample = sample ; }

  SamplePointer GetSample()
  { return m_Sample ; } 

  void AddInstance(InstanceIdentifier id)
  { m_IdHolder.push_back(id) ; }

  /**
   * returns SizeType object whose each element is the number of
   * elements in each dimension
   */
  SizeType GetSize()
  { 
    SizeType size ;
    for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
      {
        size[i] = m_IdHolder.size() ;
      }
    return size ;
  }
  
  /**
   * returns SizeValueType value that is the number of elements in the
   * 'dimension' dimension.
   */
  SizeValueType GetSize(unsigned int dimension) 
  { return m_IdHolder.size() ; }

  /**
   * retunrs the measurement of the instance which is identified by the 'id'
   */
  MeasurementVectorType GetMeasurementVector(const InstanceIdentifier id)
  { return m_Sample->GetMeasurementVector(id) ; }

  /**
   * returns the frequency of the instance which is identified by the 'id'
   */
  FrequencyType GetFrequency(const InstanceIdentifier id)
  { return m_Sample->GetFrequency(id) ; }

  /**
   * returns the measurement element which is the 'n'-th element 
   * in the 'd' dimension of the measurement vector
   */
  MeasurementType GetMeasurement(const unsigned int d, const unsigned long n) 
  { return m_Sample->GetMeasurement(d, n) ; }
  
  /**
   * returns the frequency of the 'n'-th element in the 'd' dimension 
   * of the measurement vector
   */
  FrequencyType GetFrequency(const unsigned int d, const unsigned long n)
  { return m_Sample->GetFrequency(d, n) ; }

  /**
   * returns the total frequency for the 'd' dimension
   */
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
    Iterator(InstanceIdentifierHolder::iterator iter, Pointer classSample)
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
    InstanceIdentifierHolder::iterator m_Iter ;  
    // Pointer to Subsample object
    Pointer m_Subsample ;
    SamplePointer m_Sample ;
  } ;

protected:
  Subsample() ;
  virtual ~Subsample() {}
  Subsample(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  SamplePointer m_Sample ;
  InstanceIdentifierHolder m_IdHolder ;
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSubsample.txx"
#endif

#endif







