/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogram.h
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
#ifndef __itkHistogram_h
#define __itkHistogram_h

#include <vector>

#include "itkSize.h"
#include "itkIndex.h"
#include "itkExceptionObject.h"
#include "itkSample.h"
#include "itkDenseFrequencyContainer.h"
#include "itkSparseFrequencyContainer.h"

namespace itk{
  namespace Statistics{

/** \class Histogram 
 *  \brief This class stores measurement vectors in the context of n-dimensional
 *  histogram.
 *
 * Users can set arbitrary value for each bins min max value for each 
 * dimension (variable interval). Each dimension of the histogram represents
 * each dimension of measurement vectors. For example, an Image with each pixel
 * having a gray level intensity value and a gradient magnitude can be imported
 * to this class. Then the resulting Histogram has two dimensions, intensity
 * and gradient magnitude.
 *
 * Before any operation, users have to call Initialize(SizeType) method to
 * prepare the indexing mechanism and the internal frequency container.
 * After this, users want to set range of each bin using 
 * SetBinMin(dimension, n) and SetBinMax(dimension, n) methods.
 * 
 * The first two template arguments are same as those of 
 * Sample. The last one, "TFrequencyContainter", is 
 * the type of the internal frequency container. If you think your Histogram
 * is dense, in other words, almost every bin is used, then use default.
 * If you expect that a very little portion of bins will be used, replace it
 * with SparseFrequencyContainer class
 * 
 * Since this class is n-dimensional, it supports data access
 * methods using ITK Index type in addition to the methods using 
 * "InstanceIdentifiers".
 *
 * \sa Sample, DenseFrequencyContainer, 
 * SparseFrequencyContainer
 */

template < class TMeasurement = float, unsigned int VMeasurementVectorSize = 1,
           class TFrequencyContainer = 
           DenseFrequencyContainer< float > > 
class ITK_EXPORT Histogram 
  : public Sample < TMeasurement, VMeasurementVectorSize >
{
public:
  /** Standard "Self" typedef. */
  typedef Histogram  Self ;

  /** Standard Superclass typedef */
  typedef Sample< TMeasurement, VMeasurementVectorSize > Superclass ;
  
  /** Smart pointer typedef support  */
  typedef SmartPointer<Self> Pointer ;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Histogram, Sample) ;

  /** standard New() method support */
  itkNewMacro(Self) ;

  /** Dimension of a measurement vector */
  enum { MeasurementVectorSize = VMeasurementVectorSize } ;
 
  /** type of an element of a measurement vector ; */
  typedef TMeasurement MeasurementType ;

  /** Common sample class typedefs */
  typedef Superclass::MeasurementVectorType MeasurementVectorType ;
  typedef Superclass::SizeType SizeType ;
  typedef Superclass::InstanceIdentifier InstanceIdentifier ;

  /** frequency container typedef */
  typedef TFrequencyContainer FrequencyContainerType ;
  typedef typename FrequencyContainerType::Pointer FrequencyContainerPointer ;

  /** Frequency value type from superclass */
  typedef typename FrequencyContainerType::FrequencyType FrequencyType ;

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index< MeasurementVectorSize >  IndexType;
  typedef typename IndexType::IndexValueType  IndexValueType;

  /** bin min max value storage types */
  typedef std::vector< MeasurementType > BinMinVectorType ;
  typedef std::vector< MeasurementType > BinMaxVectorType ;
  typedef std::vector< BinMinVectorType > BinMinContainerType ;
  typedef std::vector< BinMaxVectorType > BinMaxContainerType ;
  
  /** generates the offset table.
   * subclasses should call this method in their initialize() method
   * the overide methods have prepare the frequency container for
   * input and output. */
   void Initialize(SizeType size) ;

  /** returns the index of histogram corresponding to measurement value */
   IndexType GetIndex(const MeasurementVectorType measurement) 
     throw (RangeError) ;

  /** returns the index that is uniquely labelled by an instance identifier
   * The corresponding id is the offset of the index 
   * This method uses ImageBase::ComputeIndex() method */
  IndexType GetIndex(const InstanceIdentifier id)  ;

  /** returns the instance identifier of the cell that is indexed by the 
   * index. The corresponding instance identifier is the offset of the index 
   * This method uses ImageBase::ComputeIndex() method */
  InstanceIdentifier GetInstanceIdentifier(const IndexType index)  ;
  
  /** Method to get m_Size */
  SizeType GetSize() { return m_Size ; }

  /** return the size of each dimension of the measurement vector container */
  SizeValueType GetSize(unsigned int dimension) 
  { return m_Size[dimension] ; }

  /** Method to get minimum value of n th bin of dimension d */
  MeasurementType GetBinMin(unsigned int dimension, 
                               unsigned long nbin) 
  { return m_Min[dimension][nbin] ; }
  
  /** Method to get maximum value of n th bin of dimension d */
  MeasurementType GetBinMax(unsigned int dimension,
                               unsigned long nbin) 
  { return m_Max[dimension][nbin] ; }
  
  /** Method to set minimum value of n th bin of dimension d */
  void SetBinMin(unsigned int dimension, unsigned long nbin, float min)
  { m_Min[dimension][nbin] = min ; }
  
  /** Method to set maximum value of n th bin of dimension d */
  void SetBinMax(unsigned int dimension, 
                 unsigned long nbin, float max)
  { m_Max[dimension][nbin] = max ; }
  
  /** Method to get the minimum of the bin corresponding to the gray level of 
   * dimension d. */
  MeasurementType GetBinMinFromValue(unsigned int dimension, 
                                        const float value )  ;
  
  /** Method to get the maximum of the bin corresponding to the gray level of 
   * dimension d. */
  MeasurementType GetBinMaxFromValue(unsigned int dimension, 
                                        const float value )  ;
  
  /** Method to get the minimum vector of a dimension  */
  BinMinVectorType& GetDimensionMins(unsigned int dimension)
  { return m_Min[dimension] ; }
  
  /** Method to get the maximum vector of a dimension  */
  BinMaxVectorType& GetDimensionMaxs(unsigned int dimension)
  {  return m_Max[dimension] ; }
  
  /** Method to get the minimum vector  */
  BinMinContainerType& GetMins() { return m_Min ; }
  
  /** Method to get the maximum vector  */
  BinMaxContainerType& GetMaxs() { return m_Max ; }
  
  /** Method to get mins of each dimension for a measurement in the histogram */
  MeasurementVectorType GetHistogramMinFromValue(const MeasurementVectorType measurement); 
  
  /** Method to get maxs of each dimension for a measurement in the histogram */
  MeasurementVectorType GetHistogramMaxFromValue(const MeasurementVectorType measurement); 
  
  /** Method to get mins in the histogram by index  */
  MeasurementVectorType GetHistogramMinFromIndex(const IndexType index) ;
  
  /**  Method to get maxs in the histogram by index  */
  MeasurementVectorType GetHistogramMaxFromIndex(const IndexType index) ; 
  
  /** Method to get the frequency from histogram */
  FrequencyType GetFrequency(const InstanceIdentifier id)
  { return m_FrequencyContainer->GetFrequency(id) ; }

  /** returns frequency of a bin that is indexed by index */
  inline FrequencyType GetFrequency(const IndexType index) ;

  /** Method to set the frequency of histogram */
  void SetFrequency(const InstanceIdentifier id, const FrequencyType value) 
  { m_FrequencyContainer->SetFrequency(id, value) ; }
  /** Method to set the frequency of histogram */
  inline void SetFrequency(const IndexType index, 
                           const FrequencyType value) ;
  
  /** Method to set the frequency corresponding to gray levels measurement */
  inline void SetFrequency(const MeasurementVectorType measurement, 
                           const FrequencyType value) ;


  /** Method to increase the frequency by one.  This function is convinent
   * to create histogram. */
  void IncreaseFrequency(const InstanceIdentifier id,
                         const FrequencyType value) 
  { m_FrequencyContainer->IncreaseFrequency(id, value) ; }

  /** Method to increase the frequency by one.  This function is convinent
   * to create histogram. */
  inline void IncreaseFrequency(const IndexType index, 
                                const FrequencyType value) ;
  
  /** Method to increase the frequency by one.  This function is convinent
   * to create histogram. */
  inline void IncreaseFrequency(const MeasurementVectorType measurement, 
                                const FrequencyType value) ;
  
  /** Method to get measurement from the histogram using an instance identifier */
  MeasurementVectorType GetMeasurementVector(const InstanceIdentifier id) ;
  
  /** Method to get measurement from the histogram */
  MeasurementVectorType GetMeasurementVector(const IndexType index) ;
  
  /** Method to get measurement from the histogram */
  inline MeasurementType GetMeasurement(const unsigned int dimension, 
                                       const unsigned long n) ;
  
  /** returns the frequency of the'dimension' dimension's 'n'th element. */
  inline FrequencyType GetFrequency(const unsigned int dimension, 
                                    const unsigned long n) ;

  /** returns the frequency of the 'dimension' dimension */
  FrequencyType GetTotalFrequency(const unsigned int dimension) ;

  /** returns 'p'th percentile value.
   * Let assume n = the index of the bin where the p-th percentile value is,
   * min = min value of the dimension of the bin,
   * max = max value of the dimension of the bin,
   * interval = max - min , 
   * pp = cumlated proportion until n-1 bin ;
   * and pb = frequency of the bin / total frequency of the dimension.
   * 
   * If p is less than 0.5, 
   * the percentile value =  
   * min + ((p - pp ) / pb) * interval 
   * If p is greater than or equal to 0.5
   * the percentile value = 
   * max - ((pp - p) / pb) * interval  */
  double Quantile(unsigned int dimension, double p) ;

  /** iterator support */
  class Iterator ;
  friend class Iterator ;

  Iterator  Begin()
  { 
    Iterator iter(0, this) ; 
    return iter ;
  }
           
  Iterator  End()        
  {
    return Iterator(m_OffsetTable[MeasurementVectorSize], this) ;
  }
  

  class Iterator
  {
  public:
    Iterator(){};
    
    Iterator(Pointer histogram) 
    { 
      m_Id = 0 ;
      m_Histogram = histogram; 
    } 
    
    Iterator(InstanceIdentifier id, Pointer histogram)
      : m_Id(id), m_Histogram(histogram)
    {}
    
    const FrequencyType GetFrequency() 
    { 
      return  m_Histogram->GetFrequency(m_Id) ;
    }
    
    void SetFrequency(const FrequencyType value) 
    { 
      m_Histogram->SetFrequency(m_Id, value); 
    }

    InstanceIdentifier GetInstanceIdentifier()
    { return m_Id ; }

    MeasurementVectorType GetMeasurementVector()
    { 
      return m_Histogram->GetMeasurementVector(m_Id) ;
    } 

    MeasurementType GetMeasurement(int dim)
    { 
      return m_Histogram->GetMeasurementVector(m_Id)[dim] ;
    } 

    Iterator& operator++() 
    { 
      ++m_Id; 
      return *this;
    }
    
    bool operator!=(const Iterator& it) 
    { return (m_Id != it.m_Id); }
    
    bool operator==(const Iterator& it) 
    { return (m_Id == it.m_Id); }
    
    Iterator& operator=(const Iterator& it)
    { 
      m_Id  = it.m_Id;
      m_Histogram = it.m_Histogram ; 
      return *this ;
    }
    
  private:
    // Iterator pointing DenseFrequencyContainer
    InstanceIdentifier m_Id;
    
    // Pointer of DenseFrequencyContainer
    Pointer m_Histogram ;
  } ; // end of iterator class

protected:
  Histogram() ;
  virtual ~Histogram() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  // The number of bins for each each dimension
  SizeType m_Size ;
  
  // lower bound of each bin
  std::vector< std::vector<float> > m_Min ;
  
  // upper bound of each bin
  std::vector< std::vector<float> > m_Max ;
  
private:
  Histogram(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InstanceIdentifier  m_OffsetTable[MeasurementVectorSize + 1] ;
  FrequencyContainerPointer m_FrequencyContainer ;
} ; // end of class

  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogram.txx"
#endif

#endif
