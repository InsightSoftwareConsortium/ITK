/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatSparseHistogram.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkStatSparseHistogram_h
#define __itkStatSparseHistogram_h

#include <map>
#include "itkIndex.h"
#include "itkObjectFactory.h"
#include "itkStatHistogram.h"

namespace itk{

/** \class SparseHistogram 
 *  \brief his class is a container for an histogram.
 *  This class uses an map to store histogram
 */

template <class TBin, unsigned int HistogramDimension = 1, class TFeature = double>
class ITK_EXPORT SparseHistogram :
    public Histogram < TBin, HistogramDimension >
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef SparseHistogram  Self;

 /**
  * Standard Superclass typedef
  */
  typedef Histogram <TBin, HistogramDimension> Superclass;

  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::PointType PointType;

 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  * Histogram typedef support
  */
  typedef std::map<unsigned long, TBin> HistogramType;

 /**
  * Iterator typedef support
  */
  typedef HistogramType::iterator HistogramIterator;   

 /**
  * Run-time type information (and related methods).
  */
  itkTypeMacro(SparseHistogram, Histogram);

 /**
  * Method for creation through the object factory.
  */
  itkNewMacro(Self);

 /**
  * Method to get the frequency of a bin from the histogram
  */
  const TBin GetFrequency(const IndexType index);
 
 /**
  * Method to get the frequency corresponding to gray levels point
  */
  const TBin GetFrequency(const PointType point); 

 /**
  * Method to set the frequency of histogram
  */
  void SetFrequency(const IndexType index, const TBin value)
    { m_Histogram[ComputeOffset(index)] = value; }

 /**
  * Method to set the frequency corresponding to gray levels point
  */
  void SetFrequency(const PointType point, const TBin value);

 /**
  * Method to create the histogram. It has to be called after 
  * m_Size is set.
  */
  void AllocateHistogram()  { this->ComputeOffsetTable(); }

  class Iterator;
  friend class Iterator;

  class Iterator
  {
  public:
    Iterator(Pointer h)
    { m_Iter = h->m_Histogram.begin();
      m_Pos = (*m_Iter).first;
      m_SparseHistogram = h; } 

    Iterator(long l, HistogramIterator i):m_Pos(l), m_Iter(i){}
     
    const  TBin GetFrequency() 
    { IndexType index = m_SparseHistogram->ComputeIndex(m_Pos);
      return  m_SparseHistogram->GetFrequency(index); }
    
    void   SetFrequency(const TBin value) 
    { IndexType index = m_SparseHistogram->ComputeIndex(m_Pos);
      m_SparseHistogram->SetFrequency(index, value); }
    
    TFeature GetFeature(int dimension) 
    { IndexType index = m_SparseHistogram->ComputeIndex(m_Pos);
      return m_SparseHistogram->GetFeature(index, dimension); }

    PointType GetFeature()
    { IndexType index = m_SparseHistogram->ComputeIndex(m_Pos);
      return m_SparseHistogram->GetFeature(index); } 
      
    IndexType GetOffset()   { return m_Pos;  }
    void SetOffset(long l) { m_Pos = l; }  
    Iterator  Begin()
    {
      HistogramIterator it = m_SparseHistogram->m_Histogram.begin();                          
      return Iterator((*it).first, it); 
    }
           
    Iterator  End()        
    {
      HistogramIterator it = m_SparseHistogram->m_Histogram.end();
      return Iterator(-1, it);     
    }
    Iterator& operator++() 
    { 
      ++m_Iter; 
      if ( m_Iter == m_SparseHistogram->m_Histogram.end() )
        m_Pos = -1;
      else
        m_Pos = (*m_Iter).first; 
      return *this;
    }
    bool      IsAtBegin()  
    { HistogramIterator it = m_SparseHistogram->m_Histogram.begin();
      return ( m_Pos == (*it).first ); } 
    bool      IsAtEnd()    
    { HistogramIterator it = m_SparseHistogram->m_Histogram.end();
      return ( m_Pos == -1 );   }
    Iterator& operator=(const Iterator& iter){ }
     
  private:
    // Current position of iterator
    long m_Pos;
    // Iterator pointing DenseHistogram
    HistogramIterator m_Iter;

    // Pointer of DenseHistogram
    Pointer m_SparseHistogram;
  };

protected:

 /**
  * Compute an offset from the beginning of the histogram for a bin
  * at the specified index
  */
  unsigned long ComputeOffset(const IndexType index);

 /**
  * Compute an index from the beginning of the histogram for a bin
  * at the specified offset
  */
  Index<HistogramDimension> ComputeIndex(unsigned long offset);

 /**
  * Calculate the offsets needed to move from one bin to the next 
  * along a row, column, slice, volume, etc. These offsets are based
  * on the size of the Histogram. This should be called after the
  * m_Size is set.   
  */
  void ComputeOffsetTable();
 
  SparseHistogram() {};
  virtual ~SparseHistogram() {};
  SparseHistogram(const Self&) {};
  void operator=(const Self&) {};

private:
  // Container of histogram
  HistogramType m_Histogram;
  
  unsigned long m_OffsetTable[HistogramDimension+1];

};

} // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatSparseHistogram.txx"
#endif

#endif
