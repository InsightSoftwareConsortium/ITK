/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatDenseHistogram.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkStatDenseHistogram_h
#define __itkStatDenseHistogram_h

#include "itkImage.h"
#include "itkIndex.h"
#include "itkSmartPointer.h"
#include "itkStatHistogram.h"
#include "itkImageRegionIterator.h"

namespace itk{

/** \class DenseHistogram 
 *  \brief This class is a container for an histogram.
 *  This class uses an image to store histogram
 *
 */

template <class TBin, unsigned int HistogramDimension = 1, class TFeature = float >
class ITK_EXPORT DenseHistogram :
     public Histogram <TBin, HistogramDimension, TFeature>
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef DenseHistogram Self;
 
 /**
  * Standard Superclass typedef
  */
  typedef Histogram <TBin, HistogramDimension, TFeature> Superclass;

  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::PointType PointType;
  typedef typename Superclass::BinType BinType;
  typedef typename Superclass::FeatureType FeatureType;

 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  * Histogram typedef support
  */
  typedef Image<TBin,HistogramDimension> HistogramType;

 /**
  * Iterator typedef support
  */
  typedef ImageRegionIterator<HistogramType> HistogramIterator;   

 /**
  * Run-time type information (and related methods).
  */
  itkTypeMacro(DenseHistogram, Histogram);

 /**
  * Method for creation through the object factory.
  */
  itkNewMacro(Self);

 /**
  * Method to get the frequency of a bin from the histogram
  */
  const TBin GetFrequency(const IndexType index)
    { return m_Histogram->GetPixel(index); } 

 /**
  * Method to get the frequency corresponding to gray levels point
  */
  const TBin GetFrequency(const PointType point); 

 /**
  * Method to set the frequency of histogram
  */
  void SetFrequency(const IndexType index, const TBin value)
    { m_Histogram->SetPixel(index, value); }

 /**
  * Method to set the frequency corresponding to gray levels point
  */
  void SetFrequency(const PointType point, const TBin value);
 
 /**
  * Method to create the histogram. It has to be called after 
  * m_Size is set.
  */
  void AllocateHistogram();
  
  class Iterator;
  friend class Iterator;

  class Iterator
  {
  public:
    Iterator(){};
    Iterator(Pointer h)
    { 
      HistogramIterator it(h->m_Histogram, h->m_Histogram->GetBufferedRegion());
      m_Iter = it.Begin();
      m_Pos = IndexType::ZeroIndex;
      m_DenseHistogram = h; } 

    Iterator(IndexType d, HistogramIterator i):m_Pos(d), m_Iter(i){}
     
    const  TBin GetFrequency() 
    { return  m_DenseHistogram->GetFrequency(m_Pos); }
    
    void   SetFrequency(const TBin value) 
    { m_DenseHistogram->SetFrequency(m_Pos, value); }
    
    TFeature GetFeature(int dimension) 
    { return m_DenseHistogram->GetFeature(m_Pos, dimension); }

    PointType GetFeature()
    { return m_DenseHistogram->GetFeature(m_Pos); } 
      
    IndexType GetIndex()   { return m_Pos;  }
    void SetIndex(IndexType i) { m_Pos = i; }  
 

    Iterator Begin()
    { 
      HistogramIterator it(m_DenseHistogram->m_Histogram, m_DenseHistogram->m_Histogram->GetBufferedRegion());
      //m_Iter = it.Begin();
      //m_Pos = IndexType::ZeroIndex;
      Iterator iter(IndexType::ZeroIndex, it.Begin());
      return iter;
    }
       
    Iterator  End()        
    {
      HistogramIterator it(m_DenseHistogram->m_Histogram, 
                           m_DenseHistogram->m_Histogram->GetBufferedRegion());
      //m_Iter = m_Iter.End();
      //m_Pos = m_Iter.End().GetIndex();
      Iterator iter(it.End().GetIndex(), it.End()); 
      return iter;
    }
    Iterator& operator++() { ++m_Iter; m_Pos = m_Iter.GetIndex(); return *this;}
    bool      IsAtBegin()  { return ( m_Pos == m_Iter.Begin().GetIndex() ); } 
    bool      IsAtEnd()    { return ( m_Pos == m_Iter.End().GetIndex() ); }
    Iterator& operator=(const Iterator& iter){ }
     
  private:
    // Current position of iterator
    IndexType m_Pos;

    // Iterator pointing DenseHistogram
    HistogramIterator m_Iter;

    // Pointer of DenseHistogram
    Pointer m_DenseHistogram;
 
  };

protected:

  DenseHistogram() {};
  virtual ~DenseHistogram() {};
  DenseHistogram(const Self&) {};
  void operator=(const Self&) {};
  
private:

  typename HistogramType::Pointer m_Histogram;

};



} // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatDenseHistogram.txx"
#endif

#endif
