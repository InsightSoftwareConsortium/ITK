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

#include <vector>
#include <map>

#include "itkStatHistogram.h"


namespace itk{

/** \class SparseHistogram 
 *  \brief his class is a container for an histogram.
 *  This class uses an map to store histogram
 */

template <class TBin, unsigned int HistogramDimension = 1>

class ITK_EXPORT SparseHistogram :
    public Histogram <TBin, HistogramDimension>
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef SparseHistogram  Self;

 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  * Histogram typedef support
  */
  typedef map<unsigned long, TBin> HistogramType;

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
  const TBin& GetFrequency(const IndexType & index)
    { return m_Histogram[ComputeOffset(index)]; }

 /**
  * Method to get the frequency corresponding to gray levels point
  */
  const TBin& GetFrequency(const PointType & point); 

 /**
  * Method to set the frequency of histogram
  */
  void SetFrequency(const IndexType & index, const TBin & value)
    { m_Histogram[ComputeOffset(index)] = value; }

 /**
  * Method to set the frequency corresponding to gray levels point
  */
  void SetFrequency(const PointType & point, const TBin & value);

 /**
  * Method to create the histogram. It has to be called after 
  * m_Size is set.
  */
  void AllocateHistogram()
    { this->ComputeOffsetTable(); }

protected:

 /**
  * Compute an offset from the beginning of the histogram for a bin
  * at the specified index
  */
  unsigned long ComputeOffset(const IndexType & index);

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

  HistogramType m_Histogram;
  
  unsigned long m_OffsetTable[HistogramDimension+1];

};

} // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatSparseHistogram.txx"
#endif

#endif