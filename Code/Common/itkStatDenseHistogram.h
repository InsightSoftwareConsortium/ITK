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

#include <itkImage.h>
#include "itkStatHistogram.h"

namespace itk{

/** \class DenseHistogram 
 *  \brief This class is a container for an histogram.
 *  This class uses an image to store histogram
 *
 */

template <class TBin, unsigned int HistogramDimension = 1 >
class ITK_EXPORT DenseHistogram :
     public Histogram <TBin, HistogramDimension>
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef DenseHistogram  Self;
 
 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  * Histogram typedef support
  */
  typedef Image<TBin,HistogramDimension> HistogramType;
 
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
  const TBin& GetFrequency(const IndexType & index)
    { return m_Histogram->GetPixel(index); } 

 /**
  * Method to get the frequency corresponding to gray levels point
  */
  const TBin& GetFrequency(const PointType & point); 

 /**
  * Method to set the frequency of histogram
  */
  void SetFrequency(const IndexType & index, const TBin & value)
    { m_Histogram->SetPixel(index, value); }

 /**
  * Method to set the frequency corresponding to gray levels point
  */
  void SetFrequency(const PointType & point, const TBin & value);

 /**
  * Method to create the histogram. It has to be called after 
  * m_Size is set.
  */
  void AllocateHistogram();
 
protected:

  DenseHistogram() {};
  virtual ~DenseHistogram() {};
  DenseHistogram(const Self&) {};
  void operator=(const Self&) {};
  
private:

  HistogramType::Pointer m_Histogram;

};

} // end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatDenseHistogram.txx"
#endif

#endif