/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleToHistogramFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSampleToHistogramImporter_h
#define __itkListSampleToHistogramImporter_h

#include "itkObject.h"
#include "itkListSample.h"
#include "itkHistogram.h"

namespace itk{
  namespace Statistics{

/** \class ListSampleToHistogramImporter
 *  \brief Imports data from ListSample object to Histogram object
 *
 * Before beginning import process, users should prepare the Histogram object
 * by calling histogram object's Initialize(Size), SetBinMin(dimension, n), 
 * and SetBinMax(dimension, n) methods.
 *
 * To do: selective importing for subset of feature vector dimensions
 */
template< class TListSample, class THistogram >
class ITK_EXPORT ListSampleToHistogramImporter :
      public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ListSampleToHistogramImporter Self;
  
  /**
   * Standard "Superclass" typedef
   */
  typedef Object Superclass;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  
  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageListSampleAdaptor, ListSample) ;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self) ;

  typedef TListSample ListSampleType ;
  typedef typename TListSample::Pointer ListSamplePointer ;
  typedef THistogram HistogramType ;
  typedef typename THistogram::Pointer HistogramPointer ;

  /**
   * plug in the ListSample object
   */
  void SetListSample(ListSamplePointer list)
  {
    m_List = list ;
  }

  /**
   * plug in the Histogram object
   */
  void SetHistogram(HistogramPointer histogram)
  {
    m_Histogram = histogram ;
  }

  /**
   * starts import procedure
   */
  void Run() ;

protected:
  ListSampleToHistogramImporter() ;
  virtual ~ListSampleToHistogramImporter() {}
private:
  ListSamplePointer m_List ;
  HistogramPointer m_Histogram ;
} ; // end of class

  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkListSampleToHistogramImporter.txx"
#endif

#endif
