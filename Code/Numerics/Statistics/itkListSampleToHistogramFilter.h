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
#ifndef __itkListSampleToHistogramFilter_h
#define __itkListSampleToHistogramFilter_h

#include "itkObject.h"
#include "itkListSampleBase.h"
#include "itkHistogram.h"

namespace itk{
  namespace Statistics{

/** \class ListSampleToHistogramFilter
 *  \brief Imports data from ListSample object to Histogram object
 *
 * Before beginning import process, users should prepare the Histogram object
 * by calling histogram object's Initialize(Size), SetBinMin(dimension, n), 
 * and SetBinMax(dimension, n) methods.
 *
 * To do: selective importing for subset of feature vector dimensions
 */
template< class TListSample, class THistogram >
class ITK_EXPORT ListSampleToHistogramFilter :
      public Object
{
public:
  /** Standard typedefs */
  typedef ListSampleToHistogramFilter Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>   Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ListSampleToHistogramFilter, Object) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** plug in the ListSample object */
  void SetListSample(TListSample* list)
  { m_List = list ; }

  /** plug in the Histogram object */
  void SetHistogram(THistogram* histogram)
  { m_Histogram = histogram ;  }

  /** starts import procedure */
  void Run() ;

protected:
  ListSampleToHistogramFilter() ;
  virtual ~ListSampleToHistogramFilter() {}
private:
  TListSample* m_List ;
  THistogram* m_Histogram ;
} ; // end of class

  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkListSampleToHistogramFilter.txx"
#endif

#endif
