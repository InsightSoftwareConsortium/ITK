/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsAlgorithm.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsAlgorithm_h
#define __itkStatisticsAlgorithm_h

#include "itkSample.h"
#include "itkSubsample.h"

namespace itk{
namespace Statistics{

#if !defined(_MSC_VER)

template< class TSize >
TSize FloorLog(TSize size) ;

template< class TValue >
TValue MedianOfThree(const TValue a, const TValue b, const TValue c) ;

template< class TSample >
void FindSampleBound(typename TSample::Pointer sample,
                     typename TSample::Iterator begin,
                     typename TSample::Iterator end,
                     typename TSample::MeasurementVectorType &min,
                     typename TSample::MeasurementVectorType &max) ;
  
template< class TSubsample >
int Partition(typename TSubsample::Pointer sample,
              int beginIndex, int endIndex,
              const typename TSubsample::MeasurementType partitionValue) ;

template< class TSubsample >
typename TSubsample::MeasurementType 
QuickSelect(typename TSubsample::Pointer sample,
            unsigned int activeDimension,
            int beginIndex, int endIndex,
            int kth) ;

template< class TSubsample >
void InsertSort(typename TSubsample::Pointer sample, 
                unsigned int activeDimension,
                int beginIndex, int endIndex) ;

template< class TSubsample >
void DownHeap(typename TSubsample::Pointer sample,
              int beginIndex, int endIndex, int node) ;

template< class TSubsample >
void HeapSort(typename TSubsample::Pointer sample, 
                unsigned int activeDimension,
                int beginIndex, int endIndex) ;


template< class TSubsample >
void IntrospectiveSortLoop(typename TSubsample::Pointer sample, 
                                  unsigned int activeDimension,
                                  int beginIndex,
                                  int endIndex,
                                  int depthLimit, 
                                  int sizeThreshold) ;

template< class TSubsample >
void IntrospectiveSort(typename TSubsample::Pointer sample,
                       unsigned int activeDimension,
                       int beginIndex, int endIndex,
                       int sizeThreshold) ;

#endif // #if defined(_MSC_VER)

} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsAlgorithm.txx"
#endif

#endif // #ifndef __itkStatisticsAlgorithm_h



