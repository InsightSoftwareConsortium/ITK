/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsAlgorithm.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsAlgorithm_txx
#define __itkStatisticsAlgorithm_txx

#include "itkStatisticsAlgorithm.h"
#include "itkNumericTraits.h"

namespace itk{
namespace Statistics{

template< class TSize >
inline TSize FloorLog(TSize size) 
{
  TSize k ;
  
  for (k = 0; size != 1; size >>= 1) 
    {
      ++k;
    }
  
  return k;
}

/** the endIndex should points one point after the last elements 
 * if multiple partitionValue exist in the sample the return index
 * will points the middle of such values */
template< class TSubsample >
inline int Partition(TSubsample* sample,
                     unsigned int activeDimension,
                     int beginIndex, int endIndex,
                     const typename TSubsample::MeasurementType
                     partitionValue) 
{
  while (true) 
    {
      while (sample->GetMeasurementVectorByIndex(beginIndex)[activeDimension]
             < partitionValue)
        {
          ++beginIndex;
        }

      --endIndex;
      while (partitionValue < 
             sample->GetMeasurementVectorByIndex(endIndex)[activeDimension])
        {
          --endIndex;
        }

      if (!(beginIndex < endIndex))
        {
          return beginIndex ;
        }
      
      sample->Swap(beginIndex, endIndex);
      ++beginIndex;
    }
} 

template< class TValue >
inline TValue MedianOfThree(const TValue a, 
                            const TValue b, 
                            const TValue c)
{  
  if (a < b) {
    if (b < c) {
      return b;
    }
    else if (a < c) {
      return c;
    }
    else {
      return a;
    }
  }
  else if (a < c) {
    return a ;
  }
  else if (b < c) {
    return c;
  }
  else {
    return b;
  }
}

template< class TSample >
inline void FindSampleBound(TSample* ,
                            typename TSample::Iterator begin,
                            typename TSample::Iterator end,
                            typename TSample::MeasurementVectorType &min,
                            typename TSample::MeasurementVectorType &max)
{    
  enum { Dimension = TSample::MeasurementVectorSize } ;

  unsigned int dimension ;
  typename TSample::MeasurementVectorType temp ;

  min = max = temp = begin.GetMeasurementVector() ;
  while (begin != end)
    {
      for (dimension= 0 ; dimension < Dimension ; dimension++) 
        {
          if ( temp[dimension] < min[dimension]) 
            {
              min[dimension] = temp[dimension] ;
            }
          else if (temp[dimension] > max[dimension]) 
            {
              max[dimension] = temp[dimension] ;
            }
        }
      ++begin ;
      temp = begin.GetMeasurementVector() ;
    } // end of while
}

/** The endIndex should points one point after the last elements. */
template< class TSubsample >
inline void FindSampleBound(TSubsample* sample,
                            int beginIndex,
                            int endIndex,
                            typename TSubsample::MeasurementVectorType &min,
                            typename TSubsample::MeasurementVectorType &max)
{    
  enum { Dimension = TSubsample::MeasurementVectorSize } ;

  unsigned int dimension ;
  typename TSubsample::MeasurementVectorType temp ;

  min = max = temp = sample->GetMeasurementVectorByIndex(beginIndex) ;
  while (true)
    {
      for (dimension= 0 ; dimension < Dimension ; dimension++) 
        {
          if ( temp[dimension] < min[dimension]) 
            {
              min[dimension] = temp[dimension] ;
            }
          else if (temp[dimension] > max[dimension]) 
            {
              max[dimension] = temp[dimension] ;
            }
        }
      ++beginIndex ;
      if (beginIndex == endIndex)
        {
          break ;
        }
      temp = sample->GetMeasurementVectorByIndex(beginIndex) ;
    } // end of while
}

/** The endIndex should points one point after the last elements. */
template< class TSubsample >
inline void 
FindSampleBoundAndMean(TSubsample* sample,
                       int beginIndex,
                       int endIndex,
                       typename TSubsample::MeasurementVectorType &min,
                       typename TSubsample::MeasurementVectorType &max,
                       typename TSubsample::MeasurementVectorType &mean)
{    
  typedef typename TSubsample::MeasurementType MeasurementType ;
  typedef typename TSubsample::MeasurementVectorType MeasurementVectorType ;

  enum { Dimension = TSubsample::MeasurementVectorSize } ;

  FixedArray< double, Dimension > sum ;

  unsigned int dimension ;
  MeasurementVectorType temp ;

  double numberOfInstances = endIndex - beginIndex ;

  min = max = temp = sample->GetMeasurementVectorByIndex(beginIndex) ;
  sum.Fill(0.0) ;
 
  while (true)
    {
      for (dimension= 0 ; dimension < Dimension ; dimension++) 
        {
          if ( temp[dimension] < min[dimension]) 
            {
              min[dimension] = temp[dimension] ;
            }
          else if (temp[dimension] > max[dimension]) 
            {
              max[dimension] = temp[dimension] ;
            }
          sum[dimension] += temp[dimension] ;
        }
      ++beginIndex ;
      if (beginIndex == endIndex)
        {
          break ;
        }
      temp = sample->GetMeasurementVectorByIndex(beginIndex) ;
    } // end of while

  for (int i = 0 ; i < Dimension ; i++)
    {
      mean[i] = int(sum[i] / numberOfInstances) ;
    }
}

/** The endIndex should point one point after the last elements. */
template< class TSubsample >
inline typename TSubsample::MeasurementType 
QuickSelect(TSubsample* sample,
            unsigned int activeDimension,
            int beginIndex,
            int endIndex,
            int kth,
            typename TSubsample::MeasurementType medianGuess)
{
  typedef typename TSubsample::MeasurementType MeasurementType ;

  int length = endIndex - beginIndex ;
  int begin = beginIndex ;
  int end = endIndex - 1 ;
  int cut ;
  MeasurementType tempMedian ;

  if (medianGuess != NumericTraits< MeasurementType >::min())
    {
      tempMedian = medianGuess ;
    }
  else
    {
      tempMedian = 
        MedianOfThree< MeasurementType >
        (sample->
         GetMeasurementVectorByIndex(begin)[activeDimension],
         sample->
         GetMeasurementVectorByIndex(end)[activeDimension],
         sample->
         GetMeasurementVectorByIndex(begin + length/2)[activeDimension]) ;
    }

  while (length > 2)
    {
      cut = Partition< TSubsample >(sample, activeDimension, 
                                    begin, end, tempMedian) ;

      if (begin == cut)
        {
          break ;
        }

      if ( cut >= beginIndex + kth)
        {
          end = cut ;
        }
      else
        {
          begin = cut ;
        }

      length = end - begin ;

      tempMedian = 
        MedianOfThree< MeasurementType >
        (sample->
         GetMeasurementVectorByIndex(begin)[activeDimension],
         sample->
         GetMeasurementVectorByIndex(end)[activeDimension],
         sample->
         GetMeasurementVectorByIndex(begin + length/2)[activeDimension]) ;
    } // end of while

  // current partition has only 1 or 2 elements
  if (length == 2 && 
      sample->GetMeasurementVectorByIndex(end)[activeDimension]
      < sample->GetMeasurementVectorByIndex(begin)[activeDimension])
    {
      sample->Swap(begin, end) ;
    }

  return sample->GetMeasurementVectorByIndex(begin)[activeDimension] ;
}


template< class TSubsample >
inline typename TSubsample::MeasurementType 
QuickSelect(TSubsample* sample,
            unsigned int activeDimension,
            int beginIndex,
            int endIndex,
            int kth)
{
  typedef typename TSubsample::MeasurementType MeasurementType ;
  MeasurementType medianGuess = NumericTraits< MeasurementType >::min() ;
  return QuickSelect< TSubsample >(sample, activeDimension, beginIndex, 
                     endIndex, kth, medianGuess) ;
}

template< class TSubsample >
inline void InsertSort(TSubsample* sample, 
                       unsigned int activeDimension,
                       int beginIndex,
                       int endIndex)
{
  int backwardSearchBegin ;
  int backwardIndex ;

  for (backwardSearchBegin = beginIndex + 1 ;
       backwardSearchBegin < endIndex ; 
       backwardSearchBegin++)
    {
      backwardIndex = backwardSearchBegin ;
      while (backwardIndex > beginIndex) 
        {
          if (sample->GetMeasurementVectorByIndex(backwardIndex)[activeDimension] < 
              sample->GetMeasurementVectorByIndex(backwardIndex - 1)[activeDimension])
            {
              sample->Swap(backwardIndex, backwardIndex - 1) ;
            }
          else
            {
              break ;
            }
          --backwardIndex ;
        }
    }
}

template< class TSubsample >
inline void DownHeap(TSubsample* sample,
                     unsigned int activeDimension,
                     int beginIndex, int endIndex, int node)
{
  int currentNode = node;
  int leftChild ; 
  int rightChild ;
  int largerChild ;
  typedef typename TSubsample::MeasurementType MeasurementType ;
  MeasurementType currentNodeValue = 
    sample->GetMeasurementVectorByIndex(currentNode)[activeDimension] ;
  MeasurementType leftChildValue ;
  MeasurementType rightChildValue ;
  MeasurementType largerChildValue ;

  while (true) 
    {
      // location of first child
      largerChild = leftChild = 
        beginIndex + 2*(currentNode - beginIndex) + 1 ; 
      rightChild = leftChild + 1 ;
      if (leftChild > endIndex - 1)
        { 
          // leaf node
          return ;
        }

      largerChildValue = rightChildValue = leftChildValue = 
        sample->GetMeasurementVectorByIndex(leftChild)[activeDimension] ;

      if (rightChild < endIndex)
        {
          rightChildValue = 
            sample->GetMeasurementVectorByIndex(rightChild)[activeDimension] ;
        }

      if (leftChildValue < rightChildValue) 
        {
          largerChild = rightChild ;
          largerChildValue = rightChildValue ;
        }
      
      if (largerChildValue <= currentNodeValue) 
        {
          // the node satisfies heap property
          return ;
        }
      // move down current node value to the larger child
      sample->Swap(currentNode, largerChild) ;
      currentNode = largerChild ;
    }
}

template< class TSubsample >
inline void HeapSort(TSubsample* sample, 
                     unsigned int activeDimension,
                     int beginIndex,
                     int endIndex)
{
  // construct a heap
  int node ;

  for (node = beginIndex + (endIndex - beginIndex) / 2 - 1 ;
       node >= beginIndex ; node--)
    {
      DownHeap< TSubsample >(sample, activeDimension,
                             beginIndex, endIndex, node) ;
    }

  // sort
  int newEndIndex ;
  for (newEndIndex = endIndex - 1 ; newEndIndex >= beginIndex ;
       --newEndIndex)
    {
      sample->Swap(beginIndex, newEndIndex);
      DownHeap< TSubsample >(sample, activeDimension,
                             beginIndex, newEndIndex, beginIndex);
    }
}


template< class TSubsample >
inline void IntrospectiveSortLoop(TSubsample* sample, 
                                  unsigned int activeDimension,
                                  int beginIndex,
                                  int endIndex,
                                  int depthLimit, 
                                  int sizeThreshold)
{
  typedef typename TSubsample::MeasurementType MeasurementType ;

  int length = endIndex - beginIndex ;
  int cut ;
  while(length > sizeThreshold)
    {
      if (depthLimit == 0)
        {
          HeapSort< TSubsample >(sample, activeDimension, 
                                 beginIndex, endIndex) ;
          return ;
        }
          
      --depthLimit ;
      cut = Partition< TSubsample >(sample, activeDimension,
                                    beginIndex, endIndex, 
                                    MedianOfThree< MeasurementType >
                                    (sample->GetMeasurementVectorByIndex(beginIndex)[activeDimension],
                                     sample->GetMeasurementVectorByIndex(beginIndex + length/2)[activeDimension],
                                     sample->GetMeasurementVectorByIndex(endIndex - 1)[activeDimension])) ;
      IntrospectiveSortLoop< TSubsample >(sample, activeDimension, 
                                          cut, endIndex, 
                                          depthLimit, sizeThreshold) ; 
      endIndex = cut ;
      length = endIndex - beginIndex ;
    }
}

template< class TSubsample >
inline void IntrospectiveSort(TSubsample* sample, 
                              unsigned int activeDimension,
                              int beginIndex,
                              int endIndex,
                              int sizeThreshold)
{
  typedef typename TSubsample::MeasurementType MeasurementType ;
  IntrospectiveSortLoop< TSubsample >(sample, activeDimension, beginIndex, endIndex, 
                                      2 * FloorLog(endIndex - beginIndex), sizeThreshold) ; 
  InsertSort< TSubsample >(sample, activeDimension, beginIndex, endIndex) ; 
}

} // end of namespace Statistics 
} // end of namespace itk 

#endif // #ifndef __itkStatisticsAlgorithm_txx










