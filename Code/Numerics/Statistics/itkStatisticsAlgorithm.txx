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
inline int Partition(typename TSubsample::Pointer sample,
                     int beginIndex, int endIndex,
                     const typename TSubsample::MeasurementType
                     partitionValue) 
{
  while (true) 
    {
      while (sample->GetMeasurement(beginIndex) < partitionValue)
        {
          ++beginIndex;
        }

      --endIndex;
      while (partitionValue < sample->GetMeasurement(endIndex))
        {
          --endIndex;
        }

      if (!(beginIndex < endIndex))
        {
          //std::cout << "begin = " << beginIndex << " end = " << endIndex
          //                    << std::endl ;
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
inline void FindSampleBound(typename TSample::Pointer sample,
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
inline void FindSampleBound(typename TSubsample::Pointer sample,
                            int beginIndex,
                            int endIndex,
                            typename TSubsample::MeasurementVectorType &min,
                            typename TSubsample::MeasurementVectorType &max)
{    
  enum { Dimension = TSubsample::MeasurementVectorSize } ;

  unsigned int dimension ;
  typename TSubsample::MeasurementVectorType temp ;

  min = max = temp = sample->GetMeasurementVector(beginIndex) ;
  while (beginIndex < endIndex)
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
      temp = sample->GetMeasurementVector(beginIndex) ;
    } // end of while
}

/** The endIndex should point one point after the last elements. */
template< class TSubsample >
inline typename TSubsample::MeasurementType 
QuickSelect(typename TSubsample::Pointer sample,
            unsigned int activeDimension,
            int beginIndex,
            int endIndex,
            int kth)
{
  typedef typename TSubsample::MeasurementType MeasurementType ;

  sample->SetActiveDimension(activeDimension) ;

  int length = endIndex - beginIndex ;
  int begin = beginIndex ;
  int end = endIndex ;
  int cut ;

  while (length > 2)
    {
      cut = Partition< TSubsample >(sample, begin, end, 
                                    MedianOfThree< MeasurementType >
                                    (sample->GetMeasurement(begin),
                                     sample->GetMeasurement(begin + 
                                                            length / 2),
                                     sample->GetMeasurement(end - 1))) ;
      

//        std::cout << std::endl ;
//        std::cout << "dim = " << activeDimension << " begin = " << begin 
//                  << " cut = " << cut
//                  << " end = " << end << " kth = " << kth 
//                  << std::endl ;
//        for(int i = begin ; i < end ; i++)
//          {
//            if ( i % 5 == 0 && i > 0 )
//              {
//                std::cout << std::endl ; 
//              }
//            std::cout << i << ": [" <<sample->GetMeasurement(i) << "], " ;
//          }
//        std::cout << std::endl ;
//        if( end - begin < 3)
//          {
//            break ;
//          }

      if ( cut >= beginIndex + kth)
        {
          end = cut ;
        }
      else
        {
          begin = cut ;
        }

      length = end - begin ;
    }
  
  // current partition has only 1 or 2 elements
  if (length == 2 && sample->GetMeasurement(end) < sample->GetMeasurement(begin))
    {
      sample->Swap(begin, end) ;
    }

//    std::cout << "final kth = " << begin << std::endl ;
//    for(int i = beginIndex ; i < endIndex ; i++)
//      {
//        if ( i % 5 == 0 && i > 0 )
//          {
//            std::cout << std::endl ; 
//          }
//        std::cout << i << ": [" <<sample->GetMeasurement(i) << "], " ;
//      }
//    std::cout << std::endl ;
  return sample->GetMeasurement(begin) ;
}

template< class TSubsample >
inline void InsertSort(typename TSubsample::Pointer sample, 
                       unsigned int activeDimension,
                       int beginIndex,
                       int endIndex)
{
  sample->SetActiveDimension(activeDimension) ;

  int backwardSearchBegin ;
  int backwardIndex ;

  for (backwardSearchBegin = beginIndex + 1 ;
       backwardSearchBegin < endIndex ; 
       backwardSearchBegin++)
    {
      backwardIndex = backwardSearchBegin ;
      while (backwardIndex > beginIndex) 
        {
          if (sample->GetMeasurement(backwardIndex) < 
              sample->GetMeasurement(backwardIndex - 1))
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
inline void DownHeap(typename TSubsample::Pointer sample,
                     int beginIndex, int endIndex, int node)
{
  int currentNode = node;
  int leftChild ; 
  int rightChild ;
  int largerChild ;
  typedef typename TSubsample::MeasurementType MeasurementType ;
  MeasurementType currentNodeValue = sample->GetMeasurement(currentNode) ;
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
        sample->GetMeasurement(leftChild) ;

      if (rightChild < endIndex)
        {
          rightChildValue = sample->GetMeasurement(rightChild) ;
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
inline void HeapSort(typename TSubsample::Pointer sample, 
                     unsigned int activeDimension,
                     int beginIndex,
                     int endIndex)
{
  sample->SetActiveDimension(activeDimension) ;
  
  // construct a heap
  int node ;

  for (node = beginIndex + (endIndex - beginIndex) / 2 - 1 ;
       node >= beginIndex ; node--)
    {
      DownHeap< TSubsample >(sample, beginIndex, endIndex, node) ;
    }

  // sort
  int newEndIndex ;
  for (newEndIndex = endIndex - 1 ; newEndIndex >= beginIndex ;
       --newEndIndex)
    {
      sample->Swap(beginIndex, newEndIndex);
      DownHeap< TSubsample >(sample, beginIndex, newEndIndex, beginIndex);
    }
}


template< class TSubsample >
inline void IntrospectiveSortLoop(typename TSubsample::Pointer sample, 
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
      cut = Partition< TSubsample >(sample, beginIndex, endIndex, 
                                    MedianOfThree< MeasurementType >
                                    (sample->GetMeasurement(beginIndex),
                                     sample->GetMeasurement(beginIndex + 
                                                            length / 2),
                                     sample->GetMeasurement(endIndex - 1))) ;
      IntrospectiveSortLoop< TSubsample >(sample, activeDimension, cut, endIndex, 
                        depthLimit, sizeThreshold) ; 
      endIndex = cut ;
      length = endIndex - beginIndex ;
    }
}

template< class TSubsample >
inline void IntrospectiveSort(typename TSubsample::Pointer sample, 
                              unsigned int activeDimension,
                              int beginIndex,
                              int endIndex,
                              int sizeThreshold)
{
  typedef typename TSubsample::MeasurementType MeasurementType ;
  sample->SetActiveDimension(activeDimension) ;
  IntrospectiveSortLoop< TSubsample >(sample, activeDimension, beginIndex, endIndex, 
                        2 * FloorLog(endIndex - beginIndex), sizeThreshold) ; 
  InsertSort< TSubsample >(sample, activeDimension, beginIndex, endIndex) ; 
}

} // end of namespace Statistics 
} // end of namespace itk 

#endif // #ifndef __itkStatisticsAlgorithm_txx










