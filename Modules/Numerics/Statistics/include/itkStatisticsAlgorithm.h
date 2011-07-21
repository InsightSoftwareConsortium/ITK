/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkStatisticsAlgorithm_h
#define __itkStatisticsAlgorithm_h

#include "itkSubsample.h"

namespace itk
{
namespace Statistics
{
namespace Algorithm
{
#if !defined( _MSC_VER )

template< class TSize >
TSize FloorLog(TSize size);

template< class TValue >
TValue MedianOfThree(const TValue a, const TValue b, const TValue c);

template< class TSample >
void FindSampleBound(const TSample * sample,
                     typename TSample::ConstIterator begin,
                     typename TSample::ConstIterator end,
                     typename TSample::MeasurementVectorType & min,
                     typename TSample::MeasurementVectorType & max);

template< class TSubsample >
void FindSampleBoundAndMean(const TSubsample * sample,
                            int beginIndex,
                            int endIndex,
                            typename TSubsample::MeasurementVectorType & min,
                            typename TSubsample::MeasurementVectorType & max,
                            typename TSubsample::MeasurementVectorType & mean);

/** The Partition algorithm performs partial sorting in a sample. Given a
 * partitionValue, the algorithm moves to the beginning of the sample all
 * MeasurementVectors whose component activeDimension is smaller than the
 * partitionValue. In this way, the sample is partially sorted in two groups.
 * First the group with activeDimension component smaller than the
 * partitionValue, then the group of MeasurementVectors with activeDimension
 * component larger than the partitionValue. The Partition algorithm takes as
 * input a sample, and a range in that sample defined by [beginIndex,endIndex].
 * Only the activeDimension components of the MeasurementVectors in the sample
 * will be considered by the algorithm. The Algorithm return an index in the
 * range of [beginIndex,endIndex] pointing to the element with activeDimension
 * component closest to the partitionValue. */
template< class TSubsample >
int Partition(TSubsample *sample,
              unsigned int activeDimension,
              int beginIndex, int endIndex,
              const typename TSubsample::MeasurementType partitionValue);

/** QuickSelect is an algorithm for finding the k-th largest element of a list.
 * In this case, only one of the components of the measurement vectors is
 * considered. This component is defined by the argument activeDimension. The
 * search is rectricted to the range between the index begin and end, also
 * passed as arguments. In this version, a guess value for the median index is
 * provided in the argument medianGuess. The algoritm returns the value of the
 * activeDimension component in the MeasurementVector located in the kth position.
 * http://en.wikipedia.org/wiki/Selection_algorithm */
template< class TSubsample >
typename TSubsample::MeasurementType
QuickSelect(TSubsample * sample,
            unsigned int activeDimension,
            int beginIndex, int endIndex,
            int kth,
            typename TSubsample::MeasurementType medianGuess);

/** QuickSelect is an algorithm for finding the k-th largest element of a list.
 * In this case, only one of the components of the measurement vectors is
 * considered. This component is defined by the argument activeDimension. The
 * search is rectricted to the range between the index begin and end, also
 * passed as arguments.
 * http://en.wikipedia.org/wiki/Selection_algorithm. */
template< class TSubsample >
typename TSubsample::MeasurementType
QuickSelect(TSubsample *sample,
            unsigned int activeDimension,
            int beginIndex, int endIndex,
            int kth);

/** NthElement is an algorithm for finding the n-th largest element of a list.
 * In this case, only one of the components of the measurement vectors is
 * considered. This component is defined by the argument activeDimension. The
 * search is restricted to the range between the index begin and end, also
 * passed as arguments. This algorithm was based on the procedure used in the STL
 * nth_element method. */
template< class TSubsample >
typename TSubsample::MeasurementType
NthElement(TSubsample *sample,
           unsigned int activeDimension,
           int beginIndex, int endIndex,
           int nth);

template< class TSubsample >
void InsertSort(TSubsample *sample,
                unsigned int activeDimension,
                int beginIndex, int endIndex);

template< class TSubsample >
void DownHeap(TSubsample *sample,
              unsigned int activeDimension,
              int beginIndex, int endIndex, int node);

template< class TSubsample >
void HeapSort(TSubsample *sample,
              unsigned int activeDimension,
              int beginIndex, int endIndex);

template< class TSubsample >
void IntrospectiveSortLoop(TSubsample *sample,
                           unsigned int activeDimension,
                           int beginIndex,
                           int endIndex,
                           int depthLimit,
                           int sizeThreshold);

template< class TSubsample >
void IntrospectiveSort(TSubsample *sample,
                       unsigned int activeDimension,
                       int beginIndex, int endIndex,
                       int sizeThreshold);

#endif // #if defined(_MSC_VER)
} // end of namespace Algorithm
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsAlgorithm.hxx"
#endif

#endif // #ifndef __itkStatisticsAlgorithm_h
