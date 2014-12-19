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
#ifndef itkStatisticsAlgorithm_h
#define itkStatisticsAlgorithm_h

#include "itkSubsample.h"

namespace itk
{
namespace Statistics
{
namespace Algorithm
{
#if !defined( _MSC_VER )

template< typename TSize >
TSize FloorLog(TSize size);

template< typename TValue >
TValue MedianOfThree(const TValue a, const TValue b, const TValue c);

/** Finds the minimum and maximum of each measurement value, over an interval
 * within a sample.
 *
 * Samples are considered independently of each other.
 *
 * \param[in] sample An instance of TSample.
 * \param[in] begin An iterator to the first measurement vector in sample to be
 *                  considered.
 * \param[in] end An iterator to the last measurement vector in sample to be
 *                considered.
 * \param[out] min A new measurement vector, with the minimum values.
 * \param[out] max A new measurement vector, with the maximum values.
 *
 * \tparam TSample A subtype of Statistics::Sample.
 */
template< typename TSample >
void FindSampleBound(const TSample * sample,
                     const typename TSample::ConstIterator & begin,
                     const typename TSample::ConstIterator & end,
                     typename TSample::MeasurementVectorType & min,
                     typename TSample::MeasurementVectorType & max);

/** The endIndex should points one point after the last elements. */
template< typename TSubsample >
void FindSampleBoundAndMean(const TSubsample * sample,
                            int beginIndex,
                            int endIndex,
                            typename TSubsample::MeasurementVectorType & min,
                            typename TSubsample::MeasurementVectorType & max,
                            typename TSubsample::MeasurementVectorType & mean);

/** The Partition algorithm performs partial sorting in a sample.
 *
 * Given a partitionValue, the algorithm moves to the beginning of the sample
 * all MeasurementVectors whose component activeDimension is smaller than the
 * partitionValue. In this way, the sample is partially sorted in two groups.
 * First the group with activeDimension component smaller than the
 * partitionValue, then the group of MeasurementVectors with activeDimension
 * component larger than the partitionValue. The Partition algorithm takes as
 * input a sample, and a range in that sample defined by [beginIndex,endIndex].
 * Only the activeDimension components of the MeasurementVectors in the sample
 * will be considered by the algorithm. The Algorithm return an index in the
 * range of [beginIndex,endIndex] pointing to the element with activeDimension
 * component closest to the partitionValue.
 *
 * The endIndex should points one point after the last elements if multiple
 * partitionValue exist in the sample the return index will points the middle
 * of such values. Implemented following the description of the partition
 * algorithm in the QuickSelect entry of the Wikipedia.
 *
 * \sa http://en.wikipedia.org/wiki/Selection_algorithm.
 */
template< typename TSubsample >
int Partition(TSubsample *sample,
              unsigned int activeDimension,
              int beginIndex, int endIndex,
              const typename TSubsample::MeasurementType partitionValue);

/** QuickSelect is an algorithm for finding the k-th largest element of a list.
 *
 * In this case, only one of the components of the measurement vectors is
 * considered. This component is defined by the argument activeDimension. The
 * search is rectricted to the range between the index begin and end, also
 * passed as arguments.
 *
 * The endIndex should point one point after the last elements. Note that kth
 * is an index in a different scale than [beginIndex,endIndex]. For example,
 * it is possible to feed this function with beginIndex=15, endIndex=23, and
 * kth=3, since we can ask for the element 3rd in the range [15,23].
 * In this version, a guess value for the median index is provided in the
 * argument medianGuess. The algorithm returns the value of the activeDimension
 * component in the MeasurementVector located in the kth position.
 *
 * \sa http://en.wikipedia.org/wiki/Selection_algorithm
 */
template< typename TSubsample >
typename TSubsample::MeasurementType
QuickSelect(TSubsample * sample,
            unsigned int activeDimension,
            int beginIndex, int endIndex,
            int kth,
            typename TSubsample::MeasurementType medianGuess);

/** QuickSelect is an algorithm for finding the k-th largest element of a list.
 *
 * In this case, only one of the components of the measurement vectors is
 * considered. This component is defined by the argument activeDimension. The
 * search is rectricted to the range between the index begin and end, also
 * passed as arguments.
 *
 * The endIndex should point one point after the last elements. Note that kth
 * is an index in a different scale than [beginIndex,endIndex]. For example,
 * it is possible to feed this function with beginIndex=15, endIndex=23, and
 * kth=3, since we can ask for the element 3rd in the range [15,23].
 *
 * \sa http://en.wikipedia.org/wiki/Selection_algorithm.
 */
template< typename TSubsample >
typename TSubsample::MeasurementType
QuickSelect(TSubsample *sample,
            unsigned int activeDimension,
            int beginIndex, int endIndex,
            int kth);

/** NthElement is an algorithm for finding the n-th largest element of a list.
 *
 * In this case, only one of the components of the measurement vectors is
 * considered. This component is defined by the argument activeDimension. The
 * search is restricted to the range between the index begin and end, also
 * passed as arguments. This algorithm was based on the procedure used in the STL
 * nth_element method.
 */
template< typename TSubsample >
typename TSubsample::MeasurementType
NthElement(TSubsample *sample,
           unsigned int activeDimension,
           int beginIndex, int endIndex,
           int nth);

template< typename TSubsample >
void InsertSort(TSubsample *sample,
                unsigned int activeDimension,
                int beginIndex, int endIndex);

template< typename TSubsample >
void DownHeap(TSubsample *sample,
              unsigned int activeDimension,
              int beginIndex, int endIndex, int node);

template< typename TSubsample >
void HeapSort(TSubsample *sample,
              unsigned int activeDimension,
              int beginIndex, int endIndex);

template< typename TSubsample >
void IntrospectiveSortLoop(TSubsample *sample,
                           unsigned int activeDimension,
                           int beginIndex,
                           int endIndex,
                           int depthLimit,
                           int sizeThreshold);

template< typename TSubsample >
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

#endif // #ifndef itkStatisticsAlgorithm_h
