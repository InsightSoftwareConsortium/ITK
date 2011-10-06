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
#ifndef __itkThreadedDomainPartitioner_h
#define __itkThreadedDomainPartitioner_h

#include "itkIntTypes.h"
#include "itkMultiThreader.h"
#include "itkObject.h"

namespace itk
{

/** \class ThreadedDomainPartitioner
 *  \brief Virtual base class for partitioning a domain into subsets to be
 *  processed per thread when parallel processing.
 *
 * The class is templated over the type of domain over which threading
 * is performed, e.g. an image region. And it is templated over the
 * type of the data holder. The data holder is supplied to the threading
 * callback for the user, i.e. as user data.
 *
 * SplitDomain is a method to split the domain into
 * non-overlapping pieces for threading. Must be overridden by derived
 * classes to provide the particular functionality required for
 * TDomain type.
 *
 * Call SetHolder to assign a user data object.
 *
 * Call SetDomain to assign the domain object to split into per-thread
 * regions.
 *
 * Call SetThreadedGenerateData to define the worker callback function,
 *  which is called from each thread with a unique region to process.
 * \warning This callback function must be \c static if it is a class method.
 *
 * Call GenerateData to begin threaded processing.
 *
 * \warning The actual number of threads used may be less than the
 * requested number of threads. Either because the requested number is
 * greater than the number available, or the PartitionDomain method
 * decides that fewer threads would be more efficient. After the threader
 * has run, m_NumberOfThreadsUsed holds the actual number used.
 * See \c DetermineNumberOfThreadsToUse to get the number of threads
 * before running.
 *
 * \note There is no test for this class yet. See ThreadedArrayPartitioner.
 * \todo Make test.
 *
 * \sa ThreadedImageRegionPartitioner
 *
 * \ingroup DataProcessing
 *
 * \ingroup ITKCommon
 *
 */

template <class TDomain >
class ITKCommon_EXPORT ThreadedDomainPartitioner : public Object
{
public:
  /** Standard class typedefs. */
  typedef ThreadedDomainPartitioner Self;
  typedef Object                    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThreadedDomainPartitioner, Object);

  /** Type of the input object that's split for threading */
  typedef TDomain                   DomainType;

  /** Determine the number of threads that will be used by calling
   * PartitionDomain once and getting the return value. This uses
   * m_NumberOfThreads and requires that \c m_Domain has been set.
   * The number may be less than m_NumberOfThreads if PartitionDomain
   * determines that fewer threads would be more efficient. */
  ThreadIdType DetermineNumberOfThreadsToUse() const;

  /** Split the output's RequestedObject into \c requestedTotal "pieces",
   * returning piece \c i as \c splitObject. "Pieces" may represent
   * an image region, or a index range for a parameter array, etc, depending
   * on the type of object over which this class is templated.
   * This method is called \c requestedTotal times, which is the number of
   * threads available for use. The
   * pieces must not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the output RequestedObject,
   * i.e. return value is less than or equal to \c requestedTotal.
   * This must be overridden by derived classes to provide specialized
   * behavior. */
  virtual
  ThreadIdType PartitionDomain(const ThreadIdType threadID,
                           const ThreadIdType requestedTotal,
                           const DomainType& completeDomain,
                           DomainType& subdomain) const = 0;

protected:
  ThreadedDomainPartitioner(){}
  ~ThreadedDomainPartitioner(){}

private:
  ThreadedDomainPartitioner(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif
