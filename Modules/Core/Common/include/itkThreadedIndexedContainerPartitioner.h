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
#ifndef itkThreadedIndexedContainerPartitioner_h
#define itkThreadedIndexedContainerPartitioner_h

#include "itkThreadedDomainPartitioner.h"
#include "itkIndex.h"

namespace itk
{

/** \class ThreadedIndexedContainerPartitioner
 *  \brief Partitions an indexed container.
 *
 * The \c DomainType is defined to be a two element itk::Index of ElementIdentifiers:
 * the first element defines the start of the domain, and the second element
 * defines the end of the domain.
 *
 * Element counting starts from zero and the identifier at the end of the domain
 * is inclusive.
 *
 * This class is typically used as a template argument to a DomainThreader.
 *
 * \sa ThreadedDomainPartitioner
 * \sa DomainThreader
 * \sa IndexedContainerInterface
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ThreadedIndexedContainerPartitioner
  : public ThreadedDomainPartitioner< Index<2> >
{
public:
  /** Standard class typedefs. */
  typedef ThreadedIndexedContainerPartitioner   Self;
  typedef ThreadedDomainPartitioner< Index<2> > Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThreadedIndexedContainerPartitioner, ThreadedDomainPartitioner);

  /** Type for convenience of base class methods */
  typedef Superclass::DomainType  DomainType;

  /** Synonym for the domain that is more descriptive. */
  typedef DomainType                       IndexRangeType;

  /** Split the index range \c completeIndexRange into up to \c requestedTotal
   * non-overlapping subranges, setting subrange number \c threadId as
   * \c subIndexRange and returning the total number of subranges actually available.
   *
   * This method should be called repeatedly for each value of \c threadId, from 0 up
   * to the return value (which is always less than or equal to \c requestedTotal).
   *
   * It is an error for \c completeIndexRange to be zero-length (i.e. for
   * completeIndexRange.End to be less than completeIndexRange.Begin).
   * If \c threadId is greater than the return value, the contents of
   * \c subIndexRange are undefined.
   */
  virtual
  ThreadIdType PartitionDomain(const ThreadIdType threadId,
                           const ThreadIdType requestedTotal,
                           const DomainType& completeIndexRange,
                           DomainType& subIndexRange) const ITK_OVERRIDE;

protected:
  ThreadedIndexedContainerPartitioner();
  virtual ~ThreadedIndexedContainerPartitioner() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ThreadedIndexedContainerPartitioner);

};

} // end namespace itk

#endif
