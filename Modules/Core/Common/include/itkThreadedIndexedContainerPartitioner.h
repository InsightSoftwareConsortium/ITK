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
#ifndef __itkThreadedIndexedContainerPartitioner_h
#define __itkThreadedIndexedContainerPartitioner_h

#include "itkThreadedDomainPartitioner.h"
#include "itkIndex.h"

namespace itk
{

/** \class ThreadedIndexedContainerPartitioner
 *  \brief Partitions an indexed container.
 *
 * The DomainType is defined to be a two element itk::Index of ElementIdentifiers:
 * the first element defines the start of the domain, and the second element
 * defines the end of the domain.
 *
 * Element counting starts from zero and the identifier at the end of the domain
 * is inclusive.
 *
 * \sa ThreadedImageRegionPartitioner
 * \sa ThreadedDomainPartitioner
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

  /** Split the IndexRange \c overallIndexRange into
   * \c requestedTotal subranges, returning subrange \c i as \c splitIndex.
   * This method is called \c requestedTotal times. The
   * pieces will not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the output RequestedObject,
   * i.e. return value is less than or equal to \c requestedTotal. */
  virtual
  ThreadIdType PartitionDomain(const ThreadIdType i,
                           const ThreadIdType requestedTotal,
                           const DomainType& completeIndexRange,
                           DomainType& subdomain) const;

protected:
  ThreadedIndexedContainerPartitioner();
  virtual ~ThreadedIndexedContainerPartitioner();

private:
  ThreadedIndexedContainerPartitioner(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
