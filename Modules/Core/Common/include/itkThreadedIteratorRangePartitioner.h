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
#ifndef __itkThreadedIteratorRangePartitioner_h
#define __itkThreadedIteratorRangePartitioner_h

#include "itkThreadedDomainPartitioner.h"

namespace itk
{

template< class TIterator >
class ThreadedIteratorRangePartitioner;

/** \class ThreadedIteratorRangePartitionerDomain
 * \brief Domain type for the ThreadedIteratorRangePartitioner.
 *
 * Contains a Begin and End iterator.
 *
 * \ingroup ITKCommon
 */
template< class TIterator >
class ThreadedIteratorRangePartitionerDomain
{
public:
  typedef TIterator                              IteratorType;
  typedef ThreadedIteratorRangePartitionerDomain Self;

  ThreadedIteratorRangePartitionerDomain() {}

  ThreadedIteratorRangePartitionerDomain( const IteratorType & begin, const IteratorType & end )
    {
    this->m_Begin = begin;
    this->m_End = end;
    }

  ThreadedIteratorRangePartitionerDomain( const Self & rhs )
    {
    this->m_Begin = rhs.m_Begin;
    this->m_End   = rhs.m_End;
    }

  void operator=( const Self & rhs )
    {
    if ( this == & rhs )
      {
      return;
      }
    this->m_Begin = rhs.m_Begin;
    this->m_End   = rhs.m_End;
    }

  const IteratorType & Begin() const
    {
    return this->m_Begin;
    }
  const IteratorType & End() const
    {
    return this->m_End;
    }

private:
  friend class ThreadedIteratorRangePartitioner< IteratorType >;
  IteratorType m_Begin;
  IteratorType m_End;
};

/** \class ThreadedIteratorRangePartitioner
 *  \brief Partitions an iterator range for threading.
 *
 *  \tparam TIterator The type of the iterator.
 *
 * The DomainType is defined to be a two component struct of interators:
 * the first iterator, \c Begin, defines the start of the domain, and the second iterator,
 * \c End, defines one element past the end of the domain.
 *
 * The class assumes that iterating through the domain will be a repeatable
 * process.
 *
 * While this class will work for most containers that use iterators, indexed
 * containers such as std::vector or Array will be partitioned more efficiently with a
 * ThreadedIndexedContainerPartitioner.
 *
 * \sa ThreadedIndexedContainerPartitioner
 * \sa ThreadedDomainPartitioner
 * \sa IteratorRangeInterface
 * \ingroup ITKCommon
 */
template< class TIterator >
class ThreadedIteratorRangePartitioner
  : public ThreadedDomainPartitioner< ThreadedIteratorRangePartitionerDomain< TIterator > >
{
public:
  /** Standard class typedefs. */
  typedef ThreadedIteratorRangePartitioner                                                  Self;
  typedef ThreadedDomainPartitioner< ThreadedIteratorRangePartitionerDomain< TIterator > > Superclass;
  typedef SmartPointer< Self >                                                              Pointer;
  typedef SmartPointer< const Self >                                                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThreadedIteratorRangePartitioner, ThreadedDomainPartitioner);

  /** Type for convenience of base class methods */
  typedef typename Superclass::DomainType  DomainType;

  typedef TIterator IteratorType;

  /** Split the Domain into
   * \c requestedTotal sub-domains, returning sub-domain \c i as \c subdomain.
   * This method is called \c requestedTotal times. The
   * pieces will not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the output RequestedObject,
   * i.e. return value is less than or equal to \c requestedTotal. */
  virtual
  ThreadIdType PartitionDomain(const ThreadIdType i,
                           const ThreadIdType requestedTotal,
                           const DomainType& completeDomain,
                           DomainType& subdomain) const;

protected:
  ThreadedIteratorRangePartitioner();
  virtual ~ThreadedIteratorRangePartitioner();

private:
  ThreadedIteratorRangePartitioner(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThreadedIteratorRangePartitioner.hxx"
#endif

#endif
