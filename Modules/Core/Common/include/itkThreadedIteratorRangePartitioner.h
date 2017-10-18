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
#ifndef itkThreadedIteratorRangePartitioner_h
#define itkThreadedIteratorRangePartitioner_h

#include "itkThreadedDomainPartitioner.h"

namespace itk
{

// Forward reference because of circular dependencies
template< typename TIterator >
class ITK_TEMPLATE_EXPORT ThreadedIteratorRangePartitioner;

/** \class ThreadedIteratorRangePartitionerDomain
 * \brief Domain type for the ThreadedIteratorRangePartitioner.
 *
 * Contains a Begin and End iterator.
 *
 * \ingroup ITKCommon
 */
template< typename TIterator >
class ITK_TEMPLATE_EXPORT ThreadedIteratorRangePartitionerDomain
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
 * The \c DomainType is defined to be an itk::ThreadedIteratorRangePartitionerDomain,
 * a two component struct of interators: the first iterator, \c Begin, defines
 * the start of the domain, and the second iterator, \c End, defines one element
 * past the end of the domain.
 *
 * The class assumes that iterating through the domain will be a repeatable
 * process.
 *
 * While this class will work for most containers that use iterators, indexed
 * containers such as std::vector or Array will be partitioned much more efficiently
 * with a ThreadedIndexedContainerPartitioner.
 *
 * This class is typically used as a template argument to a DomainThreader.
 *
 * \sa ThreadedDomainPartitioner
 * \sa DomainThreader
 * \sa ThreadedIndexedContainerPartitioner
 * \ingroup ITKCommon
 */
template< typename TIterator >
class ITK_TEMPLATE_EXPORT ThreadedIteratorRangePartitioner
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

  /** Split the domain \c completeDomain into up to \c requestedTotal
   * non-overlapping subdomains, setting subdomain number \c threadId as
   * \c subDomain and returning the total number of subdomains actually available.
   *
   * This method should be called repeatedly for each value of \c threadId, from 0 up
   * to the return value (which is always less than or equal to \c requestedTotal).
   *
   * It is an error for \c completeDomain to be zero-length.
   * If \c threadId is greater than the return value, the contents of
   * \c subDomain are undefined.
   */
  virtual
  ThreadIdType PartitionDomain(const ThreadIdType threadId,
                           const ThreadIdType requestedTotal,
                           const DomainType& completeDomain,
                           DomainType& subDomain) const ITK_OVERRIDE;

protected:
  ThreadedIteratorRangePartitioner();
  virtual ~ThreadedIteratorRangePartitioner() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ThreadedIteratorRangePartitioner);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThreadedIteratorRangePartitioner.hxx"
#endif

#endif
