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
#ifndef itkThreadedDomainPartitioner_h
#define itkThreadedDomainPartitioner_h

#include "itkIntTypes.h"
#include "itkMultiThreader.h"
#include "itkObject.h"

namespace itk
{

/** \class ThreadedDomainPartitioner
 *  \brief Virtual base class for partitioning a domain into subsets to be
 *  processed per thread when parallel processing.
 *
 * \tparam TDomain The type of the domain to be partitioned.
 *
 * \c PartitionDomain is a method to split the domain into
 * non-overlapping pieces for threading. It must be overridden by derived
 * classes to provide the particular functionality required for
 * \c TDomain type.
 *
 * Subclasses of this class are typically used as template arguments to a DomainThreader.
 *
 * \sa DomainThreader
 *
 * \ingroup DataProcessing
 * \ingroup ITKCommon
 */
template <typename TDomain >
class ITK_TEMPLATE_EXPORT ThreadedDomainPartitioner : public Object
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

  /** Split the domain \c completeDomain into up to \c requestedTotal
   * non-overlapping subdomains, setting subdomain number \c threadId as
   * \c subDomain and returning the total number of subdomains actually available.
   *
   * Subdomains may represent an image region, or a index range for a parameter
   * array, etc, depending on the type of object over which this class is
   * templated.
   *
   * This method should be called repeatedly for each value of \c threadId, from 0 up
   * to the return value (which is always less than or equal to \c requestedTotal).
   */
  virtual
  ThreadIdType PartitionDomain(const ThreadIdType threadId,
                           const ThreadIdType requestedTotal,
                           const DomainType& completeDomain,
                           DomainType& subDomain) const = 0;

protected:
  ThreadedDomainPartitioner(){}
  ~ThreadedDomainPartitioner() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ThreadedDomainPartitioner);
};

} // end namespace itk

#endif
