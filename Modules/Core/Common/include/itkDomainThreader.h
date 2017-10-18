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
#ifndef itkDomainThreader_h
#define itkDomainThreader_h

#include "itkObject.h"
#include "itkMultiThreader.h"

namespace itk
{

/** \class DomainThreader
 *  \brief Multi-threaded processing on a domain by processing sub-domains per
 *  thread.
 *
 *  This class uses a ThreadedDomainPartitioner as a helper to split the
 *  domain into subdomains.  Each subdomain is then processed in the
 *  \c ThreadedExecution method.
 *
 *  The data on which to perform the processing is assumed to be members
 *  of an associating class.  Therefore, to perform a threaded operation in a
 *  class, the associating class usually will declare derived versions of this
 *  class as a friend class.
 *
 *  To use this class, at a minimum,
 *  \li Inherit from it.
 *  \li Implement ThreadedExecution.
 *  \li Create a member instance.
 *  \li Run with m_DomainThreader->Execute( this, domain );
 *
 *  If a 'threaded method' is desired to perform some data processing in a
 *  class, a derived version of this class can be defined to perform the threaded operation.
 *  Since a threaded operation is relatively complex compared to a simple serial
 *  operation, a class instead of a simple method is required.  Inside this
 *  class, the method to partition the data is handled, the logic for
 *  determining the number of threads is determined, and operations surrounding
 *  the threading are encapsulated into the class with the
 *  \c DetermineNumberOfThreadsToUse, \c BeforeThreadedExecution, \c ThreadedExecution,
 *  and \c AfterThreadedExecution virtual methods.
 *
 *  \tparam TDomainPartitioner A class that inherits from
 *  ThreadedDomainPartitioner.
 *  \tparam TAssociate  The associated class that uses a derived version of
 *  this class as a "threaded method".  The associated class usually declares
 *  derived version of this class as nested classes so there is easy access to
 *  its protected and private members in ThreadedExecution.
 *
 *  \ingroup ITKCommon
 */
template< typename TDomainPartitioner, typename TAssociate >
class ITK_TEMPLATE_EXPORT DomainThreader: public Object
{
public:
  /** Standard class typedefs. */
  typedef DomainThreader             Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef TDomainPartitioner                         DomainPartitionerType;
  typedef typename DomainPartitionerType::DomainType DomainType;

  typedef TAssociate AssociateType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( DomainThreader, Object );

  /** Run the multi-threaded operation on the given domain.
   *
   * The domain is first partitioned by the ThreadedDomainPartitioner, then
   * the virtual methods \c BeforeThreadedExecution, \c ThreadedExecution, and
   * \c AfterThreadedExecution. are run, in order. */
  void Execute( AssociateType * enclosingClass, const DomainType & domain );

  /** Set/Get the DomainPartitioner. */
  itkSetObjectMacro(       DomainPartitioner, DomainPartitionerType );
  itkGetModifiableObjectMacro(DomainPartitioner, DomainPartitionerType );

  /** Accessor for number of threads that were actually used in the last
   * ThreadedExecution. */
  itkGetConstMacro( NumberOfThreadsUsed, ThreadIdType );

  /** Return the multithreader used by this class. */
  MultiThreader * GetMultiThreader() const;

  /** Convenience methods to set/get the maximum number of threads to use.
   * \warning When setting the maximum number of threads, it will be clamped by
   * itk::MultiThreader::GetGlobalMaximumNumberOfThreads() and ITK_MAX_THREADS.
   * */
  ThreadIdType GetMaximumNumberOfThreads() const;
  void SetMaximumNumberOfThreads( const ThreadIdType threads );

protected:
  DomainThreader();
  virtual ~DomainThreader() ITK_OVERRIDE;

  /** This is evauated at the beginning of Execute() so that it can be used in
   * BeforeThreadedExecution(). */
  virtual void DetermineNumberOfThreadsUsed();

  /** When \c Execute is run, this method is run singled-threaded before \c
   * ThreadedExecution.  Inside this method optional operations such as
   * creating instance variables needed per thread may be performed. */
  virtual void BeforeThreadedExecution(){}

  /** Do the threaded operation, somewhat like \c ThreadedGenerateData in an
   * ImageSource.
   * \param subdomain The subdomain to operate on.
   * \param threadId  The identifier for the current thread.
   * Data to perform the operation on can be accessed by dereferencing
   * this->m_Associate, which has direct access to private and protected
   * members the enclosing class.
   */
  virtual void ThreadedExecution( const DomainType& subdomain,
                                  const ThreadIdType threadId ) = 0;

  /** When \c Execute in run, this method is run single-threaded after \c
   * ThreadedExecution.  Optionally collect results, etc. E.g. calculate the
   * global minimum from the minimums calculated per thread. */
  virtual void AfterThreadedExecution(){}

  itkSetObjectMacro( MultiThreader, MultiThreader );

  /** Static function used as a "callback" by the MultiThreader.  The threading
   * library will call this routine for each thread, which will delegate the
   * control to the ThreadFunctor. */
  static ITK_THREAD_RETURN_TYPE ThreaderCallback( void *arg );

  AssociateType * m_Associate;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DomainThreader);

  void StartThreadingSequence();

  /** This contains the object passed to the threading library. */
  struct ThreadStruct
    {
    DomainThreader     * domainThreader;
    };

  /** Store the actual number of threads used, which may be less than
   * the number allocated by the threader if the object does not split
   * well into that number.
   * This value is determined at the beginning of \c Execute(). */
  ThreadIdType                             m_NumberOfThreadsUsed;
  typename DomainPartitionerType::Pointer  m_DomainPartitioner;
  DomainType                               m_CompleteDomain;
  MultiThreader::Pointer                   m_MultiThreader;
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDomainThreader.hxx"
#endif

#endif
