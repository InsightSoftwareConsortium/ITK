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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkMultiThreaderBase.h"
#include "itkMultiThreader.h"
#include "itkPoolMultiThreader.h"
#include "itkNumericTraits.h"
#include "itkMutexLockHolder.h"
#include "itkSimpleFastMutexLock.h"
#include "itksys/SystemTools.hxx"
#include "itkImageSourceCommon.h"
#include <iostream>
#include <string>
#include <algorithm>


namespace itk
{
  struct MultiThreaderBaseGlobals
  {
    // Initialize static members.
    MultiThreaderBaseGlobals():GlobalDefaultUseThreadPoolIsInitialized(false),
    m_GlobalDefaultUseThreadPool(true),
    m_GlobalMaximumNumberOfThreads(ITK_MAX_THREADS),
    // Global default number of threads : 0 => Not initialized.
    m_GlobalDefaultNumberOfThreads(0)
    {};
    // GlobalDefaultUseThreadPoolIsInitialized is used only in this
    // file to ensure that the ITK_USE_THREADPOOL environmenal variable
    // is only used as a fall back option.  If the SetGlobalDefaultUseThreadPool
    // API is ever used by the developer, the developers choice is
    // respected over the environmental variable.
    bool GlobalDefaultUseThreadPoolIsInitialized;
    SimpleFastMutexLock globalDefaultInitializerLock;

    // Global value to control weather the threadpool implementation should
    // be used.  This defaults to the environmental variable "ITK_USE_THREADPOOL"
    // if set, else it default to true.
    bool m_GlobalDefaultUseThreadPool;

    // Global variable defining the maximum number of threads that can be used.
    //  The m_GlobalMaximumNumberOfThreads must always be less than or equal to
    //  ITK_MAX_THREADS and greater than zero. */
    ThreadIdType m_GlobalMaximumNumberOfThreads;

    //  Global variable defining the default number of threads to set at
    //  construction time of a MultiThreaderBase instance.  The
    //  m_GlobalDefaultNumberOfThreads must always be less than or equal to the
    //  m_GlobalMaximumNumberOfThreads and larger or equal to 1 once it has been
    //  initialized in the constructor of the first MultiThreaderBase instantiation.
    ThreadIdType m_GlobalDefaultNumberOfThreads;
  };
}//end of itk namespace

namespace
{
static ::itk::SimpleFastMutexLock globalInitializerLock;

/** \brief A function which does nothing
 *
 * This function is to be used to mark parameters as unused to suppress
 * compiler warning. It can be used when the parameter needs to be named
 * (i.e. itkNotUsed cannot be used) but is not always used. It ensures
 * that the parameter is not optimized out.
 */
template <typename T>
void Unused( const T &) {};

// This ensures that m_MultiThreaderBaseGlobals is has been initialized once the library
// has been loaded. In some cases, this call will perform the initialization.
// In other cases, static initializers like the IO factory initialization code
// will have done the initialization.
static ::itk::MultiThreaderBaseGlobals * initializedMultiThreaderBaseGlobals = ::itk::MultiThreaderBase::GetMultiThreaderBaseGlobals();

/** \class MultiThreaderBaseGlobalsInitializer
 *
 * \brief Initialize a MultiThreaderBaseGlobals and delete it on program
 * completion.
 * */
class MultiThreaderBaseGlobalsInitializer
{
public:
  using Self = MultiThreaderBaseGlobalsInitializer;

  MultiThreaderBaseGlobalsInitializer() {}

  /** Delete the time stamp if it was created. */
  ~MultiThreaderBaseGlobalsInitializer()
    {
    delete m_MultiThreaderBaseGlobals;
    m_MultiThreaderBaseGlobals = nullptr;
    }

  /** Create the MultiThreaderBaseGlobals if needed and return it. */
  static ::itk::MultiThreaderBaseGlobals * GetMultiThreaderBaseGlobals()
    {
    if( !m_MultiThreaderBaseGlobals )
      {
      // GetGlobalDefaultUseThreadPool() must be thread safe and can potentially call
      // this method, even though it is very unlikely.
      ::itk::MutexLockHolder< ::itk::SimpleFastMutexLock > lock(globalInitializerLock);
      if( !m_MultiThreaderBaseGlobals )
        {
        m_MultiThreaderBaseGlobals = new ::itk::MultiThreaderBaseGlobals;
        // To avoid being optimized out. The compiler does not like this
        // statement at a higher scope.
        Unused(initializedMultiThreaderBaseGlobals);
        }
      }
    return m_MultiThreaderBaseGlobals;
    }

private:
  static ::itk::MultiThreaderBaseGlobals * m_MultiThreaderBaseGlobals;
};

// Takes care of cleaning up the MultiThreaderBaseGlobals
static MultiThreaderBaseGlobalsInitializer MultiThreaderBaseGlobalsInitializerInstance;
// Initialized by the compiler to zero
::itk::MultiThreaderBaseGlobals * MultiThreaderBaseGlobalsInitializer::m_MultiThreaderBaseGlobals;

} // end anonymous namespace


namespace itk
{

::itk::MultiThreaderBaseGlobals *
MultiThreaderBase
::GetMultiThreaderBaseGlobals()
{
  if( m_MultiThreaderBaseGlobals == nullptr )
    {
    m_MultiThreaderBaseGlobals = MultiThreaderBaseGlobalsInitializer::GetMultiThreaderBaseGlobals();
    }
  return m_MultiThreaderBaseGlobals;
}


void
MultiThreaderBase
::SetMultiThreaderBaseGlobals( MultiThreaderBaseGlobals * multiThreaderBaseGlobals )
{
  m_MultiThreaderBaseGlobals = multiThreaderBaseGlobals;
}


void MultiThreaderBase::SetGlobalDefaultUseThreadPool( const bool GlobalDefaultUseThreadPool )
  {
  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals = GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);
  m_MultiThreaderBaseGlobals->m_GlobalDefaultUseThreadPool = GlobalDefaultUseThreadPool;
  m_MultiThreaderBaseGlobals->GlobalDefaultUseThreadPoolIsInitialized=true;
  }

bool MultiThreaderBase::GetGlobalDefaultUseThreadPool( )
  {
  // This method must be concurrent thread safe

  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals = GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);

  if( !m_MultiThreaderBaseGlobals->GlobalDefaultUseThreadPoolIsInitialized )
    {

    MutexLockHolder< SimpleFastMutexLock > lock(m_MultiThreaderBaseGlobals->globalDefaultInitializerLock);

    // After we have the lock, double check the initialization
    // flag to ensure it hasn't been changed by another thread.

    if (!m_MultiThreaderBaseGlobals->GlobalDefaultUseThreadPoolIsInitialized )
      {
      // look for runtime request to use thread pool
      std::string use_threadpool;

      if( itksys::SystemTools::GetEnv("ITK_USE_THREADPOOL",use_threadpool) )
        {

        use_threadpool = itksys::SystemTools::UpperCase(use_threadpool);

        // NOTE: m_MultiThreaderBaseGlobals->GlobalDefaultUseThreadPoolIsInitialized=true after this call
        if(use_threadpool != "NO" && use_threadpool != "OFF" && use_threadpool != "FALSE")
          {
          MultiThreaderBase::SetGlobalDefaultUseThreadPool( true );
          }
        else
          {
          MultiThreaderBase::SetGlobalDefaultUseThreadPool( false );
          }
        }

      // always set that we are initialized
      m_MultiThreaderBaseGlobals->GlobalDefaultUseThreadPoolIsInitialized=true;
      }
    }
  return m_MultiThreaderBaseGlobals->m_GlobalDefaultUseThreadPool;
  }

void MultiThreaderBase::SetGlobalMaximumNumberOfThreads(ThreadIdType val)
{
  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals = GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);

  m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads = val;

  // clamp between 1 and ITK_MAX_THREADS
  m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads =
    std::min( m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads,
             (ThreadIdType) ITK_MAX_THREADS );
  m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads =
    std::max( m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads,
              NumericTraits<ThreadIdType>::OneValue() );

  // If necessary reset the default to be used from now on.
  m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads =
    std::min( m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads,
              m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads);
}

ThreadIdType MultiThreaderBase::GetGlobalMaximumNumberOfThreads()
{
  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals = GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);
  return m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads;
}

void MultiThreaderBase::SetGlobalDefaultNumberOfThreads(ThreadIdType val)
{
  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals = GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);

  m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads = val;

  // clamp between 1 and m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads
  m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads  =
    std::min( m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads,
              m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads );
  m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads  =
    std::max( m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads,
              NumericTraits<ThreadIdType>::OneValue() );

}

void MultiThreaderBase::SetNumberOfThreads(ThreadIdType numberOfThreads)
{
  if( m_NumberOfThreads == numberOfThreads &&
      numberOfThreads <= m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads )
    {
    return;
    }

  m_NumberOfThreads = numberOfThreads;

  // clamp between 1 and m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads
  m_NumberOfThreads  = std::min( m_NumberOfThreads,
                                 m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads );
  m_NumberOfThreads  = std::max( m_NumberOfThreads, NumericTraits<ThreadIdType>::OneValue() );

}

ThreadIdType MultiThreaderBase::GetGlobalDefaultNumberOfThreads()
{
  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals =
    GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);

  if( m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads == 0 ) //need to initialize
    {
    m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads = ThreadPool::GetGlobalDefaultNumberOfThreads();
    }
  return m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads;
}


MultiThreaderBase::Pointer MultiThreaderBase::New()
{
  Pointer smartPtr = ::itk::ObjectFactory< MultiThreaderBase >::Create();
  if ( smartPtr == nullptr )
    {
#if defined(ITK_USE_TBB)
    //return TBBMultiThreader::New().GetPointer();
#endif
    if ( GetGlobalDefaultUseThreadPool() )
      {
      return PoolMultiThreader::New();
      }
    else
      {
      return MultiThreader::New();
      }
    }
  smartPtr->UnRegister();
  return smartPtr;
}


MultiThreaderBase::MultiThreaderBase()
{
  m_NumberOfThreads = MultiThreaderBase::GetGlobalDefaultNumberOfThreads();
}

MultiThreaderBase::~MultiThreaderBase()
{
}

ITK_THREAD_RETURN_TYPE
MultiThreaderBase
::SingleMethodProxy(void *arg)
{
  // grab the ThreadInfoStruct originally prescribed
  auto * threadInfoStruct = static_cast<MultiThreaderBase::ThreadInfoStruct *>( arg );

  // execute the user specified threader callback, catching any exceptions
  try
    {
    ( *threadInfoStruct->ThreadFunction )(arg);
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::SUCCESS;
    }
  catch( ProcessAborted & )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::ITK_PROCESS_ABORTED_EXCEPTION;
    }
  catch( ExceptionObject & )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::ITK_EXCEPTION;
    }
  catch( std::exception & )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::STD_EXCEPTION;
    }
  catch( ... )
    {
    threadInfoStruct->ThreadExitCode = ThreadInfoStruct::UNKNOWN;
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE
MultiThreaderBase
::ParallelizeImageRegionHelper(void * arg)
{
  using ThreadInfo = MultiThreaderBase::ThreadInfoStruct;
  auto * threadInfo = static_cast<ThreadInfo *>(arg);
  ThreadIdType threadId = threadInfo->ThreadID;
  ThreadIdType threadCount = threadInfo->NumberOfThreads;
  auto * rnc = static_cast<RegionAndCallback *>(threadInfo->UserData);

  const ImageRegionSplitterBase * splitter = ImageSourceCommon::GetGlobalDefaultSplitter();
  ImageIORegion region(std::get<1>(*rnc));
  for (unsigned d = 0; d < std::get<1>(*rnc); d++)
    {
    region.SetIndex(d, std::get<2>(*rnc)[d]);
    region.SetSize(d, std::get<3>(*rnc)[d]);
    }
  ThreadIdType total = splitter->GetSplit(threadId, threadCount, region);

  if ( threadId < total )
    {
    std::get<0>(*rnc)(&region.GetIndex()[0], &region.GetSize()[0]);
    }
  return ITK_THREAD_RETURN_VALUE;
}

// Print method for the multithreader
void MultiThreaderBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Thread Count: " << m_NumberOfThreads << "\n";
  os << indent << "Global Maximum Number Of Threads: "
     << m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads << std::endl;
  os << indent << "Global Default Number Of Threads: "
     << m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads << std::endl;
  os << indent << "Global Default Use ThreadPool: "
     << m_MultiThreaderBaseGlobals->m_GlobalDefaultUseThreadPool << std::endl;
  os << indent << "SingleMethod: " << m_SingleMethod << std::endl;
  os << indent << "SingleData: " << m_SingleData << std::endl;
}

MultiThreaderBaseGlobals * MultiThreaderBase::m_MultiThreaderBaseGlobals;

}
