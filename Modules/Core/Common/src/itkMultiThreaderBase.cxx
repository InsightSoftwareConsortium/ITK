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
#include "itkPlatformMultiThreader.h"
#include "itkPoolMultiThreader.h"
#include "itkNumericTraits.h"
#include <mutex>
#include <mutex>
#include "itksys/SystemTools.hxx"
#include "itkImageSourceCommon.h"
#include "itkProcessObject.h"
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

#if defined(ITK_USE_TBB)
#include "itkTBBMultiThreader.h"
#endif


namespace itk
{
  struct MultiThreaderBaseGlobals
  {
    // Initialize static members.
    MultiThreaderBaseGlobals():GlobalDefaultThreaderTypeIsInitialized(false),
#if defined(ITK_USE_TBB)
    m_GlobalDefaultThreader(MultiThreaderBase::ThreaderType::TBB),
#else
    m_GlobalDefaultThreader(MultiThreaderBase::ThreaderType::Pool),
#endif
    m_GlobalMaximumNumberOfThreads(ITK_MAX_THREADS),
    // Global default number of threads : 0 => Not initialized.
    m_GlobalDefaultNumberOfThreads(0)
    {};
    // GlobalDefaultThreaderTypeIsInitialized is used only in this
    // file to ensure that the ITK_GLOBAL_DEFAULT_THREADER or
    // ITK_USE_THREADPOOL environmenal variables are
    // only used as a fall back option.  If the SetGlobalDefaultThreaderType
    // API is ever used by the developer, the developers choice is
    // respected over the environmental variable.
    bool GlobalDefaultThreaderTypeIsInitialized;
    std::mutex globalDefaultInitializerLock;

    // Global value to control weather the threadpool implementation should
    // be used. This defaults to the environmental variable
    // ITK_GLOBAL_DEFAULT_THREADER. If that is not present, then
    // ITK_USE_THREADPOOL is examined.
    MultiThreaderBase::ThreaderType m_GlobalDefaultThreader;

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
static std::mutex globalInitializerLock;

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

  MultiThreaderBaseGlobalsInitializer() = default;

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
      // GetGlobalDefaultThreaderType() must be thread safe and can potentially call
      // this method, even though it is very unlikely.
      std::lock_guard< std::mutex > lock(globalInitializerLock);
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

#if ! defined (ITK_LEGACY_REMOVE)
void MultiThreaderBase::SetGlobalDefaultUseThreadPool( const bool GlobalDefaultUseThreadPool )
{
  if (GlobalDefaultUseThreadPool)
    {
    SetGlobalDefaultThreader(ThreaderType::Pool);
    }
  else
    {
    SetGlobalDefaultThreader(ThreaderType::Platform);
    }
}

bool MultiThreaderBase::GetGlobalDefaultUseThreadPool( )
{
  return (GetGlobalDefaultThreader() == ThreaderType::Pool);
}
#endif

void MultiThreaderBase::SetGlobalDefaultThreader(ThreaderType threaderType)
{
  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals = GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);
  m_MultiThreaderBaseGlobals->m_GlobalDefaultThreader = threaderType;
  m_MultiThreaderBaseGlobals->GlobalDefaultThreaderTypeIsInitialized = true;
}

MultiThreaderBase::ThreaderType
MultiThreaderBase
::GetGlobalDefaultThreader()
{
  // This method must be concurrent thread safe

  // This is called once, on-demand to ensure that m_MultiThreaderBaseGlobals is
  // initialized.
  static MultiThreaderBaseGlobals * multiThreaderBaseGlobals = GetMultiThreaderBaseGlobals();
  Unused(multiThreaderBaseGlobals);

  if( !m_MultiThreaderBaseGlobals->GlobalDefaultThreaderTypeIsInitialized )
    {
    std::lock_guard< std::mutex > lock(m_MultiThreaderBaseGlobals->globalDefaultInitializerLock);

    // After we have the lock, double check the initialization
    // flag to ensure it hasn't been changed by another thread.
    if (!m_MultiThreaderBaseGlobals->GlobalDefaultThreaderTypeIsInitialized )
      {
      std::string envVar;
      // first check ITK_GLOBAL_DEFAULT_THREADER
      if ( itksys::SystemTools::GetEnv("ITK_GLOBAL_DEFAULT_THREADER", envVar) )
        {
        envVar = itksys::SystemTools::UpperCase(envVar);
        ThreaderType threaderT = ThreaderTypeFromString(envVar);
        if (threaderT != ThreaderType::Unknown)
          {
          MultiThreaderBase::SetGlobalDefaultThreader(threaderT);
          }
        }
      // if that was not set check ITK_USE_THREADPOOL (deprecated)
      else if( !m_MultiThreaderBaseGlobals->GlobalDefaultThreaderTypeIsInitialized
          && itksys::SystemTools::GetEnv("ITK_USE_THREADPOOL",envVar) )
        {
        envVar = itksys::SystemTools::UpperCase(envVar);
        itkGenericOutputMacro("Warning: ITK_USE_THREADPOOL \
has been deprecated since ITK v5.0. \
You should now use ITK_GLOBAL_DEFAULT_THREADER\
\nFor example ITK_GLOBAL_DEFAULT_THREADER=Pool");
        if(envVar != "NO" && envVar != "OFF" && envVar != "FALSE")
          {
#ifdef __EMSCRIPTEN__
          MultiThreaderBase::SetGlobalDefaultThreader(ThreaderType::Platform);
#else
          MultiThreaderBase::SetGlobalDefaultThreader(ThreaderType::Pool);
#endif
          }
        else
          {
          MultiThreaderBase::SetGlobalDefaultThreader(ThreaderType::Platform);
          }
        }

      // always set that we are initialized
      m_MultiThreaderBaseGlobals->GlobalDefaultThreaderTypeIsInitialized=true;
      }
    }
  return m_MultiThreaderBaseGlobals->m_GlobalDefaultThreader;
}

MultiThreaderBase::ThreaderType
MultiThreaderBase
::ThreaderTypeFromString(std::string threaderString)
{
  threaderString = itksys::SystemTools::UpperCase(threaderString);
  if (threaderString == "PLATFORM")
    {
    return ThreaderType::Platform;
    }
  else if (threaderString == "POOL")
    {
    return ThreaderType::Pool;
    }
  else if (threaderString == "TBB")
    {
    return ThreaderType::TBB;
    }
  else
    {
    return ThreaderType::Unknown;
    }
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

void MultiThreaderBase::SetMaximumNumberOfThreads( ThreadIdType numberOfThreads )
{
  if( m_MaximumNumberOfThreads == numberOfThreads &&
      numberOfThreads <= m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads )
    {
    return;
    }

  m_MaximumNumberOfThreads = numberOfThreads;

  // clamp between 1 and m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads
  m_MaximumNumberOfThreads = std::min( m_MaximumNumberOfThreads, m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads );
  m_MaximumNumberOfThreads = std::max( m_MaximumNumberOfThreads, NumericTraits< ThreadIdType >::OneValue() );
}

void MultiThreaderBase::SetNumberOfWorkUnits(ThreadIdType numberOfWorkUnits)
{
  if( m_NumberOfWorkUnits == numberOfWorkUnits &&
      numberOfWorkUnits <= m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads )
    {
    return;
    }

  m_NumberOfWorkUnits = numberOfWorkUnits;

  // clamp between 1 and m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads
  m_NumberOfWorkUnits  = std::min( m_NumberOfWorkUnits,
                                   m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads );
  m_NumberOfWorkUnits  = std::max( m_NumberOfWorkUnits, NumericTraits<ThreadIdType>::OneValue() );

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
    ThreaderType threaderType = GetGlobalDefaultThreader();
    switch (threaderType)
      {
      case ThreaderType::Platform:
        return PlatformMultiThreader::New();
      case ThreaderType::Pool:
        return PoolMultiThreader::New();
      case ThreaderType::TBB:
#if defined(ITK_USE_TBB)
        return TBBMultiThreader::New();
#else
        itkGenericExceptionMacro("ITK has been built without TBB support!");
#endif
      default:
        itkGenericExceptionMacro("MultiThreaderBase::GetGlobalDefaultThreader returned Unknown!");
      }
    }
  smartPtr->UnRegister();
  return smartPtr;
}


MultiThreaderBase::MultiThreaderBase()
  : m_SingleMethod{ nullptr }
  , m_SingleData{ nullptr }
{
  m_MaximumNumberOfThreads = MultiThreaderBase::GetGlobalDefaultNumberOfThreads();
  m_NumberOfWorkUnits = m_MaximumNumberOfThreads;
}

MultiThreaderBase::~MultiThreaderBase() = default;

ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
MultiThreaderBase
::SingleMethodProxy(void *arg)
{
  // grab the WorkUnitInfo originally prescribed
  auto * threadInfoStruct = static_cast<MultiThreaderBase::WorkUnitInfo *>( arg );

  // execute the user specified threader callback, catching any exceptions
  try
    {
    ( *threadInfoStruct->ThreadFunction )(arg);
    threadInfoStruct->ThreadExitCode = WorkUnitInfo::SUCCESS;
    }
  catch( ProcessAborted & )
    {
    threadInfoStruct->ThreadExitCode = WorkUnitInfo::ITK_PROCESS_ABORTED_EXCEPTION;
    }
  catch( ExceptionObject & )
    {
    threadInfoStruct->ThreadExitCode = WorkUnitInfo::ITK_EXCEPTION;
    }
  catch( std::exception & )
    {
    threadInfoStruct->ThreadExitCode = WorkUnitInfo::STD_EXCEPTION;
    }
  catch( ... )
    {
    threadInfoStruct->ThreadExitCode = WorkUnitInfo::UNKNOWN;
    }

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}


void
MultiThreaderBase
::HandleFilterProgress(ProcessObject *filter, float progress)
{
  if (filter)
    {
    if (progress >= 0.0f)
      {
      filter->UpdateProgress(progress);
      }
    if (filter->GetAbortGenerateData())
      {
      std::string msg;
      ProcessAborted e(__FILE__, __LINE__);
      msg += "AbortGenerateData was called in " + std::string(filter->GetNameOfClass() )
          + " during multi-threaded part of filter execution";
      e.SetDescription(msg);
      throw e;
      }
    }
}

void
MultiThreaderBase
::ParallelizeArray(
  SizeValueType firstIndex,
  SizeValueType lastIndexPlus1,
  ArrayThreadingFunctorType aFunc,
  ProcessObject* filter )
{
  // This implementation simply delegates parallelization to the old interface
  // SetSingleMethod+SingleMethodExecute. This method is meant to be overloaded!
  MultiThreaderBase::HandleFilterProgress(filter, 0.0f);

  if ( firstIndex + 1 < lastIndexPlus1 )
    {
    struct ArrayCallback acParams {
        aFunc,
        firstIndex,
        lastIndexPlus1,
        filter,
        std::this_thread::get_id(),
        {0} };
    this->SetSingleMethod(&MultiThreaderBase::ParallelizeArrayHelper, &acParams);
    this->SingleMethodExecute();
    }
  else if ( firstIndex + 1 == lastIndexPlus1 )
    {
    aFunc( firstIndex );
    }
  // else nothing needs to be executed

  MultiThreaderBase::HandleFilterProgress(filter, 1.0f);
}

ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
MultiThreaderBase
::ParallelizeArrayHelper(void * arg)
{
  using ThreadInfo = MultiThreaderBase::WorkUnitInfo;
  auto* threadInfo = static_cast< ThreadInfo* >( arg );
  ThreadIdType threadId = threadInfo->WorkUnitID;
  ThreadIdType threadCount = threadInfo->NumberOfWorkUnits;
  auto* acParams = static_cast< struct ArrayCallback* >( threadInfo->UserData );

  MultiThreaderBase::HandleFilterProgress(acParams->filter);

  SizeValueType range = acParams->lastIndexPlus1 - acParams->firstIndex;
  double fraction = double( range ) / threadCount;
  SizeValueType first = acParams->firstIndex + fraction * threadId;
  SizeValueType afterLast = acParams->firstIndex + fraction * ( threadId + 1 );
  if ( threadId == threadCount - 1 ) // last thread
    {
    // Avoid possible problems due to floating point arithmetic
    afterLast = acParams->lastIndexPlus1;
    }

  for ( SizeValueType i = first; i < afterLast; i++ )
    {
    acParams->functor( i );
    if ( acParams->filter )
      {
      ++acParams->progress;
      //make sure we are updating progress only from the thead which invoked us
      if ( acParams->callingThread == std::this_thread::get_id() )
        {
        acParams->filter->UpdateProgress( float( acParams->progress ) / range );
        }
      }
    }

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}


void
MultiThreaderBase
::ParallelizeImageRegion(
    unsigned int dimension,
    const IndexValueType index[],
    const SizeValueType size[],
    MultiThreaderBase::ThreadingFunctorType funcP,
    ProcessObject* filter)
{
  // This implementation simply delegates parallelization to the old interface
  // SetSingleMethod+SingleMethodExecute. This method is meant to be overloaded!
  MultiThreaderBase::HandleFilterProgress(filter, 0.0f);

  SizeValueType pixelCount = 1;
  for (unsigned d = 0; d < dimension; d++)
    {
    pixelCount *= size[d];
    }
  struct RegionAndCallback rnc {
      funcP,
      dimension,
      index,
      size,
      filter,
      std::this_thread::get_id(),
      pixelCount,
      {0} };
  this->SetSingleMethod(&MultiThreaderBase::ParallelizeImageRegionHelper, &rnc);
  this->SingleMethodExecute();

  MultiThreaderBase::HandleFilterProgress(filter, 1.0f);
}

ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
MultiThreaderBase
::ParallelizeImageRegionHelper(void * arg)
{
  using ThreadInfo = MultiThreaderBase::WorkUnitInfo;
  auto * threadInfo = static_cast<ThreadInfo *>(arg);
  ThreadIdType threadId = threadInfo->WorkUnitID;
  ThreadIdType threadCount = threadInfo->NumberOfWorkUnits;
  auto * rnc = static_cast<struct RegionAndCallback *>(threadInfo->UserData);

  const ImageRegionSplitterBase * splitter = ImageSourceCommon::GetGlobalDefaultSplitter();
  ImageIORegion region(rnc->dimension);
  for (unsigned d = 0; d < rnc->dimension; d++)
    {
    region.SetIndex(d, rnc->index[d]);
    region.SetSize(d, rnc->size[d]);
    }
  ThreadIdType total = splitter->GetSplit(threadId, threadCount, region);

  MultiThreaderBase::HandleFilterProgress(rnc->filter);

  if ( threadId < total )
    {
    rnc->functor(&region.GetIndex()[0], &region.GetSize()[0]);
    if (rnc->filter)
      {
      SizeValueType pixelCount = region.GetNumberOfPixels();
      rnc->pixelProgress += pixelCount;
      //make sure we are updating progress only from the thead which invoked filter->Update();
      if (rnc->callingThread == std::this_thread::get_id())
        {
        rnc->filter->UpdateProgress(float(rnc->pixelProgress) / rnc->pixelCount);
        }
      }
    }

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

std::ostream& operator << (std::ostream& os,
    const MultiThreaderBase::ThreaderType& threader)
{
  os << MultiThreaderBase::ThreaderTypeToString(threader) << "MultiThreader";
  return os;
}

// Print method for the multithreader
void MultiThreaderBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of Work Units: " << m_NumberOfWorkUnits << "\n";
  os << indent << "Number of Threads: " << m_MaximumNumberOfThreads << "\n";
  os << indent << "Global Maximum Number Of Threads: "
     << m_MultiThreaderBaseGlobals->m_GlobalMaximumNumberOfThreads << std::endl;
  os << indent << "Global Default Number Of Threads: "
     << m_MultiThreaderBaseGlobals->m_GlobalDefaultNumberOfThreads << std::endl;
  os << indent << "Global Default Threader Type: "
     << m_MultiThreaderBaseGlobals->m_GlobalDefaultThreader << std::endl;
  os << indent << "SingleMethod: " << m_SingleMethod << std::endl;
  os << indent << "SingleData: " << m_SingleData << std::endl;
}

MultiThreaderBaseGlobals * MultiThreaderBase::m_MultiThreaderBaseGlobals;

}
