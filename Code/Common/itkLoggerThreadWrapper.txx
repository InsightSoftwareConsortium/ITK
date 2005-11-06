/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerThreadWrapper.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include<iostream>
#include "itkLoggerThreadWrapper.h"


namespace itk
{

/** Set the priority level for the current logger. Only messages that have
 * priorities equal or greater than the one set here will be posted to the
 * current outputs */
#if defined(USE_MSVS6_HACKS)
void LoggerThreadWrapper::SetPriorityLevel( PriorityLevelType level )
#else
template < class SimpleLoggerType >
void LoggerThreadWrapper<SimpleLoggerType>::SetPriorityLevel( PriorityLevelType level )
#endif
{
  this->m_WaitMutex.Unlock();
  this->m_Mutex.Lock();
  this->m_OperationQ.push(SET_PRIORITY_LEVEL);
  this->m_LevelQ.push(level);
  this->m_Mutex.Unlock();
  this->m_WaitMutex.Lock();
}

/** Get the priority level for the current logger. Only messages that have
 * priorities equal or greater than the one set here will be posted to the
 * current outputs */
#if defined(USE_MSVS6_HACKS)
SimpleLoggerType::PriorityLevelType LoggerThreadWrapper::GetPriorityLevel() const
#else
template < class SimpleLoggerType >
typename SimpleLoggerType::PriorityLevelType LoggerThreadWrapper<SimpleLoggerType>::GetPriorityLevel() const
#endif
{
  this->m_Mutex.Lock();
  PriorityLevelType level = this->m_PriorityLevel;
  this->m_Mutex.Unlock();
  return level;
}

#if defined(USE_MSVS6_HACKS)
void LoggerThreadWrapper::SetLevelForFlushing( PriorityLevelType level )
#else
template < class SimpleLoggerType >
void LoggerThreadWrapper<SimpleLoggerType>::SetLevelForFlushing( PriorityLevelType level )
#endif
{
  this->m_WaitMutex.Unlock();
  this->m_Mutex.Lock();
  this->m_LevelForFlushing = level;
  this->m_OperationQ.push(SET_LEVEL_FOR_FLUSHING);
  this->m_LevelQ.push(level);
  this->m_Mutex.Unlock();
  this->m_WaitMutex.Lock();
}

#if defined(USE_MSVS6_HACKS)
SimpleLoggerType::PriorityLevelType LoggerThreadWrapper::GetLevelForFlushing() const
#else
template < class SimpleLoggerType >
typename SimpleLoggerType::PriorityLevelType LoggerThreadWrapper<SimpleLoggerType>::GetLevelForFlushing() const
#endif
{
  this->m_Mutex.Lock();
  PriorityLevelType level = this->m_LevelForFlushing;
  this->m_Mutex.Unlock();
  return level;
}

/** Adds an output stream to the MultipleLogOutput for writing. */
#if defined(USE_MSVS6_HACKS)
void LoggerThreadWrapper::AddLogOutput( OutputType* output )
#else
template < class SimpleLoggerType >
void LoggerThreadWrapper<SimpleLoggerType>::AddLogOutput( OutputType* output )
#endif
{
  this->m_WaitMutex.Unlock();
  this->m_Mutex.Lock();
  this->m_OperationQ.push(ADD_LOG_OUTPUT);
  this->m_OutputQ.push(output);
  this->m_Mutex.Unlock();
  this->m_WaitMutex.Lock();
}

#if defined(USE_MSVS6_HACKS)
void LoggerThreadWrapper::Write(PriorityLevelType level, std::string const & content)
#else
template < class SimpleLoggerType >
void LoggerThreadWrapper<SimpleLoggerType>::Write(PriorityLevelType level, std::string const & content)
#endif
{
  this->m_WaitMutex.Unlock();
  this->m_Mutex.Lock();
  if( this->m_PriorityLevel >= level )
    {
    this->m_OperationQ.push(WRITE);
    this->m_MessageQ.push(content);
    this->m_LevelQ.push(level);
    }
  this->m_Mutex.Unlock();
  if( this->m_LevelForFlushing >= level )
    {
    this->Flush();
    }
  this->m_WaitMutex.Lock();
}

#if defined(USE_MSVS6_HACKS)
void LoggerThreadWrapper::Flush()
#else
template < class SimpleLoggerType >
void LoggerThreadWrapper<SimpleLoggerType>::Flush()
#endif
{
  this->m_Mutex.Lock();

    while( !this->m_OperationQ.empty() )
    {
      switch( this->m_OperationQ.front() )
      {
      case Self::SET_PRIORITY_LEVEL:
        this->m_PriorityLevel = this->m_LevelQ.front();
        this->m_LevelQ.pop();
        break;

      case Self::SET_LEVEL_FOR_FLUSHING:
        this->m_LevelForFlushing = this->m_LevelQ.front();
        this->m_LevelQ.pop();
        break;

      case Self::ADD_LOG_OUTPUT:
        this->m_Output->AddLogOutput(this->m_OutputQ.front());
        this->m_OutputQ.pop();
        break;

      case Self::WRITE:
        this->SimpleLoggerType::Write(this->m_LevelQ.front(), this->m_MessageQ.front());
        this->m_LevelQ.pop();
        this->m_MessageQ.pop();
        break;
      case Self::FLUSH:
        this->SimpleLoggerType::Flush();
        break;
      }
      this->m_OperationQ.pop();
    }
    this->m_Output->Flush();
  this->m_Mutex.Unlock();
}


/** Constructor */
#if defined(USE_MSVS6_HACKS)
LoggerThreadWrapper::LoggerThreadWrapper()
#else
template < class SimpleLoggerType >
LoggerThreadWrapper<SimpleLoggerType>::LoggerThreadWrapper()
#endif
{
  this->m_WaitMutex.Lock();
  this->m_Threader = MultiThreader::New();
  this->m_ThreadID = this->m_Threader->SpawnThread(ThreadFunction, this);
}


/** Destructor */
#if defined(USE_MSVS6_HACKS)
LoggerThreadWrapper::~LoggerThreadWrapper()
#else
template < class SimpleLoggerType >
LoggerThreadWrapper<SimpleLoggerType>::~LoggerThreadWrapper()
#endif
{
  this->Flush();
  this->m_WaitMutex.Unlock();
  if( this->m_Threader )
  {
    this->m_Threader->TerminateThread(this->m_ThreadID);
  }
}


#if defined(USE_MSVS6_HACKS)
ITK_THREAD_RETURN_TYPE LoggerThreadWrapper::ThreadFunction(void* pInfoStruct)
#else
template < class SimpleLoggerType >
ITK_THREAD_RETURN_TYPE LoggerThreadWrapper<SimpleLoggerType>::ThreadFunction(void* pInfoStruct)
#endif
{
  struct MultiThreader::ThreadInfoStruct * pInfo = (struct MultiThreader::ThreadInfoStruct*)pInfoStruct;

  if( pInfo == NULL )
  {
    return ITK_THREAD_RETURN_VALUE;
  }

  if( pInfo->UserData == NULL )
  {
    return ITK_THREAD_RETURN_VALUE;
  }

  LoggerThreadWrapper *pLogger = (LoggerThreadWrapper*)pInfo->UserData;

  while(1)
  {

    pLogger->m_WaitMutex.Lock();

    pInfo->ActiveFlagLock->Lock();
    int activeFlag = *pInfo->ActiveFlag;
    pInfo->ActiveFlagLock->Unlock();
    if( !activeFlag )
    {
      break;
    }

    pLogger->m_Mutex.Lock();
    while( !pLogger->m_OperationQ.empty() )
    {
      switch( pLogger->m_OperationQ.front() )
      {
      case Self::SET_PRIORITY_LEVEL:
        pLogger->m_PriorityLevel = pLogger->m_LevelQ.front();
        pLogger->m_LevelQ.pop();
        break;

      case Self::SET_LEVEL_FOR_FLUSHING:
        pLogger->m_LevelForFlushing = pLogger->m_LevelQ.front();
        pLogger->m_LevelQ.pop();
        break;

      case Self::ADD_LOG_OUTPUT:
        pLogger->m_Output->AddLogOutput(pLogger->m_OutputQ.front());
        pLogger->m_OutputQ.pop();
        break;

      case Self::WRITE:
        pLogger->SimpleLoggerType::Write(pLogger->m_LevelQ.front(), pLogger->m_MessageQ.front());
        pLogger->m_LevelQ.pop();
        pLogger->m_MessageQ.pop();
        break;
      case Self::FLUSH:
        pLogger->SimpleLoggerType::Flush();
        break;
      }
      pLogger->m_OperationQ.pop();
    }
    pLogger->m_Mutex.Unlock();
    pLogger->m_WaitMutex.Unlock();
  }
  return ITK_THREAD_RETURN_VALUE;
}


/** Print contents of a LoggerThreadWrapper */
#if defined(USE_MSVS6_HACKS)
void LoggerThreadWrapper::PrintSelf(std::ostream &os, Indent indent) const
#else
template < class SimpleLoggerType >
void LoggerThreadWrapper<SimpleLoggerType>::PrintSelf(std::ostream &os, Indent indent) const
#endif
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Thread ID: " << this->m_ThreadID << std::endl;
  os << indent << "Operation Queue Size: " << this->m_OperationQ.size() << std::endl;
  os << indent << "Message Queue Size: " << this->m_MessageQ.size() << std::endl;
  os << indent << "Level Queue Size: " << this->m_LevelQ.size() << std::endl;
  os << indent << "Output Queue Size: " << this->m_OutputQ.size() << std::endl;
}


} // namespace itk

