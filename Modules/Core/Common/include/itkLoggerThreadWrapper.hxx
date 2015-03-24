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
#ifndef itkLoggerThreadWrapper_hxx
#define itkLoggerThreadWrapper_hxx

#include <iostream>
#include "itkLoggerThreadWrapper.h"
#include "itksys/SystemTools.hxx"

namespace itk
{
/** Set the priority level for the current logger. Only messages that have
 * priorities equal or greater than the one set here will be posted to the
 * current outputs */
template< typename SimpleLoggerType >
void LoggerThreadWrapper< SimpleLoggerType >::SetPriorityLevel(PriorityLevelType level)
{
  this->m_Mutex.Lock();
  this->m_OperationQ.push(SET_PRIORITY_LEVEL);
  this->m_LevelQ.push(level);
  this->m_Mutex.Unlock();
}

/** Get the priority level for the current logger. Only messages that have
 * priorities equal or greater than the one set here will be posted to the
 * current outputs */
template< typename SimpleLoggerType >
typename SimpleLoggerType::PriorityLevelType LoggerThreadWrapper< SimpleLoggerType >::GetPriorityLevel() const
{
  this->m_Mutex.Lock();
  PriorityLevelType level = this->m_PriorityLevel;
  this->m_Mutex.Unlock();
  return level;
}

template< typename SimpleLoggerType >
void LoggerThreadWrapper< SimpleLoggerType >::SetLevelForFlushing(PriorityLevelType level)
{
  this->m_Mutex.Lock();
  this->m_LevelForFlushing = level;
  this->m_OperationQ.push(SET_LEVEL_FOR_FLUSHING);
  this->m_LevelQ.push(level);
  this->m_Mutex.Unlock();
}

template< typename SimpleLoggerType >
typename SimpleLoggerType::PriorityLevelType LoggerThreadWrapper< SimpleLoggerType >::GetLevelForFlushing() const
{
  this->m_Mutex.Lock();
  PriorityLevelType level = this->m_LevelForFlushing;
  this->m_Mutex.Unlock();
  return level;
}

template< typename SimpleLoggerType >
void LoggerThreadWrapper< SimpleLoggerType >::SetDelay(DelayType delay)
{
  this->m_Mutex.Lock();
  this->m_Delay = delay;
  this->m_Mutex.Unlock();
}

template< typename SimpleLoggerType >
typename LoggerThreadWrapper< SimpleLoggerType >::DelayType LoggerThreadWrapper< SimpleLoggerType >::GetDelay() const
{
  this->m_Mutex.Lock();
  DelayType delay = this->m_Delay;
  this->m_Mutex.Unlock();
  return delay;
}

/** Adds an output stream to the MultipleLogOutput for writing. */
template< typename SimpleLoggerType >
void LoggerThreadWrapper< SimpleLoggerType >::AddLogOutput(OutputType *output)
{
  this->m_Mutex.Lock();
  this->m_OperationQ.push(ADD_LOG_OUTPUT);
  this->m_OutputQ.push(output);
  this->m_Mutex.Unlock();
}

template< typename SimpleLoggerType >
void LoggerThreadWrapper< SimpleLoggerType >::Write(PriorityLevelType level, std::string const & content)
{
  this->m_Mutex.Lock();
  if ( this->m_PriorityLevel >= level )
    {
    this->m_OperationQ.push(WRITE);
    this->m_MessageQ.push(content);
    this->m_LevelQ.push(level);
    }
  this->m_Mutex.Unlock();
  if ( this->m_LevelForFlushing >= level )
    {
    this->Flush();
    }
}

template< typename SimpleLoggerType >
void LoggerThreadWrapper< SimpleLoggerType >::Flush()
{
  this->m_Mutex.Lock();

  while ( !this->m_OperationQ.empty() )
    {
    switch ( this->m_OperationQ.front() )
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
        this->m_Output->AddLogOutput( this->m_OutputQ.front() );
        this->m_OutputQ.pop();
        break;

      case Self::WRITE:
        this->SimpleLoggerType::Write( this->m_LevelQ.front(), this->m_MessageQ.front() );
        this->m_LevelQ.pop();
        this->m_MessageQ.pop();
        break;
      }
    this->m_OperationQ.pop();
    }
  this->SimpleLoggerType::Flush();
  this->m_Output->Flush();
  this->m_Mutex.Unlock();
}

/** Constructor */
template< typename SimpleLoggerType >
LoggerThreadWrapper< SimpleLoggerType >::LoggerThreadWrapper()
{
  this->m_Delay = 300; // ms
  this->m_Threader = MultiThreader::New();
  this->m_ThreadID = this->m_Threader->SpawnThread(ThreadFunction, this);
}

/** Destructor */
template< typename SimpleLoggerType >
LoggerThreadWrapper< SimpleLoggerType >::~LoggerThreadWrapper()
{
  this->Flush();
  if ( this->m_Threader )
    {
    this->m_Threader->TerminateThread(this->m_ThreadID);
    }
}

template< typename SimpleLoggerType >
ITK_THREAD_RETURN_TYPE LoggerThreadWrapper< SimpleLoggerType >::ThreadFunction(void *pInfoStruct)
{
  struct MultiThreader:: ThreadInfoStruct *pInfo = (struct MultiThreader::ThreadInfoStruct *)pInfoStruct;

  if ( ( pInfo != ITK_NULLPTR ) && ( pInfo->UserData != ITK_NULLPTR ) )
    {

    LoggerThreadWrapper *pLogger = (LoggerThreadWrapper *)pInfo->UserData;

    while ( 1 )
      {

      pInfo->ActiveFlagLock->Lock();
      int activeFlag = *pInfo->ActiveFlag;
      pInfo->ActiveFlagLock->Unlock();
      if ( !activeFlag )
        {
        break;
        }

      pLogger->m_Mutex.Lock();
      while ( !pLogger->m_OperationQ.empty() )
        {
        switch ( pLogger->m_OperationQ.front() )
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
            pLogger->m_Output->AddLogOutput( pLogger->m_OutputQ.front() );
            pLogger->m_OutputQ.pop();
            break;

          case Self::WRITE:
            pLogger->SimpleLoggerType::Write( pLogger->m_LevelQ.front(), pLogger->m_MessageQ.front() );
            pLogger->m_LevelQ.pop();
            pLogger->m_MessageQ.pop();
            break;
          }
        pLogger->m_OperationQ.pop();
        }
      pLogger->m_Mutex.Unlock();
      pLogger->SimpleLoggerType::Flush();
      itksys::SystemTools::Delay(pLogger->GetDelay());
      }
    }
  return ITK_THREAD_RETURN_VALUE;
}

/** Print contents of a LoggerThreadWrapper */
template< typename SimpleLoggerType >
void LoggerThreadWrapper< SimpleLoggerType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Thread ID: " << this->m_ThreadID << std::endl;
  os << indent << "Low-priority Message Delay: " << this->m_Delay << std::endl;
  os << indent << "Operation Queue Size: " << this->m_OperationQ.size() << std::endl;
  os << indent << "Message Queue Size: " << this->m_MessageQ.size() << std::endl;
  os << indent << "Level Queue Size: " << this->m_LevelQ.size() << std::endl;
  os << indent << "Output Queue Size: " << this->m_OutputQ.size() << std::endl;
}
} // namespace itk

#endif // itkLoggerThreadWrapper_hxx
