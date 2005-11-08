/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThreadLogger.cxx
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
#include "itkThreadLogger.h"


namespace itk
{

/** Set the priority level for the current logger. Only messages that have
 * priorities equal or greater than the one set here will be posted to the
 * current outputs */
void ThreadLogger::SetPriorityLevel( PriorityLevelType level )
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
Logger::PriorityLevelType ThreadLogger::GetPriorityLevel() const
{
  this->m_Mutex.Lock();
  PriorityLevelType level = this->m_PriorityLevel;
  this->m_Mutex.Unlock();
  return level;
}

void ThreadLogger::SetLevelForFlushing( PriorityLevelType level )
{
  this->m_WaitMutex.Unlock();
  this->m_Mutex.Lock();
  this->m_LevelForFlushing = level;
  this->m_OperationQ.push(SET_LEVEL_FOR_FLUSHING);
  this->m_LevelQ.push(level);
  this->m_Mutex.Unlock();
  this->m_WaitMutex.Lock();
}

Logger::PriorityLevelType ThreadLogger::GetLevelForFlushing() const
{
  this->m_Mutex.Lock();
  PriorityLevelType level = this->m_LevelForFlushing;
  this->m_Mutex.Unlock();
  return level;
}

/** Adds an output stream to the MultipleLogOutput for writing. */
void ThreadLogger::AddLogOutput( OutputType* output )
{
  this->m_WaitMutex.Unlock();
  this->m_Mutex.Lock();
  this->m_OperationQ.push(ADD_LOG_OUTPUT);
  this->m_OutputQ.push(output);
  this->m_Mutex.Unlock();
  this->m_WaitMutex.Lock();
}

void ThreadLogger::Write(PriorityLevelType level, std::string const & content)
{
  this->m_WaitMutex.Unlock();
  this->m_Mutex.Lock();
  this->m_OperationQ.push(WRITE);
  this->m_MessageQ.push(content);
  this->m_LevelQ.push(level);
  this->m_Mutex.Unlock();
  this->m_WaitMutex.Lock();
}


void ThreadLogger::Flush()
{
  this->m_Mutex.Lock();

    while( !this->m_OperationQ.empty() )
    {
      switch( this->m_OperationQ.front() )
      {
      case ThreadLogger::SET_PRIORITY_LEVEL:
        this->m_PriorityLevel = this->m_LevelQ.front();
        this->m_LevelQ.pop();
        break;

      case ThreadLogger::SET_LEVEL_FOR_FLUSHING:
        this->m_LevelForFlushing = this->m_LevelQ.front();
        this->m_LevelQ.pop();
        break;

      case ThreadLogger::ADD_LOG_OUTPUT:
        this->m_Output->AddLogOutput(this->m_OutputQ.front());
        this->m_OutputQ.pop();
        break;

      case ThreadLogger::WRITE:
        this->Logger::Write(this->m_LevelQ.front(), this->m_MessageQ.front());
        this->m_LevelQ.pop();
        this->m_MessageQ.pop();
        break;
      case ThreadLogger::FLUSH:
        this->Logger::Flush();
        break;
      }
      this->m_OperationQ.pop();
    }
    this->m_Output->Flush();
  this->m_Mutex.Unlock();

}


/** Constructor */
ThreadLogger::ThreadLogger()
{
  this->m_WaitMutex.Lock();
  this->m_Threader = MultiThreader::New();
  this->m_ThreadID = this->m_Threader->SpawnThread(ThreadFunction, this);
}


/** Destructor */
ThreadLogger::~ThreadLogger()
{
  this->m_WaitMutex.Unlock();
  if( this->m_Threader )
  {
    this->m_Threader->TerminateThread(this->m_ThreadID);
  }
}


ITK_THREAD_RETURN_TYPE ThreadLogger::ThreadFunction(void* pInfoStruct)
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

  ThreadLogger *pLogger = (ThreadLogger*)pInfo->UserData;

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
      case ThreadLogger::SET_PRIORITY_LEVEL:
        pLogger->m_PriorityLevel = pLogger->m_LevelQ.front();
        pLogger->m_LevelQ.pop();
        break;

      case ThreadLogger::SET_LEVEL_FOR_FLUSHING:
        pLogger->m_LevelForFlushing = pLogger->m_LevelQ.front();
        pLogger->m_LevelQ.pop();
        break;

      case ThreadLogger::ADD_LOG_OUTPUT:
        pLogger->m_Output->AddLogOutput(pLogger->m_OutputQ.front());
        pLogger->m_OutputQ.pop();
        break;

      case ThreadLogger::WRITE:
        pLogger->Logger::Write(pLogger->m_LevelQ.front(), pLogger->m_MessageQ.front());
        pLogger->m_LevelQ.pop();
        pLogger->m_MessageQ.pop();
        break;
      case ThreadLogger::FLUSH:
        pLogger->Logger::Flush();
        break;
      }
      pLogger->m_OperationQ.pop();
    }
    pLogger->m_Mutex.Unlock();
    pLogger->m_WaitMutex.Unlock();
  }
  return ITK_THREAD_RETURN_VALUE;
}


/** Print contents of a ThreadLogger */
void ThreadLogger::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Thread ID: " << this->m_ThreadID << std::endl;
  os << indent << "Operation Queue Size: " << this->m_OperationQ.size() << std::endl;
  os << indent << "Message Queue Size: " << this->m_MessageQ.size() << std::endl;
  os << indent << "Level Queue Size: " << this->m_LevelQ.size() << std::endl;
  os << indent << "Output Queue Size: " << this->m_OutputQ.size() << std::endl;
}


} // namespace itk

