/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include <iostream>
#include "itkThreadLogger.h"
#include "itksys/SystemTools.hxx"

namespace itk
{

ThreadLogger ::ThreadLogger()
{
  m_Delay = 300; // ms
  m_TerminationRequested = false;
  m_Thread = std::thread(&ThreadLogger::ThreadFunction, this);
}

ThreadLogger ::~ThreadLogger()
{
  if (m_Thread.joinable())
  {
    m_TerminationRequested = true;
    m_Thread.join(); // waits for it to finish if necessary
  }
}

void
ThreadLogger ::SetPriorityLevel(PriorityLevelEnum level)
{
  this->m_Mutex.lock();
  this->m_OperationQ.push(SET_PRIORITY_LEVEL);
  this->m_LevelQ.push(level);
  this->m_Mutex.unlock();
}

Logger::PriorityLevelEnum
ThreadLogger ::GetPriorityLevel() const
{
  this->m_Mutex.lock();
  PriorityLevelEnum level = this->m_PriorityLevel;
  this->m_Mutex.unlock();
  return level;
}

void
ThreadLogger ::SetLevelForFlushing(PriorityLevelEnum level)
{
  this->m_Mutex.lock();
  this->m_LevelForFlushing = level;
  this->m_OperationQ.push(SET_LEVEL_FOR_FLUSHING);
  this->m_LevelQ.push(level);
  this->m_Mutex.unlock();
}

Logger::PriorityLevelEnum
ThreadLogger ::GetLevelForFlushing() const
{
  this->m_Mutex.lock();
  PriorityLevelEnum level = this->m_LevelForFlushing;
  this->m_Mutex.unlock();
  return level;
}

void
ThreadLogger ::SetDelay(DelayType delay)
{
  this->m_Mutex.lock();
  this->m_Delay = delay;
  this->m_Mutex.unlock();
}

ThreadLogger::DelayType
ThreadLogger ::GetDelay() const
{
  this->m_Mutex.lock();
  DelayType delay = this->m_Delay;
  this->m_Mutex.unlock();
  return delay;
}

void
ThreadLogger ::AddLogOutput(OutputType * output)
{
  this->m_Mutex.lock();
  this->m_OperationQ.push(ADD_LOG_OUTPUT);
  this->m_OutputQ.push(output);
  this->m_Mutex.unlock();
}

void
ThreadLogger ::Write(PriorityLevelEnum level, std::string const & content)
{
  this->m_Mutex.lock();
  this->m_OperationQ.push(WRITE);
  this->m_MessageQ.push(content);
  this->m_LevelQ.push(level);
  this->m_Mutex.unlock();
  if (this->m_LevelForFlushing >= level)
  {
    this->InternalFlush();
  }
}

void
ThreadLogger ::Flush()
{
  this->m_Mutex.lock();
  this->m_OperationQ.push(FLUSH);
  this->m_Mutex.unlock();
  this->InternalFlush();
}

void
ThreadLogger ::InternalFlush()
{
  this->m_Mutex.lock();

  while (!this->m_OperationQ.empty())
  {
    switch (this->m_OperationQ.front())
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
  this->m_Mutex.unlock();
}

void
ThreadLogger ::ThreadFunction()
{
  while (!m_TerminationRequested)
  {
    m_Mutex.lock();
    while (!m_OperationQ.empty())
    {
      switch (m_OperationQ.front())
      {
        case ThreadLogger::SET_PRIORITY_LEVEL:
          m_PriorityLevel = m_LevelQ.front();
          m_LevelQ.pop();
          break;

        case ThreadLogger::SET_LEVEL_FOR_FLUSHING:
          m_LevelForFlushing = m_LevelQ.front();
          m_LevelQ.pop();
          break;

        case ThreadLogger::ADD_LOG_OUTPUT:
          m_Output->AddLogOutput(m_OutputQ.front());
          m_OutputQ.pop();
          break;

        case ThreadLogger::WRITE:
          Logger::Write(m_LevelQ.front(), m_MessageQ.front());
          m_LevelQ.pop();
          m_MessageQ.pop();
          break;
        case ThreadLogger::FLUSH:
          Logger::Flush();
          break;
      }
      m_OperationQ.pop();
    }
    m_Mutex.unlock();
    itksys::SystemTools::Delay(this->GetDelay());
  }
}

void
ThreadLogger ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Thread ID: " << this->m_Thread.get_id() << std::endl;
  os << indent << "Low-priority Message Delay: " << this->m_Delay << std::endl;
  os << indent << "Operation Queue Size: " << this->m_OperationQ.size() << std::endl;
  os << indent << "Message Queue Size: " << this->m_MessageQ.size() << std::endl;
  os << indent << "Level Queue Size: " << this->m_LevelQ.size() << std::endl;
  os << indent << "Output Queue Size: " << this->m_OutputQ.size() << std::endl;
}

} // namespace itk
