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
template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::SetPriorityLevel(PriorityLevelEnum level)
{
  this->m_Mutex.lock();
  this->m_OperationQ.push(OperationEnum::SET_PRIORITY_LEVEL);
  this->m_LevelQ.push(level);
  this->m_Mutex.unlock();
}

/** Get the priority level for the current logger. Only messages that have
 * priorities equal or greater than the one set here will be posted to the
 * current outputs */
template <typename SimpleLoggerType>
typename SimpleLoggerType::PriorityLevelEnum
LoggerThreadWrapper<SimpleLoggerType>::GetPriorityLevel() const
{
  this->m_Mutex.lock();
  PriorityLevelEnum level = this->m_PriorityLevel;
  this->m_Mutex.unlock();
  return level;
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::SetLevelForFlushing(PriorityLevelEnum level)
{
  this->m_Mutex.lock();
  this->m_LevelForFlushing = level;
  this->m_OperationQ.push(OperationEnum::SET_LEVEL_FOR_FLUSHING);
  this->m_LevelQ.push(level);
  this->m_Mutex.unlock();
}

template <typename SimpleLoggerType>
typename SimpleLoggerType::PriorityLevelEnum
LoggerThreadWrapper<SimpleLoggerType>::GetLevelForFlushing() const
{
  this->m_Mutex.lock();
  PriorityLevelEnum level = this->m_LevelForFlushing;
  this->m_Mutex.unlock();
  return level;
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::SetDelay(DelayType delay)
{
  this->m_Mutex.lock();
  this->m_Delay = delay;
  this->m_Mutex.unlock();
}

template <typename SimpleLoggerType>
typename LoggerThreadWrapper<SimpleLoggerType>::DelayType
LoggerThreadWrapper<SimpleLoggerType>::GetDelay() const
{
  this->m_Mutex.lock();
  DelayType delay = this->m_Delay;
  this->m_Mutex.unlock();
  return delay;
}

/** Adds an output stream to the MultipleLogOutput for writing. */
template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::AddLogOutput(OutputType * output)
{
  this->m_Mutex.lock();
  this->m_OperationQ.push(OperationEnum::ADD_LOG_OUTPUT);
  this->m_OutputQ.push(output);
  this->m_Mutex.unlock();
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::Write(PriorityLevelEnum level, std::string const & content)
{
  this->m_Mutex.lock();
  if (this->m_PriorityLevel >= level)
  {
    this->m_OperationQ.push(OperationEnum::WRITE);
    this->m_MessageQ.push(content);
    this->m_LevelQ.push(level);
  }
  this->m_Mutex.unlock();
  if (this->m_LevelForFlushing >= level)
  {
    this->Flush();
  }
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::Flush()
{
  this->m_Mutex.lock();

  while (!this->m_OperationQ.empty())
  {
    switch (this->m_OperationQ.front())
    {
      case OperationEnum::SET_PRIORITY_LEVEL:
        this->m_PriorityLevel = this->m_LevelQ.front();
        this->m_LevelQ.pop();
        break;
      case OperationEnum::SET_LEVEL_FOR_FLUSHING:
        this->m_LevelForFlushing = this->m_LevelQ.front();
        this->m_LevelQ.pop();
        break;
      case OperationEnum::ADD_LOG_OUTPUT:
        this->m_Output->AddLogOutput(this->m_OutputQ.front());
        this->m_OutputQ.pop();
        break;
      case OperationEnum::WRITE:
        this->SimpleLoggerType::Write(this->m_LevelQ.front(), this->m_MessageQ.front());
        this->m_LevelQ.pop();
        this->m_MessageQ.pop();
        break;
    }
    this->m_OperationQ.pop();
  }
  this->SimpleLoggerType::Flush();
  this->m_Output->Flush();
  this->m_Mutex.unlock();
}

/** Constructor */
template <typename SimpleLoggerType>
LoggerThreadWrapper<SimpleLoggerType>::LoggerThreadWrapper()
{
  m_Delay = 300; // ms
  m_TerminationRequested = false;
  m_Thread = std::thread(&Self::ThreadFunction, this);
}

/** Destructor */
template <typename SimpleLoggerType>
LoggerThreadWrapper<SimpleLoggerType>::~LoggerThreadWrapper()
{
  this->Flush();
  if (m_Thread.joinable())
  {
    m_TerminationRequested = true;
    m_Thread.join(); // waits for it to finish if necessary
  }
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::ThreadFunction()
{
  while (!m_TerminationRequested)
  {
    m_Mutex.lock();
    while (!m_OperationQ.empty())
    {
      switch (m_OperationQ.front())
      {
        case OperationEnum::SET_PRIORITY_LEVEL:
          this->m_PriorityLevel = m_LevelQ.front();
          m_LevelQ.pop();
          break;

        case OperationEnum::SET_LEVEL_FOR_FLUSHING:
          this->m_LevelForFlushing = m_LevelQ.front();
          m_LevelQ.pop();
          break;

        case OperationEnum::ADD_LOG_OUTPUT:
          this->m_Output->AddLogOutput(m_OutputQ.front());
          m_OutputQ.pop();
          break;

        case OperationEnum::WRITE:
          SimpleLoggerType::Write(m_LevelQ.front(), m_MessageQ.front());
          m_LevelQ.pop();
          m_MessageQ.pop();
          break;
      }
      m_OperationQ.pop();
    }
    m_Mutex.unlock();
    SimpleLoggerType::Flush();
    itksys::SystemTools::Delay(this->GetDelay());
  }
}

/** Print contents of a LoggerThreadWrapper */
template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::PrintSelf(std::ostream & os, Indent indent) const
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

#endif // itkLoggerThreadWrapper_hxx
