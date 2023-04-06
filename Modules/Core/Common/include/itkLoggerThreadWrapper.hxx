/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  this->m_OperationQ.push(OperationEnum::SET_PRIORITY_LEVEL);
  this->m_LevelQ.push(level);
}

/** Get the priority level for the current logger. Only messages that have
 * priorities equal or greater than the one set here will be posted to the
 * current outputs */
template <typename SimpleLoggerType>
typename SimpleLoggerType::PriorityLevelEnum
LoggerThreadWrapper<SimpleLoggerType>::GetPriorityLevel() const
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  PriorityLevelEnum                 level = this->m_PriorityLevel;
  return level;
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::SetLevelForFlushing(PriorityLevelEnum level)
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  this->m_LevelForFlushing = level;
  this->m_OperationQ.push(OperationEnum::SET_LEVEL_FOR_FLUSHING);
  this->m_LevelQ.push(level);
}

template <typename SimpleLoggerType>
typename SimpleLoggerType::PriorityLevelEnum
LoggerThreadWrapper<SimpleLoggerType>::GetLevelForFlushing() const
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  PriorityLevelEnum                 level = this->m_LevelForFlushing;
  return level;
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::SetDelay(DelayType delay)
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  this->m_Delay = delay;
}

template <typename SimpleLoggerType>
auto
LoggerThreadWrapper<SimpleLoggerType>::GetDelay() const -> DelayType
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  DelayType                         delay = this->m_Delay;
  return delay;
}

/** Adds an output stream to the MultipleLogOutput for writing. */
template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::AddLogOutput(OutputType * output)
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  this->m_OperationQ.push(OperationEnum::ADD_LOG_OUTPUT);
  this->m_OutputQ.push(output);
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::Write(PriorityLevelEnum level, std::string const & content)
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  if (this->m_PriorityLevel >= level)
  {
    this->m_OperationQ.push(OperationEnum::WRITE);
    this->m_MessageQ.push(content);
    this->m_LevelQ.push(level);
  }
  if (this->m_LevelForFlushing >= level)
  {
    this->PrivateFlush();
  }
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::PrivateFlush()
{
  // m_Mutex must already be held here!

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
  this->SimpleLoggerType::PrivateFlush();
  this->m_Output->Flush();
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::Flush()
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  this->PrivateFlush();
}

template <typename SimpleLoggerType>
LoggerThreadWrapper<SimpleLoggerType>::LoggerThreadWrapper()
{
  m_Delay = 300; // ms
  m_TerminationRequested = false;
  m_Thread = std::thread(&Self::ThreadFunction, this);
}

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
    {
      const std::lock_guard<std::mutex> lockGuard(m_Mutex);
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
      SimpleLoggerType::PrivateFlush();

    } // end of scope of lockGuard (unlocking m_Mutex automatically)

    itksys::SystemTools::Delay(this->GetDelay());
  }
}

template <typename SimpleLoggerType>
void
LoggerThreadWrapper<SimpleLoggerType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Thread ID: " << m_Thread.get_id() << std::endl;
  os << indent << "TerminationRequested: " << m_TerminationRequested << std::endl;

  os << indent << "OperationQ size: " << m_OperationQ.size() << std::endl;
  os << indent << "MessageQ size: " << m_MessageQ.size() << std::endl;
  os << indent << "LevelQ size: " << m_LevelQ.size() << std::endl;
  os << indent << "OutputQ size: " << m_OutputQ.size() << std::endl;

  os << indent << "Delay: " << m_Delay << std::endl;
}

} // namespace itk

#endif // itkLoggerThreadWrapper_hxx
