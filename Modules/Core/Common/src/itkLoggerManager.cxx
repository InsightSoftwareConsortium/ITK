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
#include "itkLoggerManager.h"

namespace itk
{
/** create a logger and add it into LoggerManager */
LoggerManager::LoggerPointer
LoggerManager::CreateLogger(const NameType & name, PriorityLevelType level,
                            PriorityLevelType levelForFlushing)
{
  Logger::Pointer logger = Logger::New();

  logger->SetName( name.c_str() );
  logger->SetPriorityLevel(level);
  logger->SetLevelForFlushing(levelForFlushing);
  this->AddLogger(name, logger);
  return logger;
}

/** create a thread logger and add it into LoggerManager */
LoggerManager::ThreadLoggerPointer
LoggerManager::CreateThreadLogger(const NameType & name, PriorityLevelType level,
                                  PriorityLevelType levelForFlushing)
{
  ThreadLogger::Pointer logger = ThreadLogger::New();

  logger->SetName( name.c_str() );
  logger->SetPriorityLevel(level);
  logger->SetLevelForFlushing(levelForFlushing);
  this->AddLogger(name, logger);
  return logger;
}

/** Registers another logger */
void LoggerManager::AddLogger(const NameType & name, Logger *logger)
{
//  this->m_LoggerSet.insert(logger);
  this->m_LoggerSet[name] = logger;
}

Logger *
LoggerManager::GetLogger(const NameType & name)
{
  ContainerType::iterator loggerItr = this->m_LoggerSet.find(name);

  if ( loggerItr == this->m_LoggerSet.end() )
    {
    return ITK_NULLPTR;
    }
  return loggerItr->second.GetPointer();
}

void LoggerManager::SetPriorityLevel(PriorityLevelType level)
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();

  while ( itr != this->m_LoggerSet.end() )
    {
    ( *itr ).second->SetPriorityLevel(level);
    ++itr;
    }
}

void LoggerManager::SetLevelForFlushing(PriorityLevelType level)
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();

  while ( itr != this->m_LoggerSet.end() )
    {
    ( *itr ).second->SetLevelForFlushing(level);
    ++itr;
    }
}

void LoggerManager::AddLogOutput(OutputType *output)
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();

  while ( itr != this->m_LoggerSet.end() )
    {
    ( *itr ).second->AddLogOutput(output);
    ++itr;
    }
}

void LoggerManager::Write(PriorityLevelType level, std::string const & content)
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();

  while ( itr != this->m_LoggerSet.end() )
    {
    ( *itr ).second->Write(level, content);
    ++itr;
    }
}

void LoggerManager::Flush()
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();

  while ( itr != this->m_LoggerSet.end() )
    {
    ( *itr ).second->Flush();
    ++itr;
    }
}

/** Print contents of a LoggerManager */
void LoggerManager::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "The number of loggers: " << m_LoggerSet.size() << std::endl;
}
} // namespace itk
