/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerManager.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkLoggerManager.h"


namespace itk
{

/** create a logger and add it into LoggerManager */
LoggerManager::LoggerPointer
LoggerManager::CreateLogger( const NameType &name, PriorityLevelType level,
                                            PriorityLevelType levelForFlushing )
{
  Logger::Pointer logger = Logger::New();
  logger->SetName(name.c_str());
  logger->SetPriorityLevel( level );
  logger->SetLevelForFlushing( levelForFlushing );
  this->AddLogger(name, logger);
  return logger;
}


/** create a thread logger and add it into LoggerManager */
LoggerManager::ThreadLoggerPointer 
LoggerManager::CreateThreadLogger( const NameType &name, PriorityLevelType level, 
                                        PriorityLevelType levelForFlushing )
{
  ThreadLogger::Pointer logger = ThreadLogger::New();
  logger->SetName(name.c_str());
  logger->SetPriorityLevel( level );
  logger->SetLevelForFlushing( levelForFlushing );
  this->AddLogger(name, logger);
  return logger;
}


/** Registers another logger */
void LoggerManager::AddLogger( const NameType &name, Logger* logger )
{
//  this->m_LoggerSet.insert(logger);
  this->m_LoggerSet[name] = logger;
}


Logger* 
LoggerManager::GetLogger( const NameType &name ) 
{
  ContainerType::iterator loggerItr = this->m_LoggerSet.find( name );
  if( loggerItr == this->m_LoggerSet.end() )
    {
    return NULL;
    }
  return loggerItr->second.GetPointer();
}


void LoggerManager::SetPriorityLevel( PriorityLevelType level )
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();
  while( itr != this->m_LoggerSet.end() )
  {
    (*itr).second->SetPriorityLevel( level );
    ++itr;
  }
}


void LoggerManager::SetLevelForFlushing( PriorityLevelType level )
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();
  while( itr != this->m_LoggerSet.end() )
  {
    (*itr).second->SetLevelForFlushing( level );
    ++itr;
  }
}


void LoggerManager::AddLogOutput( OutputType* output )
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();
  while( itr != this->m_LoggerSet.end() )
  {
    (*itr).second->AddLogOutput( output );
    ++itr;
  }
}


void LoggerManager::Write( PriorityLevelType level, std::string const & content)
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();
  while( itr != this->m_LoggerSet.end() )
  {
    (*itr).second->Write( level, content );
    ++itr;
  }
}


void LoggerManager::Flush()
{
  ContainerType::iterator itr = this->m_LoggerSet.begin();
  while( itr != this->m_LoggerSet.end() )
  {
    (*itr).second->Flush();
    ++itr;
  }
}

/** Print contents of a LoggerManager */
void LoggerManager::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "The number of loggers: " << m_LoggerSet.size() << std::endl;
}

} // namespace itk


