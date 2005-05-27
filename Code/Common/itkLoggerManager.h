/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerManager.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itk_LoggerManager_h
#define __itk_LoggerManager_h

#if defined(_MSC_VER)
   //Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
   #pragma warning( disable : 4786 )
  // warning C4503: 'insert' : decorated name length exceeded, name was truncated
  #pragma warning ( disable : 4503 )
#endif

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkLogger.h"
#include "itkThreadLogger.h"

#include <map>
#include <set>

namespace itk
{
/** \class LoggerManager
 *  \brief Class LoggerManager is meant for centrally managing loggers. 
 *
 *
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *
 *  \ingroup OSSystemObjects LoggingObjects                    
 *
 */

class ITKCommon_EXPORT LoggerManager : public Object
{

public:
  
  typedef LoggerManager  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( LoggerManager, Object );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  typedef Logger::PriorityLevelType   PriorityLevelType;

  typedef Logger::OutputType          OutputType;

  typedef Logger::Pointer             LoggerPointer;
  typedef ThreadLogger::Pointer       ThreadLoggerPointer;

  typedef const std::string   NameType;

  /** create a logger and add it into LoggerManager */
  LoggerPointer CreateLogger( 
                          const NameType &name, 
                          PriorityLevelType level, 
                          PriorityLevelType levelForFlushing = Logger::MUSTFLUSH );

  /** create a thread logger and add it into LoggerManager */
  ThreadLoggerPointer CreateThreadLogger( 
                          const NameType &name, 
                          PriorityLevelType level, 
                          PriorityLevelType levelForFlushing = Logger::MUSTFLUSH );

  /** Registers a logger */
  void AddLogger( const NameType &name, Logger* logger );

  Logger* GetLogger( const NameType &name);

  void SetPriorityLevel( PriorityLevelType level );

  void SetLevelForFlushing( PriorityLevelType level );

  void AddLogOutput( OutputType* output );

  void Write( PriorityLevelType level, std::string const & content);

  void Flush();

protected:

  /** Constructor */
  LoggerManager() {}

  /** Destructor */
  virtual ~LoggerManager() {}

  /** Print contents of a LoggerManager */
  void PrintSelf(std::ostream &s, Indent indent) const;

private:

  typedef std::map< NameType, LoggerPointer >  ContainerType;

  ContainerType   m_LoggerSet;

};  // class Logger


} // namespace itk


#endif  // __itk_LoggerManager_h
