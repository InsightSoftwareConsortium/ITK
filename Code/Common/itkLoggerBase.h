/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLoggerBase_h
#define __itkLoggerBase_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkMultipleLogOutput.h"
#include "itkRealTimeClock.h"

namespace itk
{
/** \class LoggerBase
*   \brief Class LoggerBase is meant for logging information during a run.
*
* \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
*                     ISIS Center, Georgetown University.
*
*
 *  \ingroup OSSystemObjects LoggingObjects
*/

class ITKCommon_EXPORT LoggerBase : public Object
{

public:

  typedef LoggerBase  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( LoggerBase, Object );

  typedef  MultipleLogOutput::OutputType   OutputType;

  /** Definition of types of messages. These codes will be used to regulate the
   * level of detail of messages reported to the final outputs */
  typedef enum
  {
    MUSTFLUSH=0,
    FATAL,
    CRITICAL,
    WARNING,
    INFO,
    DEBUG,
    NOTSET
  } PriorityLevelType;

  itkSetStringMacro(Name);
  itkGetStringMacro(Name);

  /** Provides a default formatted log entry */
  virtual std::string BuildFormattedEntry(PriorityLevelType level, std::string const & content);

  /** Set the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual void SetPriorityLevel( PriorityLevelType level )
  {
    m_PriorityLevel = level;
  }

  /** Get the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual PriorityLevelType GetPriorityLevel() const
  {
    return m_PriorityLevel;
  }

  virtual void SetLevelForFlushing( PriorityLevelType level )
  {
    m_LevelForFlushing = level;
  }

  virtual PriorityLevelType GetLevelForFlushing() const
  {
    return m_LevelForFlushing;
  }

  /** Registers another output stream with the multiple output. */
  virtual void AddLogOutput( OutputType* output ) ;


  virtual void Write(PriorityLevelType level, std::string const & content) ;
  /** Helper methods */
  void Debug ( std::string const& message ) { this->Write ( LoggerBase::DEBUG, message ); }
  void Info ( std::string const& message ) { this->Write ( LoggerBase::INFO, message ); }
  void Warning ( std::string const& message ) { this->Write ( LoggerBase::WARNING, message ); }
  void Critical ( std::string const& message ) { this->Write ( LoggerBase::CRITICAL, message ); }
  void Error ( std::string const& message ) { this->Write ( LoggerBase::CRITICAL, message ); }
  void Fatal ( std::string const& message ) { this->Write ( LoggerBase::FATAL, message ); }

  virtual void Flush();

protected:

  /** Constructor */
  LoggerBase();

  /** Destructor */
  virtual ~LoggerBase();

  /** Print contents of a LoggerBase */
  virtual void PrintSelf(std::ostream &os, Indent indent) const;

protected:

  PriorityLevelType m_PriorityLevel;

  PriorityLevelType m_LevelForFlushing;

  MultipleLogOutput::Pointer  m_Output;

  RealTimeClock::Pointer  m_Clock;
private:

  std::string m_Name;

};  // class LoggerBase


} // namespace itk


#endif  // __itkLoggerBase_h
