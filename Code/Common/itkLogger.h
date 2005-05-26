/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLogger.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLogger_h
#define __itkLogger_h

#if defined(_MSC_VER)
   //Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
   #pragma warning( disable : 4786 )
  // warning C4503: 'insert' : decorated name length exceeded, name was truncated
  #pragma warning ( disable : 4503 )
#endif

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkMultipleLogOutput.h"
#include "itkRealTimeClock.h"

namespace itk
{
/** \class Logger
*   \brief Class Logger is meant for logging information during a run. 
*
* \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
*                     ISIS Center, Georgetown University.
*
*
 *  \ingroup OSSystemObjects LoggingObjects                    
*/

class Logger : public Object
{

public:
  
  typedef Logger  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( Logger, Object );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

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
  virtual void AddLogOutput( OutputType* output );

  virtual void Write(PriorityLevelType level, std::string const & content);

  virtual void Flush();

protected:

  /** Constructor */
  Logger();

  /** Destructor */
  virtual ~Logger();

  /** Print contents of a Logger */
  virtual void PrintSelf(std::ostream &os, Indent indent) const;

protected:

  PriorityLevelType m_PriorityLevel;

  PriorityLevelType m_LevelForFlushing;

  MultipleLogOutput::Pointer  m_Output;

private:

  RealTimeClock::Pointer  m_Clock;

  std::string m_Name;

};  // class Logger


} // namespace itk


#endif  // __itkLogger_h
