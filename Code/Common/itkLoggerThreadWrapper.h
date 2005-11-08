/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerThreadWrapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLoggerThreadWrapper_h
#define __itkLoggerThreadWrapper_h

#if defined(_MSC_VER)
   //Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
   #pragma warning( disable : 4786 )
  // warning C4503: 'insert' : decorated name length exceeded, name was truncated
  #pragma warning ( disable : 4503 )
#endif
#if ! defined(_MSC_VER) //NOTE: This class does not work under MSVS6


#include <string>
#include <queue>

#include "itkMacro.h"
#include "itkMultiThreader.h"
#include "itkSimpleFastMutexLock.h"

namespace itk
{

/** \class LoggerThreadWrapper
 *  \brief Class LoggerThreadWrapper is meant for providing logging service as a separate thread.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *  \ingroup OSSystemObjects LoggingObjects
 */

// MSVS6 can't do this type of nested template
template < class SimpleLoggerType >
class LoggerThreadWrapper : public SimpleLoggerType
{

public:

  typedef LoggerThreadWrapper  Self;
  typedef SimpleLoggerType  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( LoggerThreadWrapper, SimpleLoggerType );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  typedef  typename SimpleLoggerType::OutputType   OutputType;
  typedef  typename SimpleLoggerType::PriorityLevelType  PriorityLevelType;

  /** Definition of types of operations for LoggerThreadWrapper. */
  typedef enum
  {
    SET_PRIORITY_LEVEL,
    SET_LEVEL_FOR_FLUSHING,
    ADD_LOG_OUTPUT,
    WRITE,
    FLUSH
  } OperationType;


  /** Set the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual void SetPriorityLevel( PriorityLevelType level );

  /** Get the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual PriorityLevelType GetPriorityLevel() const;

  virtual void SetLevelForFlushing( PriorityLevelType level );
  virtual PriorityLevelType GetLevelForFlushing() const;

  /** Registers another output stream with the multiple output. */
  virtual void AddLogOutput( OutputType* output );

  virtual void Write(PriorityLevelType level, std::string const & content);

  virtual void Flush();

protected:

  /** Constructor */
  LoggerThreadWrapper();

  /** Destructor */
  virtual ~LoggerThreadWrapper();

  /** Print contents of a LoggerThreadWrapper */
  virtual void PrintSelf(std::ostream &os, Indent indent) const;

  static ITK_THREAD_RETURN_TYPE ThreadFunction(void*);

private:

  typedef std::queue<OperationType> OperationContainerType;

  typedef std::queue<std::string>  MessageContainerType;

  typedef std::queue<PriorityLevelType>  LevelContainerType;

  typedef std::queue<typename OutputType::Pointer>  OutputContainerType;

  MultiThreader::Pointer  m_Threader;

  int m_ThreadID;

  OperationContainerType  m_OperationQ;

  MessageContainerType  m_MessageQ;

  LevelContainerType  m_LevelQ;

  OutputContainerType m_OutputQ;

  SimpleFastMutexLock m_Mutex;

  SimpleFastMutexLock m_WaitMutex;

};  // class LoggerThreadWrapper
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLoggerThreadWrapper.txx"
#endif



#endif  // !defined (_MSC_VER)
#endif  // __itkLoggerThreadWrapper_h
