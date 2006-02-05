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

#include "itkMultipleLogOutput.h"
#include "itkRealTimeClock.h"
#include "itkLoggerBase.h"

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

class ITKCommon_EXPORT Logger : public LoggerBase
{
public:
  typedef Logger  Self;
  typedef LoggerBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( Logger, Object );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

protected:

  /** Constructor */
  Logger() {};

  /** Destructor */
  virtual ~Logger() {};

};  // class Logger

} // namespace itk

#endif  // __itkLogger_h
