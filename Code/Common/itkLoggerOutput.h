/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerOutput.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLoggerOutput_h
#define __itkLoggerOutput_h

#include "itkOutputWindow.h"
#include "itkLogger.h"


namespace itk
{
/** \class LoggerOutput
 * \brief This class is meant for overriding itk::OutputWindow to redirect
 * messages to itk::Logger.
 *
 * Text messages that the system should display to the user are sent to 
 * this object (or subclasses of this object).
 *
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 * \ingroup OSSystemObjects
 */
class ITKCommon_EXPORT LoggerOutput : public OutputWindow 
{
public:
  /** Standard class typedefs. */
  typedef LoggerOutput        Self;
  typedef OutputWindow  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(LoggerOutput, OutputWindow);

  itkNewMacro(LoggerOutput);

  typedef Logger*   LoggerType;

  /** Send a string to display. */
  virtual void DisplayText(const char* t);

  /** Send a string as an error message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayErrorText(const char *t);

  /** Send a string as a warningmessage to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayWarningText(const char *t);

  /** Send a string as a message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayGenericOutputText(const char *t);

  /** Send a string as a debug message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayDebugText(const char *t);

  itkSetMacro(Logger, LoggerType);

  itkGetMacro(Logger, LoggerType);

  virtual void OverrideITKWindow() 
  {
    itk::OutputWindow::SetInstance(this);
  }
 
protected:
  LoggerOutput() {}
  virtual ~LoggerOutput() {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  Logger* m_Logger;
};
  
} // end namespace itk


#endif  // __itkLoggerOutput_h
