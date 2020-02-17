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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkLoggerOutput_h
#define itkLoggerOutput_h

#include "itkOutputWindow.h"
#include "itkLogger.h"

namespace itk
{
/** \class LoggerOutput
 * \brief Used for overriding itk::OutputWindow to redirect
 *        messages to itk::Logger.
 *
 * Text messages that the system should display to the user are sent to
 * this object (or subclasses of this object).
 *
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT LoggerOutput : public OutputWindow
{
public:
  /** Standard class type aliases. */
  using Self = LoggerOutput;
  using Superclass = OutputWindow;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoggerOutput, OutputWindow);

  itkNewMacro(LoggerOutput);

  using LoggerType = Logger *;

  /** Send a string to display. */
  void
  DisplayText(const char * t) override;

  /** Send a string as an error message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  void
  DisplayErrorText(const char * t) override;

  /** Send a string as a warningmessage to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  void
  DisplayWarningText(const char * t) override;

  /** Send a string as a message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  void
  DisplayGenericOutputText(const char * t) override;

  /** Send a string as a debug message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  void
  DisplayDebugText(const char * t) override;

  itkSetMacro(Logger, LoggerType);

  itkGetConstMacro(Logger, LoggerType);

  virtual void
  OverrideITKWindow()
  {
    itk::OutputWindow::SetInstance(this);
  }

protected:
  LoggerOutput() = default;
  ~LoggerOutput() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  Logger * m_Logger{ nullptr };
};
} // end namespace itk

#endif // itkLoggerOutput_h
