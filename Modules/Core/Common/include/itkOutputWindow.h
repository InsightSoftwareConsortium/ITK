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
#ifndef itkOutputWindow_h
#define itkOutputWindow_h

#include "itkObject.h"

namespace itk
{
/** \class OutputWindow
 * \brief Messages sent from the system are collected by this object.
 *
 * Text messages that the system should display to the user are sent to
 * this object (or subclasses of this object).
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT OutputWindow:public Object
{
public:
  /** Standard class typedefs. */
  typedef OutputWindow               Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(OutputWindow, Object);

  /** This is a singleton pattern New.  There will only be ONE
   * reference to a OutputWindow object per process.  Clients that
   * call this must call Delete on the object so that the reference
   * counting will work.   The single instance will be unreferenced when
   * the program exits. */
  static Pointer New();

  /** Return the singleton instance with no reference counting. */
  static Pointer GetInstance();

  /** Supply a user defined output window. Call ->Delete() on the supplied
   * instance after setting it. */
  static void SetInstance(OutputWindow *instance);

  /** Send a string to display. */
  virtual void DisplayText(const char *);

  /** Send a string as an error message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayErrorText(const char *t) { this->DisplayText(t); }

  /** Send a string as a warningmessage to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayWarningText(const char *t) { this->DisplayText(t); }

  /** Send a string as a message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayGenericOutputText(const char *t) { this->DisplayText(t); }

  /** Send a string as a debug message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayDebugText(const char *t) { this->DisplayText(t); }

  /** If PromptUser is set to true then each time a line of text
   * is displayed, the user is asked if they want to keep getting
   * messages. */
  itkSetMacro(PromptUser, bool);
  itkGetConstMacro(PromptUser, bool);
  itkBooleanMacro(PromptUser);

protected:
  OutputWindow();
  virtual ~OutputWindow() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OutputWindow);

  bool           m_PromptUser;
  static Pointer m_Instance;
};
} // end namespace itk

#endif
