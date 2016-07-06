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
#ifndef itkWin32OutputWindow_h
#define itkWin32OutputWindow_h
#if defined(_MSC_VER) || defined(__MINGW32__) // if on Windows

#include "itkWin32Header.h"
#include "itkOutputWindow.h"
#include "itkObjectFactory.h"
#include "itkWindows.h"
namespace itk
{
/** \class Win32OutputWindow
 * \brief Collect error and debug messages on Win32-based systems.
 *
 * This class is used for error and debug message output on the windows
 * platform. It creates a read only EDIT control to display the
 * output. This class should not be used directly. It should
 * only be used through the interface of OutputWindow.  This class
 * only handles one output window per process.  If the window is destroyed,
 * the Object::GlobalWarningDisplayOff() function is called.  The
 * window is created the next time text is written to the window.
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT Win32OutputWindow:public OutputWindow
{
public:
  /** Standard class typedefs. */
  typedef Win32OutputWindow          Self;
  typedef OutputWindow               Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Win32OutputWindow, OutputWindow);

  /** Put the text into the display window.
   * Each new line is converted to a carriage return, new line. */
  virtual void DisplayText(const char *);

  static LRESULT APIENTRY WndProc(HWND hWnd, UINT message,
                                  WPARAM wParam, LPARAM lParam);

protected:
  Win32OutputWindow() {}
  virtual ~Win32OutputWindow();

  void PromptText(const char *text);

  static void AddText(const char *);

  static int Initialize();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Win32OutputWindow);

  static HWND m_OutputWindow;
};
} // end namespace itk

#endif // _MSC_VER
#endif  //  itkWin32OutputWindow_h
