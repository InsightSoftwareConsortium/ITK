/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWin32OutputWindow.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWin32OutputWindow_h
#define __itkWin32OutputWindow_h

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
 */

class ITKCommon_EXPORT Win32OutputWindow : public OutputWindow
{
public:
  /** Standard class typedefs. */
  typedef Win32OutputWindow   Self;
  typedef OutputWindow  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Win32OutputWindow, OutputWindow);

  /** Put the text into the display window.
   * Each new line is converted to a carriage return, new line. */ 
  virtual void DisplayText(const char*);

  static LRESULT APIENTRY WndProc(HWND hWnd, UINT message, 
                                  WPARAM wParam, LPARAM lParam);
protected: 
  Win32OutputWindow() {}
  virtual ~Win32OutputWindow() {}
  
  void PromptText(const char* text);
  static void AddText(const char*);
  static int Initialize();

private:
  Win32OutputWindow(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  static HWND m_OutputWindow;
};
  
} // end namespace itk
#endif  //  __itkWin32OutputWindow_h
