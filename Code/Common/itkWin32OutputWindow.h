/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWin32OutputWindow.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * This class is used for error and debug message output on the windows
 * platform. It creates a read only EDIT control to display the
 * output. This class should not be used directly. It should
 * only be used through the interface of itkOutputWindow.  This class
 * only handles one output window per process.  If the window is destroyed,
 * the itkObject::GlobalWarningDisplayOff() function is called.  The
 * window is created the next time text is written to the window.
 */

#ifndef __itkWin32OutputWindow_h
#define __itkWin32OutputWindow_h

#include "itkOutputWindow.h"
#include <windows.h>

class ITK_EXPORT itkWin32OutputWindow : public itkOutputWindow
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer<itkWin32OutputWindow> Pointer;

  /** 
   * Create the object.
   */
  static itkWin32OutputWindow* New();

  /** 
   * Put the text into the display window.
   * Each new line is converted to a carriage return, new line.
   */ 
  virtual void DisplayText(const char*);

  static LRESULT APIENTRY WndProc(HWND hWnd, UINT message, 
                                  WPARAM wParam, LPARAM lParam);
protected: 
  itkWin32OutputWindow() {}; 
  virtual ~itkWin32OutputWindow() {}; 
  itkWin32OutputWindow(const itkWin32OutputWindow&) {};
  void operator=(const itkWin32OutputWindow&) {};
  
  void PromptText(const char* text);
  static void AddText(const char*);
  static int Initialize();
  static HWND m_OutputWindow;
};

#endif
