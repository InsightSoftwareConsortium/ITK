/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWin32OutputWindow.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 */

class ITK_EXPORT Win32OutputWindow : public OutputWindow
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Win32OutputWindow   Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef OutputWindow  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Win32OutputWindow, OutputWindow);

  /** 
   * Put the text into the display window.
   * Each new line is converted to a carriage return, new line.
   */ 
  virtual void DisplayText(const char*);

  static LRESULT APIENTRY WndProc(HWND hWnd, UINT message, 
                                  WPARAM wParam, LPARAM lParam);
protected: 
  Win32OutputWindow() {}
  virtual ~Win32OutputWindow() {}
  Win32OutputWindow(const Self&) {}
  void operator=(const Self&) {}
  
  void PromptText(const char* text);
  static void AddText(const char*);
  static int Initialize();
  static HWND m_OutputWindow;
};
 
  
} // end namespace itk
#endif  //  __itkWin32OutputWindow_h
