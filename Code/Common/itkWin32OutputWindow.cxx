/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWin32OutputWindow.cxx
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
#include "itkWin32OutputWindow.h"
#include "itkObjectFactory.h"

namespace itk
{


/**
 *
 */
LRESULT APIENTRY 
Win32OutputWindow
::WndProc(HWND hWnd, UINT message, 
          WPARAM wParam, 
          LPARAM lParam)
{ 
  switch (message) 
    {
    case WM_SIZE:
    {
    /**
     * width of client area 
     */
    int w = LOWORD(lParam);

    /**
     * height of client area
     */
    int h = HIWORD(lParam);
      
    MoveWindow(Win32OutputWindow::m_OutputWindow,
         0, 0, w, h, true);
    }
    break;
    case WM_DESTROY:
      Win32OutputWindow::m_OutputWindow = NULL;
      Object::GlobalWarningDisplayOff();
      break;
    case WM_CREATE:
      break;
    }
  return DefWindowProc(hWnd, message, wParam, lParam);
}
 
/**
 *
 */
HWND Win32OutputWindow::m_OutputWindow = 0;

/**
 * Display text in the window, and translate the \n to \r\n.
 */
void 
Win32OutputWindow
::DisplayText(const char* text)
{
  if ( !text )
    {
    return;
    }

  if ( this->GetPromptUser() )
    {
    this->PromptText(text);
    return;
    }
  
  /**
   * Create a buffer big enough to hold the entire text
   */
  char* buffer = new char[strlen(text)+1];

  /**
   * Start at the begining
   */
  const char* NewLinePos = text;
  while ( NewLinePos )
    {
    int len;
    /**
     * Find the next new line in text
     */
    NewLinePos = strchr(text, '\n');
    /**
     * if no new line is found then just add the text
     */
    if(NewLinePos == 0)
      {
      Win32OutputWindow::AddText(text);
      }
    /**
     * if a new line is found copy it to the buffer
     * and add the buffer with a control new line
     */
    else
      {
      len = NewLinePos - text;
      strncpy(buffer, text, len);
      buffer[len] = 0;
      text = NewLinePos+1;
      Win32OutputWindow::AddText(buffer);
      Win32OutputWindow::AddText("\r\n");
      }
    }
  delete [] buffer;
}


/**
 * Add some text to the EDIT control.
 */

void 
Win32OutputWindow
::AddText(const char* text)
{
  if(!Initialize()  || (strlen(text) == 0))
    {
    return;
    }
  
  /**
   * move to the end of the text area
   */
  SendMessage( Win32OutputWindow::m_OutputWindow, EM_SETSEL, 
               (WPARAM)-1, (LPARAM)-1 );  
  /**
   * Append the text to the control
   */
  SendMessage( Win32OutputWindow::m_OutputWindow, EM_REPLACESEL, 
               0, (LPARAM)text );
}


/**
 * initialize the output window with an EDIT control and
 * a container window.
 */

int 
Win32OutputWindow
::Initialize()
{
  /**
   * check to see if it is already initialized
   */
  if(Win32OutputWindow::m_OutputWindow)
    {
    return 1;
    }
  /**
   * Initialized the output window
   */
  
  WNDCLASS wndClass;   
  /**
   * has the class been registered ?
   */
  if (!GetClassInfo(GetModuleHandle(NULL),"OutputWindow",&wndClass))
    {
    wndClass.style = CS_HREDRAW | CS_VREDRAW;
    wndClass.lpfnWndProc = Win32OutputWindow::WndProc;
    wndClass.cbClsExtra = 0;
    wndClass.hInstance = GetModuleHandle(NULL);
    wndClass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wndClass.hCursor = LoadCursor(NULL, IDC_ARROW);
    wndClass.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wndClass.lpszMenuName = NULL;
    wndClass.lpszClassName = "OutputWindow";
    /**
     *  doesn't use these extra 4 bytes, but app writers
     *  may want them, so we provide them.
     */
    wndClass.cbWndExtra = 4;
    RegisterClass(&wndClass);
    }

  /**
   * create parent container window
   */
  HWND win = CreateWindow(
    "OutputWindow", "OutputWindow",
    WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
    0, 0, 512, 512,
    NULL, NULL, GetModuleHandle(NULL), NULL);
  
  /**
   * Now create child window with text display box
   */
  CREATESTRUCT lpParam;
  lpParam.hInstance = GetModuleHandle(NULL);
  lpParam.hMenu = NULL;
  lpParam.hwndParent = win;
  lpParam.cx = 512;
  lpParam.cy = 512;
  lpParam.x = 0;
  lpParam.y = 0;
  lpParam.style = ES_MULTILINE | ES_READONLY | WS_CHILD 
    | ES_AUTOVSCROLL | ES_AUTOHSCROLL | WS_VISIBLE | WS_MAXIMIZE
    | WS_VSCROLL | WS_HSCROLL;
  
  lpParam.lpszName = "Output Control";
  lpParam.lpszClass = "EDIT";  // use the RICHEDIT control widget
  lpParam.dwExStyle = 0;
  /**
   * Create the EDIT window as a child of win
   */
  Win32OutputWindow::m_OutputWindow = CreateWindow(
    lpParam.lpszClass,  // pointer to registered class name
    "", // pointer to window name
    lpParam.style,        // window style
    lpParam.x,                // horizontal position of window
    lpParam.y,                // vertical position of window
    lpParam.cx,           // window width
    lpParam.cy,          // window height
    lpParam.hwndParent,      // handle to parent or owner window
    NULL,          // handle to menu or child-window identifier
    lpParam.hInstance,     // handle to application instance
    &lpParam        // pointer to window-creation data
    );
  const int maxsize = 5242880;
  
  SendMessage(Win32OutputWindow::m_OutputWindow, 
              EM_LIMITTEXT, maxsize, 0L);

  
  /**
   * show the top level container window
   */
  ShowWindow(win, SW_SHOW);
  return 1;
}


/**
 *
 */
void 
Win32OutputWindow
::PromptText(const char* text)
{
  OStringStream msg;
  msg << text << "\nPress Cancel to supress any further messages.";
  if (MessageBox(NULL, msg.str().c_str(), "Error",
                 MB_ICONERROR | MB_OKCANCEL) == IDCANCEL) 
    { 
    Object::GlobalWarningDisplayOff(); 
    }
}

} // end namespace itk
