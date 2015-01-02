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
#include "itkWin32OutputWindow.h"

namespace itk
{
/** */
HWND Win32OutputWindow:: m_OutputWindow = 0;

Win32OutputWindow
::~Win32OutputWindow()
{
  if ( Win32OutputWindow::m_OutputWindow )
    {
    DestroyWindow(Win32OutputWindow::m_OutputWindow);
    Win32OutputWindow::m_OutputWindow = ITK_NULLPTR;
    }
}

/** */
LRESULT APIENTRY
Win32OutputWindow
::WndProc(HWND hWnd, UINT message,
          WPARAM wParam,
          LPARAM lParam)
{
  switch ( message )
    {
    case WM_SIZE:
      {
      /** width of client area  */
      int w = LOWORD(lParam);

      /** height of client area */
      int h = HIWORD(lParam);

      MoveWindow(Win32OutputWindow::m_OutputWindow,
                 0, 0, w, h, true);
      }
      break;
    case WM_DESTROY:
      Win32OutputWindow::m_OutputWindow = ITK_NULLPTR;
      Object::GlobalWarningDisplayOff();
      break;
    case WM_CLOSE:
      if ( Win32OutputWindow::m_OutputWindow )
        {
        DestroyWindow(Win32OutputWindow::m_OutputWindow);
        Win32OutputWindow::m_OutputWindow = ITK_NULLPTR;
        }
      break;
    case WM_CREATE:
      break;
    }
  return DefWindowProc(hWnd, message, wParam, lParam);
}

/** Display text in the window, and translate the \n to \r\n. */
void
Win32OutputWindow
::DisplayText(const char *text)
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

  /** Create a buffer big enough to hold the entire text */
  char *buffer = new char[strlen(text) + 1];

  /** Start at the beginning */
  const char *NewLinePos = text;
  while ( NewLinePos )
    {
    /** Find the next new line in text */
    NewLinePos = strchr(text, '\n');
    /** if no new line is found then just add the text */
    if ( NewLinePos == 0 )
      {
      Win32OutputWindow::AddText(text);
      }
    /** if a new line is found copy it to the buffer
     *  and add the buffer with a control new line */
    else
      {
      int len = NewLinePos - text;
      strncpy(buffer, text, len);
      buffer[len] = 0;
      text = NewLinePos + 1;
      Win32OutputWindow::AddText(buffer);
      Win32OutputWindow::AddText("\r\n");
      }
    }
  delete[] buffer;
}

/** Add some text to the EDIT control. */
void
Win32OutputWindow
::AddText(const char *text)
{
  if ( !Initialize()  || ( strlen(text) == 0 ) )
    {
    return;
    }

  /** move to the end of the text area */
  SendMessage(Win32OutputWindow::m_OutputWindow, EM_SETSEL,
              (WPARAM)-1, (LPARAM)-1);

  /** Append the text to the control */
  SendMessage(Win32OutputWindow::m_OutputWindow, EM_REPLACESEL,
              0, (LPARAM)text);
}

/** initialize the output window with an EDIT control and
 *  a container window. */
int
Win32OutputWindow
::Initialize()
{
  /** check to see if it is already initialized */
  if ( Win32OutputWindow::m_OutputWindow )
    {
    return 1;
    }
  /** Initialized the output window */

  WNDCLASS wndClass;
  /** has the class been registered ? */
  if ( !GetClassInfo(GetModuleHandle(ITK_NULLPTR), "OutputWindow", &wndClass) )
    {
    wndClass.style = CS_HREDRAW | CS_VREDRAW;
    wndClass.lpfnWndProc = Win32OutputWindow::WndProc;
    wndClass.cbClsExtra = 0;
    wndClass.hInstance = GetModuleHandle(ITK_NULLPTR);
    wndClass.hIcon = LoadIcon(ITK_NULLPTR, IDI_APPLICATION);
    wndClass.hCursor = LoadCursor(ITK_NULLPTR, IDC_ARROW);
    wndClass.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wndClass.lpszMenuName = ITK_NULLPTR;
    wndClass.lpszClassName = "OutputWindow";
    /** doesn't use these extra 4 bytes, but app writers may want them,
     *  so we provide them. */
    wndClass.cbWndExtra = 4;
    RegisterClass(&wndClass);
    }

  /** create parent container window */
  HWND win = CreateWindow(
    "OutputWindow", "OutputWindow",
    WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
    0, 0, 512, 512,
    ITK_NULLPTR, ITK_NULLPTR, GetModuleHandle(ITK_NULLPTR), ITK_NULLPTR);

  /** Now create child window with text display box */
  CREATESTRUCT lpParam;
  lpParam.hInstance = GetModuleHandle(ITK_NULLPTR);
  lpParam.hMenu = ITK_NULLPTR;
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

  /**Create the EDIT window as a child of win */
  Win32OutputWindow::m_OutputWindow = CreateWindow(
    lpParam.lpszClass,  // pointer to registered class name
    "",                 // pointer to window name
    lpParam.style,      // window style
    lpParam.x,          // horizontal position of window
    lpParam.y,          // vertical position of window
    lpParam.cx,         // window width
    lpParam.cy,         // window height
    lpParam.hwndParent, // handle to parent or owner window
    ITK_NULLPTR,               // handle to menu or child-window identifier
    lpParam.hInstance,  // handle to application instance
    &lpParam            // pointer to window-creation data
    );
  const int maxsize = 5242880;

  SendMessage(Win32OutputWindow::m_OutputWindow,
              EM_LIMITTEXT, maxsize, 0L);

  /** show the top level container window */
  ShowWindow(win, SW_SHOW);
  return 1;
}

/** Prompt some text */
void
Win32OutputWindow
::PromptText(const char *text)
{
  std::ostringstream msg;

  msg << text << "\nPress Cancel to suppress any further messages.";
  if ( MessageBox(ITK_NULLPTR, msg.str().c_str(), "Error",
                  MB_ICONERROR | MB_OKCANCEL) == IDCANCEL )
    {
    Object::GlobalWarningDisplayOff();
    }
}
} // end namespace itk
