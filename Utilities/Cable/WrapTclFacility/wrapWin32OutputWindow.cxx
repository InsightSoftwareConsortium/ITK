#include "wrapWin32OutputWindow.h"
#include <string.h>

namespace _wrap_
{

Win32OutputWindow* Win32OutputWindow::GetInstance()
{
  static Win32OutputWindow* win32OutputWindow = 0;
  if(!win32OutputWindow)
    {
    win32OutputWindow = new Win32OutputWindow;
    }
  return win32OutputWindow;
}

LRESULT APIENTRY Win32OutputWindow
::WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  switch (message) 
    {
    case WM_SIZE:
      {
      int w = LOWORD(lParam);  // width of client area 
      int h = HIWORD(lParam); // height of client area  
      
      MoveWindow(Self::outputWindow, 0, 0, w, h, true);
      }
      break;
    case WM_DESTROY:
      Self::outputWindow = 0;
      break;
    case WM_CREATE:
      break;
    }
  return DefWindowProc(hWnd, message, wParam, lParam);
}

// There is one instance of the output window class.
HWND Win32OutputWindow::outputWindow = 0;

// Display text in the window, and translate the \n to \r\n.
void Win32OutputWindow::DisplayText(const char* someText)
{
  if(!someText) { return; }
  
  // Create a buffer big enough to hold the entire text
  char* buffer = new char[strlen(someText)+1];
  // Start at the begining
  const char* newLinePos = someText;
  while(newLinePos)
    {
    int len = 0;
    // Find the next new line in text
    newLinePos = strchr(someText, '\n');
    // if no new line is found then just add the text
    if(newLinePos == 0)
      {
      Self::AddText(someText);
      }
    // if a new line is found copy it to the buffer
    // and add the buffer with a control new line
    else
      {
      len = newLinePos - someText;
      strncpy(buffer, someText, len);
      buffer[len] = '\0';
      someText = newLinePos+1;
      Self::AddText(buffer);
      Self::AddText("\r\n");
      }
    }
  delete [] buffer;
}


/**
 * Add some text to the EDIT control.
 */
void Win32OutputWindow::AddText(const char* someText)
{
  // Make sure the window is open before displaying any text.
  if(!Self::Initialize() || (strlen(someText) == 0)) { return; }  
  // move to the end of the text area
  SendMessage( Self::outputWindow, EM_SETSEL, (WPARAM)-1, (LPARAM)-1);  
  // Append the text to the control
  SendMessage( Self::outputWindow, EM_REPLACESEL, 0, (LPARAM)someText);
}


/**
 * Initialize the output window with an EDIT control and
 * a container window.
 */
bool Win32OutputWindow::Initialize()
{
  // check to see if it is already initialized
  if(Self::outputWindow) { return true; }
  
  // Initialize the output window  
  WNDCLASS wndClass;   
  // has the class been registered ?
  if (!GetClassInfo(GetModuleHandle(0),
                    "_wrap_::Win32OutputWindow",&wndClass))
    {
    wndClass.style = CS_HREDRAW | CS_VREDRAW;
    wndClass.lpfnWndProc = Self::WndProc;
    wndClass.cbClsExtra = 0;
    wndClass.hInstance = GetModuleHandle(NULL);
    wndClass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wndClass.hCursor = LoadCursor(NULL, IDC_ARROW);
    wndClass.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wndClass.lpszMenuName = NULL;
    wndClass.lpszClassName = "_wrap_::Win32OutputWindow";
    wndClass.cbWndExtra = 4;
    RegisterClass(&wndClass);
    }

  // create parent container window
  HWND win = CreateWindow(
    "_wrap_::Win32OutputWindow", "_wrap_::Win32OutputWindow",
    WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
    0, 0, 512, 512,
    NULL, NULL, GetModuleHandle(NULL), NULL);
  
  // Now create child window with text display box
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
  lpParam.lpszClass = "EDIT"; // use the RICHEDIT control widget
  lpParam.dwExStyle = NULL;
  // Create the EDIT window as a child of win
  Self::outputWindow = CreateWindow(
    lpParam.lpszClass,    // pointer to registered class name
    "",                   // pointer to window name
    lpParam.style,        // window style
    lpParam.x,            // horizontal position of window
    lpParam.y,            // vertical position of window
    lpParam.cx,           // window width
    lpParam.cy,           // window height
    lpParam.hwndParent,   // handle to parent or owner window
    NULL,                 // handle to menu or child-window identifier
    lpParam.hInstance,    // handle to application instance
    &lpParam              // pointer to window-creation data
    );
  const int maxsize = 5242880;
  
  SendMessage(Self::outputWindow, EM_LIMITTEXT, maxsize, 0L);
  
  // show the top level container window
  ShowWindow(win, SW_SHOW);
  return true;
}

} // namespace _wrap_
