#ifndef _wrapWin32OutputWindow_h
#define _wrapWin32OutputWindow_h

#include "wrapUtils.h"

#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace _wrap_
{

class _wrap_EXPORT Win32OutputWindow
{
public:
  typedef Win32OutputWindow Self;
  
  static Win32OutputWindow* GetInstance();
  
  // New lines are converted to carriage return new lines.
  void DisplayText(const char*);
  //BTX
  static LRESULT APIENTRY WndProc(HWND hWnd, UINT message, 
				  WPARAM wParam, LPARAM lParam);
  //ETX
private: 
  Win32OutputWindow() {}
  ~Win32OutputWindow() {}
  Win32OutputWindow(const Self&);
  void operator=(const Self&);

  static void AddText(const char*);
  static bool Initialize();
  static HWND outputWindow;
};

} // namespace _wrap_

#endif
