/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOutputWindow.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkOutputWindow.h"
#ifdef _WIN32
#include "itkWin32OutputWindow.h"
#endif
#include "itkObjectFactory.h"

namespace itk
{
  
OutputWindow::Pointer OutputWindow::m_Instance = 0;

/**
 * Prompting off by default
 */
OutputWindow
::OutputWindow()
{
  m_PromptUser = 0;
}

OutputWindow
::~OutputWindow()
{
}

void 
OutputWindowDisplayText(const char* message)
{
  OutputWindow::GetInstance()->DisplayText(message);
}

void 
OutputWindow
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutputWindow (single instance): "
     << (void*)OutputWindow::m_Instance << std::endl;

  os << indent << "Prompt User: " << (m_PromptUser ? "On\n" : "Off\n");
}


/**
 * default implementation outputs to cerr only
 */
void 
OutputWindow
::DisplayText(const char* txt)
{
  std::cerr << txt;
  if ( m_PromptUser )
    {
    char c = 'n';
    std::cerr << "\nDo you want to suppress any further messages (y,n)?." 
              << std::endl;
    std::cin >> c;
    if ( c == 'y' || c == 'Y' )
      {
      Object::GlobalWarningDisplayOff(); 
      }
    }
}

/**
 * Return the single instance of the OutputWindow
 */
OutputWindow::Pointer
OutputWindow
::GetInstance()
{
  if ( !OutputWindow::m_Instance )
    {
    // Try the factory first
    OutputWindow::m_Instance  = ObjectFactory<Self>::Create();
    // if the factory did not provide one, then create it here
    if( ! OutputWindow::m_Instance )
      {
      // For the windows OS, use a special output window
#ifdef _WIN32
      OutputWindow::m_Instance = Win32OutputWindow::New();
#else
      OutputWindow::m_Instance = new OutputWindow;
#endif
      }
    }
  /**
   * return the instance
   */
  return OutputWindow::m_Instance;
}

void 
OutputWindow
::SetInstance(OutputWindow* instance)
{
  if ( OutputWindow::m_Instance == instance )
    {
    return;
    }
  OutputWindow::m_Instance = instance;
}

/**
 * This just calls GetInstance
 */
OutputWindow::Pointer 
OutputWindow
::New()
{ 
  return GetInstance();
}


} // end namespace itk
