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
#ifdef _MSC_VER
#include "itkWin32OutputWindow.h"
#endif
#include "itkObjectFactory.h"

namespace itk
{
  
OutputWindow* OutputWindow::m_Instance = 0;

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
  this->Object::PrintSelf(os, indent);

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
OutputWindow* 
OutputWindow
::GetInstance()
{
  if ( !OutputWindow::m_Instance )
    {
    /**
     * Try the factory first
     */

    /**
     * if the factory did not provide one, then create it here
     */
    if( ! OutputWindow::m_Instance )
      {
#ifdef _MSC_VER
      OutputWindow::m_Instance = Win32OutputWindow::New();
#else
      OutputWindow::m_Instance = OutputWindow::New();
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

  /**
   * preferably this will be NULL
   */
  if ( OutputWindow::m_Instance )
    {
    OutputWindow::m_Instance->Delete();;
    OutputWindow::m_Instance = NULL;
    }

  OutputWindow::m_Instance = instance;

}

/**
 * Up the reference count so it behaves like New
 */
OutputWindow* 
OutputWindow
::New()
{ 
  if ( ! OutputWindow::m_Instance )
    {
    OutputWindow::m_Instance = new OutputWindow;
    }
  
  return OutputWindow::m_Instance;
}


} // end namespace itk
