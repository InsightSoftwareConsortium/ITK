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

itkOutputWindow* itkOutputWindow::m_Instance = 0;

// Prompting off by default
itkOutputWindow::itkOutputWindow()
{
  m_PromptUser = 0;
}

itkOutputWindow::~itkOutputWindow()
{
}

void itkOutputWindowDisplayText(const char* message)
{
  itkOutputWindow::GetInstance()->DisplayText(message);
}

void itkOutputWindow::PrintSelf(std::ostream& os, itkIndent indent)
{
  this->itkObject::PrintSelf(os, indent);

  os << indent << "itkOutputWindow (single instance): "
     << (void*)itkOutputWindow::m_Instance << endl;

  os << indent << "Prompt User: " << (m_PromptUser ? "On\n" : "Off\n");
}

// default implementation outputs to cerr only
void itkOutputWindow::DisplayText(const char* txt)
{
  std::cerr << txt;
  if ( m_PromptUser )
    {
    char c = 'n';
    std::cerr << "\nDo you want to suppress any further messages (y,n)?." 
              << endl;
    std::cin >> c;
    if ( c == 'y' || c == 'Y' )
      {
      itkObject::GlobalWarningDisplayOff(); 
      }
    }
}

// Return the single instance of the itkOutputWindow
itkOutputWindow* itkOutputWindow::GetInstance()
{
  if ( !itkOutputWindow::m_Instance )
    {
    // Try the factory first

    // if the factory did not provide one, then create it here
    if( ! itkOutputWindow::m_Instance )
      {
#ifdef _WIN32    
      itkOutputWindow::m_Instance = itkWin32OutputWindow::New();
#else
      itkOutputWindow::m_Instance = itkOutputWindow::New();
#endif
      }
    }
  // return the instance
  return itkOutputWindow::m_Instance;
}

void itkOutputWindow::SetInstance(itkOutputWindow* instance)
{
  if ( itkOutputWindow::m_Instance == instance )
    {
    return;
    }

  // preferably this will be NULL
  if ( itkOutputWindow::m_Instance )
    {
    itkOutputWindow::m_Instance->Delete();;
    itkOutputWindow::m_Instance = NULL;
    }

  itkOutputWindow::m_Instance = instance;

}

// Up the reference count so it behaves like New
itkOutputWindow* itkOutputWindow::New()
{
  itkOutputWindow* ret = itkOutputWindow::GetInstance();
  return ret;
}


