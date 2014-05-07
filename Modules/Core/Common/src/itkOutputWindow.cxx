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
#ifdef _WIN32
# include "itkWin32OutputWindow.h"
#else
# include "itkOutputWindow.h"
# include "itkObjectFactory.h"
#endif

namespace itk
{
OutputWindow::Pointer OutputWindow:: m_Instance = ITK_NULLPTR;

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
{}

void
OutputWindowDisplayText(const char *message)
{
  OutputWindow::GetInstance()->DisplayText(message);
}

void
OutputWindowDisplayErrorText(const char *message)
{
  OutputWindow::GetInstance()->DisplayErrorText(message);
}

void
OutputWindowDisplayWarningText(const char *message)
{
  OutputWindow::GetInstance()->DisplayWarningText(message);
}

void
OutputWindowDisplayGenericOutputText(const char *message)
{
  OutputWindow::GetInstance()->DisplayGenericOutputText(message);
}

void
OutputWindowDisplayDebugText(const char *message)
{
  OutputWindow::GetInstance()->DisplayDebugText(message);
}

void
OutputWindow
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutputWindow (single instance): "
     << (void *)OutputWindow::m_Instance << std::endl;

  os << indent << "Prompt User: " << ( m_PromptUser ? "On\n" : "Off\n" );
}

/**
 * default implementation outputs to cerr only
 */
void
OutputWindow
::DisplayText(const char *txt)
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
    OutputWindow::m_Instance  = ObjectFactory< Self >::Create();
    // if the factory did not provide one, then create it here
    if ( !OutputWindow::m_Instance )
      {
      // For the windows OS, use a special output window
#ifdef _WIN32
      OutputWindow::m_Instance = Win32OutputWindow::New();
#else
      OutputWindow::m_Instance = new OutputWindow;
      // Remove extra reference from construction.
      OutputWindow::m_Instance->UnRegister();
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
::SetInstance(OutputWindow *instance)
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
