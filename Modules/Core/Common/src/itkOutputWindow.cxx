/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkOutputWindow.h"
#include "itkObjectFactory.h"
#include "itkSingleton.h"
#include <mutex>

namespace itk
{

struct OutputWindowGlobals
{
  OutputWindow::Pointer m_Instance{ nullptr };
  std::recursive_mutex  m_StaticInstanceLock;
};

/**
 * Prompting off by default
 */
OutputWindow::OutputWindow()
{
  m_PromptUser = false;
}

OutputWindow::~OutputWindow() = default;

void
OutputWindowDisplayText(const char * message)
{
  OutputWindow::GetInstance()->DisplayText(message);
}

void
OutputWindowDisplayErrorText(const char * message)
{
  OutputWindow::GetInstance()->DisplayErrorText(message);
}

void
OutputWindowDisplayWarningText(const char * message)
{
  OutputWindow::GetInstance()->DisplayWarningText(message);
}

void
OutputWindowDisplayGenericOutputText(const char * message)
{
  OutputWindow::GetInstance()->DisplayGenericOutputText(message);
}

void
OutputWindowDisplayDebugText(const char * message)
{
  OutputWindow::GetInstance()->DisplayDebugText(message);
}

void
OutputWindow::PrintSelf(std::ostream & os, Indent indent) const
{
  itkInitGlobalsMacro(PimplGlobals);
  Superclass::PrintSelf(os, indent);

  os << indent << "OutputWindow (single instance): " << (void *)m_PimplGlobals->m_Instance << std::endl;

  os << indent << "Prompt User: " << (m_PromptUser ? "On\n" : "Off\n");
}

/**
 * default implementation outputs to cerr only
 */
void
OutputWindow::DisplayText(const char * txt)
{
  std::lock_guard<std::mutex> cerrLock(m_cerrMutex);
  std::cerr << txt;
  if (m_PromptUser)
  {
    char c = 'n';
    std::cerr << "\nDo you want to suppress any further messages (y,n)?." << std::endl;
    std::cin >> c;
    if (c == 'y' || c == 'Y')
    {
      Object::GlobalWarningDisplayOff();
    }
  }
}

/**
 * Return the single instance of the OutputWindow
 */
OutputWindow::Pointer
OutputWindow::GetInstance()
{
  itkInitGlobalsMacro(PimplGlobals);
  std::lock_guard<std::recursive_mutex> mutexHolder(m_PimplGlobals->m_StaticInstanceLock);
  if (!m_PimplGlobals->m_Instance)
  {
    // Try the factory first
    m_PimplGlobals->m_Instance = ObjectFactory<Self>::Create();
    // if the factory did not provide one, then create it here
    if (!m_PimplGlobals->m_Instance)
    {
      m_PimplGlobals->m_Instance = new OutputWindow;
      // Remove extra reference from construction.
      m_PimplGlobals->m_Instance->UnRegister();
    }
  }
  /**
   * return the instance
   */
  return m_PimplGlobals->m_Instance;
}

itkGetGlobalSimpleMacro(OutputWindow, OutputWindowGlobals, PimplGlobals);

OutputWindowGlobals * OutputWindow::m_PimplGlobals;

void
OutputWindow::SetInstance(OutputWindow * instance)
{
  itkInitGlobalsMacro(PimplGlobals);

  std::lock_guard<std::recursive_mutex> mutexHolder(m_PimplGlobals->m_StaticInstanceLock);
  if (m_PimplGlobals->m_Instance == instance)
  {
    return;
  }
  m_PimplGlobals->m_Instance = instance;
}

/**
 * This just calls GetInstance
 */
OutputWindow::Pointer
OutputWindow::New()
{
  return GetInstance();
}
} // end namespace itk
