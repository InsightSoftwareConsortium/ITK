/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFilterWatcher.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkSimpleFilterWatcher.h"

namespace itk
{

SimpleFilterWatcher
::SimpleFilterWatcher(ProcessObject* o, char *comment)
{
  // Initialize state
  m_Start = 0;
  m_End = 0;
  m_Process = o;
  m_Steps = 0;
  m_Comment = comment;
  m_TestAbort = false;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION == 730)
  m_Quiet = true;
#else
  m_Quiet = false;
#endif

  // Create a series of commands
  typedef SimpleMemberCommand<SimpleFilterWatcher> CommandType;
  CommandType::Pointer startFilterCommand;
  CommandType::Pointer endFilterCommand;
  CommandType::Pointer progressFilterCommand;
  CommandType::Pointer iterationFilterCommand;
  CommandType::Pointer abortFilterCommand;
  
  startFilterCommand =      CommandType::New();
  endFilterCommand =        CommandType::New();
  progressFilterCommand =   CommandType::New();
  iterationFilterCommand =  CommandType::New();
  abortFilterCommand =      CommandType::New();

  // Assign the callbacks
  startFilterCommand->SetCallbackFunction(this,
                                        &SimpleFilterWatcher::StartFilter);
  endFilterCommand->SetCallbackFunction(this,
                                        &SimpleFilterWatcher::EndFilter);
  progressFilterCommand->SetCallbackFunction(this,
                                        &SimpleFilterWatcher::ShowProgress);
  iterationFilterCommand->SetCallbackFunction(this,
                                        &SimpleFilterWatcher::ShowIteration);
  abortFilterCommand->SetCallbackFunction(this,
                                        &SimpleFilterWatcher::ShowAbort);


  // Add the commands as observers
  m_Process->AddObserver(StartEvent(), startFilterCommand);
  m_Process->AddObserver(EndEvent(), endFilterCommand);
  m_Process->AddObserver(ProgressEvent(), progressFilterCommand);
  m_Process->AddObserver(IterationEvent(), iterationFilterCommand);
  m_Process->AddObserver(AbortEvent(), abortFilterCommand);
}


} // end namespace itk

