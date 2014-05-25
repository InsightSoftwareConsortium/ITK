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
#include "itkSimpleFilterWatcher.h"

namespace itk
{
SimpleFilterWatcher
::SimpleFilterWatcher(ProcessObject *o, const char *comment)
{
  // Initialize state
  m_Process = o;
  m_Steps = 0;
  m_Comment = comment;
  m_TestAbort = false;
  m_Iterations = 0;
#if defined( _COMPILER_VERSION ) && ( _COMPILER_VERSION == 730 )
  m_Quiet = true;
#else
  m_Quiet = false;
#endif

  // Create a series of commands
  m_StartFilterCommand =      CommandType::New();
  m_EndFilterCommand =        CommandType::New();
  m_ProgressFilterCommand =   CommandType::New();
  m_IterationFilterCommand =  CommandType::New();
  m_AbortFilterCommand =      CommandType::New();

  // Assign the callbacks
  m_StartFilterCommand->SetCallbackFunction(this,
                                            &SimpleFilterWatcher::StartFilter);
  m_EndFilterCommand->SetCallbackFunction(this,
                                          &SimpleFilterWatcher::EndFilter);
  m_ProgressFilterCommand->SetCallbackFunction(this,
                                               &SimpleFilterWatcher::ShowProgress);
  m_IterationFilterCommand->SetCallbackFunction(this,
                                                &SimpleFilterWatcher::ShowIteration);
  m_AbortFilterCommand->SetCallbackFunction(this,
                                            &SimpleFilterWatcher::ShowAbort);

  // Add the commands as observers
  m_StartTag = m_Process->AddObserver(StartEvent(), m_StartFilterCommand);
  m_EndTag = m_Process->AddObserver(EndEvent(), m_EndFilterCommand);
  m_ProgressTag =
    m_Process->AddObserver(ProgressEvent(), m_ProgressFilterCommand);
  m_IterationTag =
    m_Process->AddObserver(IterationEvent(), m_IterationFilterCommand);
  m_AbortTag =
    m_Process->AddObserver(AbortEvent(), m_AbortFilterCommand);
}

SimpleFilterWatcher
::SimpleFilterWatcher() :
  m_Steps(0),
  m_Iterations(0),
#if defined( _COMPILER_VERSION ) && ( _COMPILER_VERSION == 730 )
  m_Quiet(true),
#else
  m_Quiet(false),
#endif
  m_TestAbort(false),
  m_Comment("Not watching an object"),
  m_Process(ITK_NULLPTR),
  m_StartTag(0),
  m_EndTag(0),
  m_ProgressTag(0),
  m_IterationTag(0),
  m_AbortTag(0)
{
}

SimpleFilterWatcher
::SimpleFilterWatcher(const SimpleFilterWatcher & watch)
{
  // Remove any observers we have on the old process object
  if ( m_Process )
    {
    if ( m_StartFilterCommand )
      {
      m_Process->RemoveObserver(m_StartTag);
      }
    if ( m_EndFilterCommand )
      {
      m_Process->RemoveObserver(m_EndTag);
      }
    if ( m_ProgressFilterCommand )
      {
      m_Process->RemoveObserver(m_ProgressTag);
      }
    if ( m_IterationFilterCommand )
      {
      m_Process->RemoveObserver(m_IterationTag);
      }
    if ( m_AbortFilterCommand )
      {
      m_Process->RemoveObserver(m_AbortTag);
      }
    }

  // Initialize state
  m_TimeProbe = watch.m_TimeProbe;
  m_Process = watch.m_Process;
  m_Steps = watch.m_Steps;
  m_Iterations = watch.m_Iterations;
  m_Comment = watch.m_Comment;
  m_TestAbort = watch.m_TestAbort;
  m_Quiet = watch.m_Quiet;

  m_StartTag = 0;
  m_EndTag = 0;
  m_ProgressTag = 0;
  m_IterationTag = 0;
  m_AbortTag = 0;

  // Create a series of commands
  if ( m_Process )
    {
    m_StartFilterCommand =      CommandType::New();
    m_EndFilterCommand =        CommandType::New();
    m_ProgressFilterCommand =   CommandType::New();
    m_IterationFilterCommand =  CommandType::New();
    m_AbortFilterCommand =      CommandType::New();

    // Assign the callbacks
    m_StartFilterCommand->SetCallbackFunction(this,
                                              &SimpleFilterWatcher::StartFilter);
    m_EndFilterCommand->SetCallbackFunction(this,
                                            &SimpleFilterWatcher::EndFilter);
    m_ProgressFilterCommand->SetCallbackFunction(this,
                                                 &SimpleFilterWatcher::ShowProgress);
    m_IterationFilterCommand->SetCallbackFunction(this,
                                                  &SimpleFilterWatcher::ShowIteration);
    m_AbortFilterCommand->SetCallbackFunction(this,
                                              &SimpleFilterWatcher::ShowAbort);

    // Add the commands as observers
    m_StartTag = m_Process->AddObserver(StartEvent(), m_StartFilterCommand);
    m_EndTag = m_Process->AddObserver(EndEvent(), m_EndFilterCommand);
    m_ProgressTag =
      m_Process->AddObserver(ProgressEvent(), m_ProgressFilterCommand);
    m_IterationTag =
      m_Process->AddObserver(IterationEvent(), m_IterationFilterCommand);
    m_AbortTag = m_Process->AddObserver(AbortEvent(), m_AbortFilterCommand);
    }
}

SimpleFilterWatcher &
SimpleFilterWatcher
::operator=(const SimpleFilterWatcher & watch)
{
  if(this != &watch)
    {
    // Remove any observers we have on the old process object
    if ( m_Process )
      {
      if ( m_StartFilterCommand )
        {
        m_Process->RemoveObserver(m_StartTag);
        }
      if ( m_EndFilterCommand )
        {
        m_Process->RemoveObserver(m_EndTag);
        }
      if ( m_ProgressFilterCommand )
        {
        m_Process->RemoveObserver(m_ProgressTag);
        }
      if ( m_IterationFilterCommand )
        {
        m_Process->RemoveObserver(m_IterationTag);
        }
      if ( m_AbortFilterCommand )
        {
        m_Process->RemoveObserver(m_AbortTag);
        }
      }

    // Initialize state
    m_TimeProbe = watch.m_TimeProbe;
    m_Process = watch.m_Process;
    m_Steps = watch.m_Steps;
    m_Iterations = watch.m_Iterations;
    m_Comment = watch.m_Comment;
    m_TestAbort = watch.m_TestAbort;
    m_Quiet = watch.m_Quiet;

    m_StartTag = 0;
    m_EndTag = 0;
    m_ProgressTag = 0;
    m_IterationTag = 0;
    m_AbortTag = 0;

    // Create a series of commands
    if ( m_Process )
      {
      m_StartFilterCommand =      CommandType::New();
      m_EndFilterCommand =        CommandType::New();
      m_ProgressFilterCommand =   CommandType::New();
      m_IterationFilterCommand =  CommandType::New();
      m_AbortFilterCommand =      CommandType::New();

      // Assign the callbacks
      m_StartFilterCommand->SetCallbackFunction(this,
                                                &SimpleFilterWatcher::StartFilter);
      m_EndFilterCommand->SetCallbackFunction(this,
                                              &SimpleFilterWatcher::EndFilter);
      m_ProgressFilterCommand->SetCallbackFunction(this,
                                                   &SimpleFilterWatcher::ShowProgress);
      m_IterationFilterCommand->SetCallbackFunction(this,
                                                    &SimpleFilterWatcher::ShowIteration);
      m_AbortFilterCommand->SetCallbackFunction(this,
                                                &SimpleFilterWatcher::ShowAbort);

      // Add the commands as observers
      m_StartTag = m_Process->AddObserver(StartEvent(), m_StartFilterCommand);
      m_EndTag = m_Process->AddObserver(EndEvent(), m_EndFilterCommand);
      m_ProgressTag =
        m_Process->AddObserver(ProgressEvent(), m_ProgressFilterCommand);
      m_IterationTag =
        m_Process->AddObserver(IterationEvent(), m_IterationFilterCommand);
      m_AbortTag = m_Process->AddObserver(AbortEvent(), m_AbortFilterCommand);
      }
    }
  return *this;
}

SimpleFilterWatcher
::~SimpleFilterWatcher()
{
  // Remove any observers we have on the old process object
  if ( m_Process )
    {
    if ( m_StartFilterCommand )
      {
      m_Process->RemoveObserver(m_StartTag);
      }
    if ( m_EndFilterCommand )
      {
      m_Process->RemoveObserver(m_EndTag);
      }
    if ( m_ProgressFilterCommand )
      {
      m_Process->RemoveObserver(m_ProgressTag);
      }
    if ( m_IterationFilterCommand )
      {
      m_Process->RemoveObserver(m_IterationTag);
      }
    if ( m_AbortFilterCommand )
      {
      m_Process->RemoveObserver(m_AbortTag);
      }
    }
}
} // end namespace itk
