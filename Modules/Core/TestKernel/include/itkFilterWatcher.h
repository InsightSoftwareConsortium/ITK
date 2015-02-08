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
#ifndef itkFilterWatcher_h
#define itkFilterWatcher_h

#include "itkCommand.h"
#include "itkProcessObject.h"
#include <ctime>
// The following class is a convenience  to watch the progress of a filter

class FilterWatcher
{
public:
  FilterWatcher(itk::ProcessObject* o, const char *comment="")
  {
    m_Start = 0; m_End = 0; m_Process = o; m_Steps = 0; m_Comment = comment;
    m_Iterations = 0;
    m_TestAbort = false;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION == 730)
    m_Quiet = true;
#else
    m_Quiet = false;
#endif
    itk::SimpleMemberCommand<FilterWatcher>::Pointer startFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer endFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer progressFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer iterationFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer abortFilterCommand;

    startFilterCommand =    itk::SimpleMemberCommand<FilterWatcher>::New();
    endFilterCommand =      itk::SimpleMemberCommand<FilterWatcher>::New();
    progressFilterCommand = itk::SimpleMemberCommand<FilterWatcher>::New();
    iterationFilterCommand = itk::SimpleMemberCommand<FilterWatcher>::New();
    abortFilterCommand = itk::SimpleMemberCommand<FilterWatcher>::New();

    startFilterCommand->SetCallbackFunction(this,
                                            &FilterWatcher::StartFilter);
    endFilterCommand->SetCallbackFunction(this,
                                          &FilterWatcher::EndFilter);
    progressFilterCommand->SetCallbackFunction(this,
                                               &FilterWatcher::ShowProgress);
    iterationFilterCommand->SetCallbackFunction(this,
                                               &FilterWatcher::ShowIteration);
    abortFilterCommand->SetCallbackFunction(this,
                                               &FilterWatcher::ShowAbort);
    m_Process->AddObserver(itk::StartEvent(), startFilterCommand);
    m_Process->AddObserver(itk::EndEvent(), endFilterCommand);
    m_Process->AddObserver(itk::ProgressEvent(), progressFilterCommand);
    m_Process->AddObserver(itk::IterationEvent(), iterationFilterCommand);
    m_Process->AddObserver(itk::AbortEvent(), abortFilterCommand);
  }

  virtual ~FilterWatcher() {}

  virtual void ShowProgress()
  {
    m_Steps++;
    if (!m_Quiet)
      {
      std::cout << " | " << m_Process->GetProgress() << std::flush;
      if ((m_Steps % 10) == 0)
        {
        std::cout << std::endl;
        }
      }
    if (m_TestAbort)
      {
      if (m_Process->GetProgress() > .03)
        {
        m_Process->AbortGenerateDataOn();
        }
      }
  }
  virtual void ShowAbort()
  {
    std::cout << std::endl << "      ABORT" << std::endl << std::flush;
  }
  virtual void ShowIteration()
  {
    std::cout << " # " << std::flush;
    m_Iterations++;
  }
  virtual void StartFilter()
  {
    m_Steps = 0;
    m_Iterations = 0;
    m_Start = ::clock();
    std::cout << "-------- Start " << m_Process->GetNameOfClass()
              << " \"" << m_Comment << "\" "
              << m_Process
              << (m_Quiet ? "Progress Quiet " : "Progress ")
              << std::flush;
    }
  const char *GetNameOfClass () {return m_Process->GetNameOfClass();}
  virtual void EndFilter()
  {
    m_End = ::clock();
    std::cout << std::endl << "Filter took "
              << static_cast<double>(m_End - m_Start) / CLOCKS_PER_SEC
              << " seconds.";
    std::cout << std::endl << std::endl
              << "-------- End " << m_Process->GetNameOfClass()
              << " \"" << m_Comment << "\" "
              << m_Process << std::flush;
    if (m_Steps < 1)
      {
      itkExceptionMacro ("Filter does not have progress.");
      }
    }

  void QuietOn() {m_Quiet = true;}
  void QuietOff() {m_Quiet = false;}
  void TestAbortOn() {m_TestAbort = true;}
  void TestAbortOff() {m_TestAbort = false;}

protected:
  clock_t m_Start;
  clock_t m_End;
  int     m_Steps;
  int     m_Iterations;
  bool    m_Quiet;
  bool    m_TestAbort;

  std::string                 m_Comment;
  itk::ProcessObject::Pointer m_Process;

private:
  FilterWatcher(); // Purposely not implemented
};

#endif
