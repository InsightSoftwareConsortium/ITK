/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterWatcher.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFilterWatcher_h
#define _itkFilterWatcher_h

#include "itkCommand.h"
#include <time.h>
// The following class is a convenience  to watch the progress of a filter

class FilterWatcher
{
public:
  FilterWatcher(itk::ProcessObject* o, char *comment="")
  {m_Start = 0; m_End = 0; m_Process = o; m_Steps = 0; m_Comment = comment;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer startFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer endFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer progressFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer iterationFilterCommand;
  
    startFilterCommand =    itk::SimpleMemberCommand<FilterWatcher>::New();
    endFilterCommand =      itk::SimpleMemberCommand<FilterWatcher>::New();
    progressFilterCommand = itk::SimpleMemberCommand<FilterWatcher>::New();
    iterationFilterCommand = itk::SimpleMemberCommand<FilterWatcher>::New();

    startFilterCommand->SetCallbackFunction(this,
                                            &FilterWatcher::StartFilter);
    endFilterCommand->SetCallbackFunction(this,
                                          &FilterWatcher::EndFilter);
    progressFilterCommand->SetCallbackFunction(this,
                                               &FilterWatcher::ShowProgress);
    iterationFilterCommand->SetCallbackFunction(this,
                                               &FilterWatcher::ShowIteration);
    m_Process->AddObserver(itk::StartEvent(), startFilterCommand);
    m_Process->AddObserver(itk::EndEvent(), endFilterCommand);
    m_Process->AddObserver(itk::ProgressEvent(), progressFilterCommand);
    m_Process->AddObserver(itk::IterationEvent(), iterationFilterCommand);
    }

  virtual ~FilterWatcher() {}

  virtual void ShowProgress()
    {
      std::cout << " | " << m_Process->GetProgress() << std::flush;
      m_Steps++;
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
              << "Progress " << std::flush;
    }
  const char *GetNameOfClass () {return m_Process->GetNameOfClass();}
  virtual void EndFilter()
    {
    m_End = ::clock();
    std::cout << std::endl << "Filter took " << static_cast<double>(m_End - m_Start) / CLOCKS_PER_SEC << " seconds.";
    std::cout << std::endl << std::endl << "-------- End " << m_Process->GetNameOfClass()
              << " \"" << m_Comment << "\" "
              << m_Process << std::flush;
    if (m_Steps < 1)
      {
      itkExceptionMacro ("Filter does not have progress.");
      }
    }
protected:
  clock_t m_Start;
  clock_t m_End;
  int m_Steps;
  int m_Iterations;
  std::string m_Comment;
  itk::ProcessObject::Pointer m_Process;
private:
  FilterWatcher(); // Purposely not implemented
};

#endif
