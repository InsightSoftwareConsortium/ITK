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
// The following class supports callbacks
// on the filter in the pipeline that follows later
class FilterWatcher
{
public:
  FilterWatcher(itk::ProcessObject* o)
    {m_Process = o;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer startFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer endFilterCommand;
    itk::SimpleMemberCommand<FilterWatcher>::Pointer progressFilterCommand;
  
    startFilterCommand =    itk::SimpleMemberCommand<FilterWatcher>::New();
    endFilterCommand =      itk::SimpleMemberCommand<FilterWatcher>::New();
    progressFilterCommand = itk::SimpleMemberCommand<FilterWatcher>::New();

    startFilterCommand->SetCallbackFunction(this,
                                            &FilterWatcher::StartFilter);
    endFilterCommand->SetCallbackFunction(this,
                                          &FilterWatcher::EndFilter);
    progressFilterCommand->SetCallbackFunction(this,
                                               &FilterWatcher::ShowProgress);
    m_Process->AddObserver(itk::StartEvent(), startFilterCommand);
    m_Process->AddObserver(itk::EndEvent(), endFilterCommand);
    m_Process->AddObserver(itk::ProgressEvent(), progressFilterCommand);
    }
  virtual void ShowProgress()
    {
    std::cout << " | " << m_Process->GetProgress();
    }
  virtual void StartFilter()
    {
    std::cout << "-------- Start Filter " << m_Process->GetNameOfClass()
              << m_Process
              << "Progress ";
    }
  virtual void EndFilter()
    {
      std::cout << std::endl << std::endl << "-------- End Filter " << m_Process->GetNameOfClass()
              << m_Process;
    }
  itk::ProcessObject::Pointer m_Process;
};

#endif
