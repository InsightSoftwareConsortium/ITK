/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedMiniPipelineProgressCommand.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkWatershedMiniPipelineProgressCommand.h"


namespace itk {

void WatershedMiniPipelineProgressCommand
::Execute(Object *caller, const EventObject &event)
{
  ProcessObject *po = dynamic_cast<ProcessObject *>(caller);
  if (! po) return;
  
  if( typeid(event) == typeid ( ProgressEvent)  )
    {
    m_Filter->UpdateProgress( (m_Count + po->GetProgress()) /
                              m_NumberOfFilters);
    if (po->GetProgress() == 1.0) m_Count += 1.0;
    }
}

void WatershedMiniPipelineProgressCommand
::Execute(const Object *caller, const EventObject &event)
{
  ProcessObject *po = dynamic_cast<ProcessObject *>(const_cast<Object *>(caller));
  if (! po) return;
  
  if( typeid(event) == typeid ( ProgressEvent)  )
    {
    m_Filter->UpdateProgress( (m_Count + po->GetProgress()) /
                              m_NumberOfFilters);
    if (po->GetProgress() == 1.0) m_Count += 1.0;
    }
}










} // end namespace itk

