/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkEventObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkEventObject.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkEventObject);
  namespace wrappers
  {
    namespace itk
    {
      typedef ::itk::EventObject     EventObject;
      typedef ::itk::NoEvent         NoEvent;
      typedef ::itk::AnyEvent        AnyEvent;
      typedef ::itk::DeleteEvent     DeleteEvent;
      typedef ::itk::StartEvent      StartEvent;
      typedef ::itk::EndEvent        EndEvent;
      typedef ::itk::ProgressEvent   ProgressEvent;
      typedef ::itk::ExitEvent       ExitEvent;
      typedef ::itk::ModifiedEvent   ModifiedEvent;
      typedef ::itk::IterationEvent  IterationEvent;
      typedef ::itk::PickEvent       PickEvent;
      typedef ::itk::StartPickEvent  StartPickEvent;
      typedef ::itk::EndPickEvent    EndPickEvent;
      typedef ::itk::AbortCheckEvent AbortCheckEvent;
      typedef ::itk::UserEvent       UserEvent;
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  sizeof(EventObject);
  sizeof(NoEvent);
  sizeof(AnyEvent);
  sizeof(DeleteEvent);
  sizeof(StartEvent);
  sizeof(EndEvent);
  sizeof(ProgressEvent);
  sizeof(ExitEvent);
  sizeof(ModifiedEvent);
  sizeof(IterationEvent);
  sizeof(PickEvent);
  sizeof(StartPickEvent);
  sizeof(EndPickEvent);
  sizeof(AbortCheckEvent);
  sizeof(UserEvent);
}

#endif
