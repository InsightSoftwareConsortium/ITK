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

ITK_WRAP_CONFIG_GROUP(itkEventObject);

ITK_WRAP_CLASS(EventObject);
ITK_WRAP_CLASS(NoEvent);
ITK_WRAP_CLASS(AnyEvent);
ITK_WRAP_CLASS(DeleteEvent);
ITK_WRAP_CLASS(StartEvent);
ITK_WRAP_CLASS(EndEvent);
ITK_WRAP_CLASS(ProgressEvent);
ITK_WRAP_CLASS(ExitEvent);
ITK_WRAP_CLASS(ModifiedEvent);
ITK_WRAP_CLASS(IterationEvent);
ITK_WRAP_CLASS(PickEvent);
ITK_WRAP_CLASS(StartPickEvent);
ITK_WRAP_CLASS(EndPickEvent);
ITK_WRAP_CLASS(AbortCheckEvent);
ITK_WRAP_CLASS(UserEvent);

#endif
