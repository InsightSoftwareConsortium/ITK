/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCommand.h"
#include "itkDataObject.h"
#include "itkDirectory.h"
#include "itkLightObject.h"
#include "itkObject.h"
#include "itkLightProcessObject.h"
#include "itkProcessObject.h"
#include "itkOutputWindow.h"
#include "itkVersion.h"
#include "itkTimeStamp.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

ITK_WRAP_CONFIG_GROUP(ITKBase);
ITK_WRAP_OBJECT(Command);
ITK_WRAP_OBJECT(DataObject);
ITK_WRAP_OBJECT(Directory);
ITK_WRAP_OBJECT(LightObject);
ITK_WRAP_OBJECT(Object);
ITK_WRAP_OBJECT(LightProcessObject);
ITK_WRAP_OBJECT(ProcessObject);
ITK_WRAP_OBJECT(OutputWindow);
ITK_WRAP_OBJECT(Version);
ITK_WRAP_CLASS(TimeStamp);

#endif
