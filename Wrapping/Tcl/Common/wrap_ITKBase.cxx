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
#include "itkImageIO.h"
#include "itkLightObject.h"
#include "itkObject.h"
#include "itkLightProcessObject.h"
#include "itkProcessObject.h"
#include "itkOutputWindow.h"
#include "itkVersion.h"
#include "itkImageIO.h"
#include "itkTimeStamp.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKBase);
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_OBJECT_TYPEDEF(Command);
      ITK_WRAP_OBJECT_TYPEDEF(DataObject);
      ITK_WRAP_OBJECT_TYPEDEF(Directory);
      ITK_WRAP_OBJECT_TYPEDEF(ImageIO);
      ITK_WRAP_OBJECT_TYPEDEF(LightObject);
      ITK_WRAP_OBJECT_TYPEDEF(Object);
      ITK_WRAP_OBJECT_TYPEDEF(LightProcessObject);
      ITK_WRAP_OBJECT_TYPEDEF(ProcessObject);
      ITK_WRAP_OBJECT_TYPEDEF(OutputWindow);
      ITK_WRAP_OBJECT_TYPEDEF(Version);
      ITK_WRAP_OBJECT_TYPEDEF(ImageIO);
      typedef ::itk::TimeStamp TimeStamp;
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  ITK_WRAP_OBJECT_SIZEOF(Command);
  ITK_WRAP_OBJECT_SIZEOF(DataObject);
  ITK_WRAP_OBJECT_SIZEOF(Directory);
  ITK_WRAP_OBJECT_SIZEOF(ImageIO);
  ITK_WRAP_OBJECT_SIZEOF(LightObject);
  ITK_WRAP_OBJECT_SIZEOF(Object);
  ITK_WRAP_OBJECT_SIZEOF(LightProcessObject);
  ITK_WRAP_OBJECT_SIZEOF(ProcessObject);
  ITK_WRAP_OBJECT_SIZEOF(OutputWindow);
  ITK_WRAP_OBJECT_SIZEOF(Version);
  ITK_WRAP_OBJECT_SIZEOF(ImageIO);
  sizeof(TimeStamp);
}

#endif
