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

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKBase);
  namespace wrappers
  {
    namespace itk
    {
      ITK_WRAP_BASE_TYPEDEF(Command);
      ITK_WRAP_BASE_TYPEDEF(DataObject);
      ITK_WRAP_BASE_TYPEDEF(Directory);
      ITK_WRAP_BASE_TYPEDEF(ImageIO);
      ITK_WRAP_BASE_TYPEDEF(LightObject);
      ITK_WRAP_BASE_TYPEDEF(Object);
      ITK_WRAP_BASE_TYPEDEF(LightProcessObject);
      ITK_WRAP_BASE_TYPEDEF(ProcessObject);
      ITK_WRAP_BASE_TYPEDEF(OutputWindow);
      ITK_WRAP_BASE_TYPEDEF(Version);
    }
  }
}

void force_instantiate()
{
  ITK_WRAP_BASE_SIZEOF(Command);
  ITK_WRAP_BASE_SIZEOF(DataObject);
  ITK_WRAP_BASE_SIZEOF(Directory);
  ITK_WRAP_BASE_SIZEOF(ImageIO);
  ITK_WRAP_BASE_SIZEOF(LightObject);
  ITK_WRAP_BASE_SIZEOF(Object);
  ITK_WRAP_BASE_SIZEOF(LightProcessObject);
  ITK_WRAP_BASE_SIZEOF(ProcessObject);
  ITK_WRAP_BASE_SIZEOF(OutputWindow);
  ITK_WRAP_BASE_SIZEOF(Version);
}

#endif
