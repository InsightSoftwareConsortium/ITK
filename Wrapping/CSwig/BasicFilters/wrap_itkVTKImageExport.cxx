/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkVTKImageExport.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVTKImageExport.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkVTKImageExport);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(VTKImageExport, image::F2,
                                     itkVTKImageExportF2);
    ITK_WRAP_OBJECT1(VTKImageExport, image::UC2,
                                     itkVTKImageExportUC2);
    ITK_WRAP_OBJECT1(VTKImageExport, image::US2,
                                     itkVTKImageExportUS2);
    ITK_WRAP_OBJECT1(VTKImageExport, image::F3, 
                                     itkVTKImageExportF3);
    ITK_WRAP_OBJECT1(VTKImageExport, image::UC3, 
                                     itkVTKImageExportUC3);
    ITK_WRAP_OBJECT1(VTKImageExport, image::US3, 
                                     itkVTKImageExportUS3);
  }
}

#endif
