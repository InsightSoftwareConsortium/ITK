/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_ITKIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE_NAME(ITK_WRAP_PACKAGE);
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(IOBase),
    ITK_WRAP_GROUP(itkImageFileReader_2D),
    ITK_WRAP_GROUP(itkImageFileReader_3D),
#ifdef ITK_TCL_WRAP
    ITK_WRAP_GROUP(itkTkImageViewer2D),
#endif
    ITK_WRAP_GROUP(itkImageFileWriter_2D),
    ITK_WRAP_GROUP(itkImageFileWriter_3D),
    ITK_WRAP_GROUP(itkImageSeriesReader),
    ITK_WRAP_GROUP(itkImageSeriesWriter)
  };
}
#endif
