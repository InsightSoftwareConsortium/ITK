/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImportImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImportImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImportImageFilter);
  namespace wrappers
  {
    ITK_WRAP_OBJECT2( ImportImageFilter, float,          2,  itkImportImageFilterF2  );
    ITK_WRAP_OBJECT2( ImportImageFilter, unsigned char,  2,  itkImportImageFilterUC2 );
    ITK_WRAP_OBJECT2( ImportImageFilter, unsigned short, 2,  itkImportImageFilterUS2 );
    ITK_WRAP_OBJECT2( ImportImageFilter, float,          3,  itkImportImageFilterF3  );
    ITK_WRAP_OBJECT2( ImportImageFilter, unsigned char,  3,  itkImportImageFilterUC3 );
    ITK_WRAP_OBJECT2( ImportImageFilter, unsigned short, 3,  itkImportImageFilterUS3 );
  }
}

#endif
