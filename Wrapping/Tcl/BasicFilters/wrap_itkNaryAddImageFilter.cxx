/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkNaryAddImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNaryAddImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_NAIF(x) \
  ITK_WRAP_IMAGE_TO_SAME_IMAGE(NaryAddImageFilter, x) \
  ITK_WRAP_IMAGE_TO_SAME_IMAGE_SUPERCLASS(NaryAddImageFilter, x)

ITK_WRAP_CONFIG_GROUP(itkNaryAddImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_NAIF(F2);
ITK_WRAP_NAIF(F3);
ITK_WRAP_NAIF(US2);
ITK_WRAP_NAIF(US3);

#endif
