/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkCastImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCastImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_CIF(x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE(CastImageFilter, x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE_SUPERCLASS(CastImageFilter, x, y)

ITK_WRAP_CONFIG_GROUP(itkCastImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_CIF(F2, US2);
ITK_WRAP_CIF(US2, F2);
ITK_WRAP_CIF(F2, F2);
ITK_WRAP_CIF(US2, US2);
ITK_WRAP_CIF(F3, US3);
ITK_WRAP_CIF(US3, F3);
ITK_WRAP_CIF(F3, F3);
ITK_WRAP_CIF(US3, US3);

#endif
