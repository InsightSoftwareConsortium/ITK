/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkExpNegativeImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkExpNegativeImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_ENIF(x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE(ExpNegativeImageFilter, x, y)

ITK_WRAP_CONFIG_GROUP(itkExpNegativeImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_ENIF(F2, F2);
ITK_WRAP_ENIF(F3, F3);
ITK_WRAP_ENIF(US3, US3);
ITK_WRAP_ENIF(US2, US2);

#endif
