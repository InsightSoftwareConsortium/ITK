/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkTernaryMagnitudeImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTernaryMagnitudeImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_TMIF(x) \
  ITK_WRAP_TERNARY_IMAGE_TO_IMAGE(TernaryMagnitudeImageFilter, x, x) \
  ITK_WRAP_TERNARY_IMAGE_TO_IMAGE_SUPERCLASS(TernaryMagnitudeImageFilter, x, x)

ITK_WRAP_CONFIG_GROUP(itkTernaryMagnitudeImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_TMIF(F2);
ITK_WRAP_TMIF(F3);
ITK_WRAP_TMIF(US2);
ITK_WRAP_TMIF(US3);

#endif
