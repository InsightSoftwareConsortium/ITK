/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkGradientAnisotropicDiffusionImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_GADIF(x) \
  ITK_WRAP_IMAGE_TO_SAME_IMAGE(GradientAnisotropicDiffusionImageFilter, x);

ITK_WRAP_CONFIG_GROUP(itkGradientAnisotropicDiffusionImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_GADIF(F2);
ITK_WRAP_GADIF(F3);
ITK_WRAP_GADIF(US2);
ITK_WRAP_GADIF(US3);

#endif
