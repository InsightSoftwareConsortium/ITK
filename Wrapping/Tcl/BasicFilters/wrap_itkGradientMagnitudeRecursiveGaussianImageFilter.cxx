/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkGradientMagnitudeRecursiveGaussianImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_GMRGIF(x,y) ITK_WRAP_IMAGE_TO_IMAGE(GradientMagnitudeRecursiveGaussianImageFilter, x, y)

ITK_WRAP_CONFIG_GROUP(itkGradientMagnitudeRecursiveGaussianImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_GMRGIF(F2,F2);
ITK_WRAP_GMRGIF(F3,F3);
ITK_WRAP_GMRGIF(US2,US2);
ITK_WRAP_GMRGIF(US3,US3);

#endif
