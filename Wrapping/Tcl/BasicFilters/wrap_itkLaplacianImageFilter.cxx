/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkLaplacianImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkLaplacianImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_LIF(x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE(LaplacianImageFilter, x, y) 

ITK_WRAP_CONFIG_GROUP(itkLaplacianImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_LIF(F2, F2);
ITK_WRAP_LIF(F3, F3);

#endif
