/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkHistogramMatchingImageFilter.cxx
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
#include "itkHistogramMatchingImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKAlgorithms.h"

#define ITK_WRAP_HMIF(x) \
  ITK_WRAP_IMAGE_TO_SAME_IMAGE(HistogramMatchingImageFilter, x)

ITK_WRAP_CONFIG_GROUP(itkHistogramMatchingImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_HMIF(F2);
ITK_WRAP_HMIF(F3);

#endif
