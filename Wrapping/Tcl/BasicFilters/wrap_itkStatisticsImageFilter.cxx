/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkStatisticsImageFilter.cxx
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
#include "itkStatisticsImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_SIF(x) \
  ITK_WRAP_IMAGE_SINK(StatisticsImageFilter, x);

ITK_WRAP_CONFIG_GROUP(itkStatisticsImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_SIF(F2);
ITK_WRAP_SIF(F3);
ITK_WRAP_SIF(US2);
ITK_WRAP_SIF(US3);

#endif
