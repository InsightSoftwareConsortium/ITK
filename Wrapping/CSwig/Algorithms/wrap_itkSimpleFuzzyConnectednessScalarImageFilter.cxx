/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkSimpleFuzzyConnectednessScalarImageFilter.cxx
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
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKAlgorithms.h"

#define ITK_WRAP_SFCSIF(x) \
  ITK_WRAP_IMAGE_TO_SAME_IMAGE(SimpleFuzzyConnectednessScalarImageFilter, x)

ITK_WRAP_CONFIG_GROUP(itkSimpleFuzzyConnectednessScalarImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_SFCSIF(F2);
ITK_WRAP_SFCSIF(F3);
ITK_WRAP_SFCSIF(US2);
ITK_WRAP_SFCSIF(US3);

#endif
