/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkBinaryThresholdImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBinaryThresholdImageFilter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKBasicFilters.h"

#define ITK_WRAP_BTIF(x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE(BinaryThresholdImageFilter, x, y) \
  ITK_WRAP_IMAGE_TO_IMAGE_SUPERCLASS(BinaryThresholdImageFilter, x, y)

ITK_WRAP_CONFIG_GROUP(itkBinaryThresholdImageFilter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_BTIF(F2, US2);
ITK_WRAP_BTIF(US2, US2);
ITK_WRAP_BTIF(F3, US3);
ITK_WRAP_BTIF(US3, US3);

#endif
