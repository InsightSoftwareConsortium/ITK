/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageRegistrationMethod.cxx
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
#include "itkImageRegistrationMethod.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKAlgorithms.h"

#define ITK_WRAP_IRM(x) \
  ITK_WRAP_IMAGE_TO_SAME_IMAGE(ImageRegistrationMethod, x)

ITK_WRAP_CONFIG_GROUP(itkImageRegistrationMethod);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_IRM(F2);
ITK_WRAP_IRM(F3);
ITK_WRAP_IRM(US2);
ITK_WRAP_IRM(US3);

#endif
