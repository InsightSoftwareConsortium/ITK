/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileReader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKIO.h"

#define ITK_WRAP_IFR(x) ITK_WRAP_IMAGE_SOURCE(ImageFileReader, x)

ITK_WRAP_CONFIG_GROUP(itkImageFileReader);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_IFR(F2);
ITK_WRAP_IFR(F3);
ITK_WRAP_IFR(US2);
ITK_WRAP_IFR(US3);

#endif
