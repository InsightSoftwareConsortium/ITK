/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkImageFileWriter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileWriter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKIO.h"

#define ITK_WRAP_IFW(x) ITK_WRAP_IMAGE_SINK(ImageFileWriter, x)

ITK_WRAP_CONFIG_GROUP(itkImageFileWriter);
ITK_WRAP_DEFINE_IMAGE_TYPES();

ITK_WRAP_IFW(F2);
ITK_WRAP_IFW(F3);
ITK_WRAP_IFW(US2);
ITK_WRAP_IFW(US3);
ITK_WRAP_IFW(UC2);


#endif
