/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_IOBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRawImageIO.h"
#include "itkImageIOBase.h"
#include "itkPNGImageIO.h"
#include "itkMetaImageIO.h"
#include "itkPNGImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkDicomImageIOFactory.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKIO.h"

ITK_WRAP_CONFIG_GROUP(IOBase);
ITK_WRAP_OBJECT(ImageIOBase);
ITK_WRAP_OBJECT(PNGImageIO);
ITK_WRAP_OBJECT(MetaImageIO);
ITK_WRAP_OBJECT(PNGImageIOFactory);
ITK_WRAP_OBJECT(MetaImageIOFactory);
ITK_WRAP_OBJECT(DicomImageIOFactory);
ITK_WRAP_OBJECT_TEMPLATE_2(RawImageIOF2, RawImageIO<float, 2>);
ITK_WRAP_OBJECT_TEMPLATE_2(RawImageIOF3, RawImageIO<float, 3>);

#endif
