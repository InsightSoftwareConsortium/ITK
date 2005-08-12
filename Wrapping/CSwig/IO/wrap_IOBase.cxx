/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_IOBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRawImageIO.h"
#include "itkImageIOBase.h"
#include "itkPNGImageIO.h"
#include "itkMetaImageIO.h"
#include "itkDicomImageIO.h"
#include "itkGDCMImageIO.h"
#include "itkPNGImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkDicomImageIOFactory.h"
#include "itkDICOMSeriesFileNames.h"
#include "itkNumericSeriesFileNames.h"
#include "itkRegularExpressionSeriesFileNames.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(IOBase);
  namespace wrappers
  {
    ITK_WRAP_OBJECT(ImageIOBase);
    ITK_WRAP_OBJECT(PNGImageIO);
    ITK_WRAP_OBJECT(MetaImageIO);
    ITK_WRAP_OBJECT(DicomImageIO);
    ITK_WRAP_OBJECT(GDCMImageIO);
    ITK_WRAP_OBJECT(PNGImageIOFactory);
    ITK_WRAP_OBJECT(MetaImageIOFactory);
    ITK_WRAP_OBJECT(DicomImageIOFactory);
    ITK_WRAP_OBJECT2(RawImageIO, float, 2, itkRawImageIOF2);
    ITK_WRAP_OBJECT2(RawImageIO, float, 3, itkRawImageIOF3);
    ITK_WRAP_OBJECT(DICOMSeriesFileNames);
    ITK_WRAP_OBJECT(NumericSeriesFileNames);
    ITK_WRAP_OBJECT(RegularExpressionSeriesFileNames);
  }
}

#endif
