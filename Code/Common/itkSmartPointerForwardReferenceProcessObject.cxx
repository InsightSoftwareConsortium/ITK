/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmartPointerForwardReferenceProcessObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSmartPointerForwardReference.h"
#include "itkSmartPointerForwardReference.txx"
#include "itkProcessObject.h"

template class ITKCommon_EXPORT itk::SmartPointerForwardReference< itk::ProcessObject >;
