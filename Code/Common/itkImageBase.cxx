/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageBase.h"

namespace itk
{
  
/**
 *
 */
ImageBase
::ImageBase()
{
}


/**
 *
 */
ImageBase
::~ImageBase()
{
  this->Initialize();
}


/**
 *
 */
void 
ImageBase
::Initialize()
{
  this->DataObject::Initialize();
}


/**
 *
 */
void 
ImageBase
::PrintSelf(std::ostream& os, Indent indent)
{
  DataObject::PrintSelf(os,indent);
}

} // end namespace itk
