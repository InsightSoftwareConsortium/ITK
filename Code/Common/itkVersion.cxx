/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersion.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkVersion.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
itkVersion* itkVersion::New()
{
  itkVersion *ret = itkObjectFactory<itkVersion>::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkVersion;
}



