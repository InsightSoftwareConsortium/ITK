/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkMeshBase.h"
#include "itkObjectFactory.h"

itkMeshBase::Pointer itkMeshBase::New()
{
  itkMeshBase *ret = itkObjectFactory<itkMeshBase>::Create();
  if ( ret )
    {
    return ret;
    }
  return itkMeshBase::Pointer(new itkMeshBase);
}

//-------------------------------------------------------------------------
itkMeshBase::itkMeshBase()
{
}

//-------------------------------------------------------------------------
itkMeshBase::~itkMeshBase()
{
  this->Initialize();
}

//-------------------------------------------------------------------------
void itkMeshBase::Initialize()
{
  this->itkDataObject::SetDimension(-1);
}

//-------------------------------------------------------------------------
void itkMeshBase::SetDimension(int dim)
{
}

//-------------------------------------------------------------------------
void itkMeshBase::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkDataObject::PrintSelf(os,indent);
  
}


