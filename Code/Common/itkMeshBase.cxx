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

itkMeshBase::Pointer itkMeshBase::New()
{
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

