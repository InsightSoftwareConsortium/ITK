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
#include "itkObjectFactory.h"

itkImageBase::Pointer itkImageBase::New()
{
  itkImageBase *ret = itkObjectFactory<itkImageBase>::Create();
  if ( ret )
    {
    return ret;
    }
  return itkImageBase::Pointer(new itkImageBase);
}

//-------------------------------------------------------------------------
itkImageBase::itkImageBase()
{
  m_Size = 0;
  m_Spacing = 0;
  m_Origin = 0;
}

//-------------------------------------------------------------------------
itkImageBase::~itkImageBase()
{
  this->Initialize();
}

//-------------------------------------------------------------------------
void itkImageBase::Initialize()
{
  if ( m_Size != 0 )
    {
    delete [] m_Size;
    delete [] m_Spacing;
    delete [] m_Origin;
    }
  this->itkDataObject::Initialize();
}

//-------------------------------------------------------------------------
void itkImageBase::SetDimension(unsigned int dim)
{
  if ( dim != this->GetDimension() )
    {
    this->Initialize();

    this->itkDataObject::SetDimension(dim);
    m_Size = new unsigned long [dim];
    m_Spacing = new float [dim];
    m_Origin = new float [dim];
    
    this->Modified();
    }
}

//-------------------------------------------------------------------------
void itkImageBase::SetSize(unsigned long *size)
{
  bool modified;

  for (int i=0; i<this->GetDimension(); i++)
    {
    if ( m_Size[i] != size[i] )
      {
      m_Size[i] = size[i];
      modified = true;
      }
    }

  if ( modified )
    {
    this->Modified();
    }
}

//-------------------------------------------------------------------------
void itkImageBase::SetSpacing(float *spacing)
{
  bool modified;

  for (int i=0; i<this->GetDimension(); i++)
    {
    if ( m_Spacing[i] != spacing[i] )
      {
      m_Spacing[i] = spacing[i];
      modified = true;
      }
    }

  if ( modified )
    {
    this->Modified();
    }
}

//-------------------------------------------------------------------------
void itkImageBase::SetOrigin(float *origin)
{
  bool modified;

  for (int i=0; i<this->GetDimension(); i++)
    {
    if ( m_Origin[i] != origin[i] )
      {
      m_Origin[i] = origin[i];
      modified = true;
      }
    }

  if ( modified )
    {
    this->Modified();
    }
}

//-------------------------------------------------------------------------
void itkImageBase::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkDataObject::PrintSelf(os,indent);
  
}

  
  

