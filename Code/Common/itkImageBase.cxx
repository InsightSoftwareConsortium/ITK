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

itkImageBase::Pointer itkImageBase::New()
{
  return itkImageBase::Pointer(new itkImageBase);
}

//-------------------------------------------------------------------------
itkImageBase::itkImageBase()
{
  m_Dimensions = 0;
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
  if ( m_Dimensions != 0 )
    {
    delete [] m_Dimensions;
    delete [] m_Spacing;
    delete [] m_Origin;
    }
  this->itkDataObject::SetDimension(-1);
}

//-------------------------------------------------------------------------
void itkImageBase::SetDimension(int dim)
{
  dim = (dim < 0 ? 0 : dim);

  if ( dim != this->GetDimension() )
    {
    this->Initialize();

    this->itkDataObject::SetDimension(dim);
    m_Dimensions = new int [dim];
    m_Spacing = new float [dim];
    m_Origin = new float [dim];
    
    this->Modified();
    }
}

//-------------------------------------------------------------------------
void itkImageBase::SetDimensions(int *dims)
{
  bool modified;

  for (int i=0; i<this->GetDimension(); i++)
    {
    if ( m_Dimensions[i] != dims[i] )
      {
      m_Dimensions[i] = dims[i];
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

  
  
  

