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
  m_Size = 0;
  m_Spacing = 0;
  m_Origin = 0;
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
  if ( m_Size != 0 )
    {
    delete [] m_Size;
    delete [] m_Spacing;
    delete [] m_Origin;
    }
  this->DataObject::Initialize();
}


/**
 *
 */
void 
ImageBase
::SetDimension(unsigned int dim)
{
  if ( dim != this->GetDimension() )
    {
    this->Initialize();

    this->DataObject::SetDimension(dim);
    m_Size = new unsigned long [dim];
    m_Spacing = new float [dim];
    m_Origin = new float [dim];
    
    this->Modified();
    }
}


/**
 *
 */
void 
ImageBase
::SetSize(unsigned long *size)
{
  bool modified = false;

  for (unsigned int i=0; i<this->GetDimension(); i++)
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
/**
 *
 */
void 
ImageBase
::SetSpacing(float *spacing)
{
  bool modified = false;

  for (unsigned int i=0; i<this->GetDimension(); i++)
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


/**
 *
 */
void 
ImageBase
::SetOrigin(float *origin)
{
  bool modified = false;

  for (unsigned int i=0; i<this->GetDimension(); i++)
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


/**
 *
 */
void 
ImageBase
::PrintSelf(std::ostream& os, Indent indent)
{
  DataObject::PrintSelf(os,indent);
}

} // namespace itk
