/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshRegion.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkMeshRegion.h"

namespace itk
{

/**
 * Instantiate object.
 */
MeshRegion
::MeshRegion()
{
  m_NumberOfRegions = 0;
  m_Region = 0;
}

/**
 * Destructor for the MeshRegion class.
 */
MeshRegion
::~MeshRegion()
{
}

} // end namespace itk

