/*=========================================================================

  Program:   itkUNC
  Module:    itkSpatialObjectWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Author:    Julien Jomier (julien@jomier.com)

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectWriter_txx
#define __itkSpatialObjectWriter_txx

#include "itkSpatialObjectWriter.h"


namespace itk
{

template <unsigned int NDimensions, class PixelType>
SpatialObjectWriter<NDimensions,PixelType>
::SpatialObjectWriter()
{
  m_FullFileName = "";
  m_SpatialObject = 0;
}

template <unsigned int NDimensions, class PixelType>
SpatialObjectWriter<NDimensions,PixelType>
::~SpatialObjectWriter()
{
}


template <unsigned int NDimensions, class PixelType>
void
SpatialObjectWriter<NDimensions,PixelType>
::Update()
{ 
  m_MetaToSpatialConverter.WriteMeta(m_SpatialObject,m_FullFileName.c_str());
}


} // namespace itk

#endif
