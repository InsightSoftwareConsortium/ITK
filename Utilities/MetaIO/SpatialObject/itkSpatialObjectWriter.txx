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
  m_Scene = 0;
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
  if(m_Scene != 0)
    {
    m_MetaToSpatialConverter.WriteMeta(m_Scene,m_FullFileName.c_str());
    m_Scene = 0;
    }
  else
    if(m_SpatialObject != 0)
      {
      typename SceneType::Pointer tScene = SceneType::New();
      tScene->AddSpatialObject(m_SpatialObject);
      m_MetaToSpatialConverter.WriteMeta(tScene,
                                         m_FullFileName.c_str());
      m_SpatialObject = 0;
      }
}


} // namespace itk

#endif
