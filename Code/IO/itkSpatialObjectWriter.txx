/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectWriter_txx
#define __itkSpatialObjectWriter_txx

#include "itkSpatialObjectWriter.h"


namespace itk
{

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
SpatialObjectWriter<NDimensions,PixelType,TMeshTraits>
::SpatialObjectWriter()
{
  m_FileName = "";
  m_SpatialObject = 0;
  m_Scene = 0;
  m_BinaryPoints = false;
  m_WriteImagesInSeparateFile = false;
}

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
SpatialObjectWriter<NDimensions,PixelType,TMeshTraits>
::~SpatialObjectWriter()
{
}

/** Set the precision at which the transform should be written */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
void
SpatialObjectWriter<NDimensions,PixelType,TMeshTraits>
::SetTransformPrecision(unsigned int precision)
{
  m_MetaToSpatialConverter.SetTransformPrecision(precision);
}


/** Get the precision at which the transform should be written */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
unsigned int
SpatialObjectWriter<NDimensions,PixelType,TMeshTraits>
::GetTransformPrecision()
{
  return m_MetaToSpatialConverter.GetTransformPrecision();
}


template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
void
SpatialObjectWriter<NDimensions,PixelType,TMeshTraits>
::Update()
{ 
  m_MetaToSpatialConverter.SetBinaryPoints(m_BinaryPoints);
  m_MetaToSpatialConverter.SetWriteImagesInSeparateFile(m_WriteImagesInSeparateFile);

  if(m_Scene != 0)
    {   
    m_MetaToSpatialConverter.WriteMeta(m_Scene,m_FileName.c_str());
    m_Scene = 0;
    }
  else
    if(m_SpatialObject.IsNotNull())
      {
      typename SceneType::Pointer tScene = SceneType::New();
      tScene->AddSpatialObject(m_SpatialObject);   
      // Check if IDs are valid because IDs are used to determine
      //    parent-child hierarchy
      tScene->FixIdValidity();
     
      m_MetaToSpatialConverter.WriteMeta(tScene,
                                         m_FileName.c_str());
      m_SpatialObject = 0;
      }
}


} // namespace itk

#endif
