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

template <unsigned int NDimensions, class PixelType>
SpatialObjectWriter<NDimensions,PixelType>
::SpatialObjectWriter()
{
  m_FullFileName = "";
  m_FileName = "";
  m_SpatialObject = 0;
  m_Scene = 0;
  m_BinaryPoints = false;
}

template <unsigned int NDimensions, class PixelType>
SpatialObjectWriter<NDimensions,PixelType>
::~SpatialObjectWriter()
{
}

/** Set the precision at which the transform should be written */
template <unsigned int NDimensions, class PixelType>
void
SpatialObjectWriter<NDimensions,PixelType>
::SetTransformPrecision(unsigned int precision)
{
  m_MetaToSpatialConverter.SetTransformPrecision(precision);
}


/** Get the precision at which the transform should be written */
template <unsigned int NDimensions, class PixelType>
unsigned int
SpatialObjectWriter<NDimensions,PixelType>
::GetTransformPrecision()
{
  return m_MetaToSpatialConverter.GetTransformPrecision();
}


template <unsigned int NDimensions, class PixelType>
void
SpatialObjectWriter<NDimensions,PixelType>
::Update()
{ 
  if(m_FileName != "")
    {
    m_FullFileName = m_FileName;
    }

  m_MetaToSpatialConverter.SetBinaryPoints(m_BinaryPoints);

  if(m_Scene != 0)
    {   
    m_MetaToSpatialConverter.WriteMeta(m_Scene,m_FullFileName.c_str());
    m_Scene = 0;
    }
  else
    if(m_SpatialObject.IsNotNull())
      {
      typename SceneType::Pointer tScene = SceneType::New();
      tScene->AddSpatialObject(m_SpatialObject);   
      // Check if IDs are valid because IDs are used to determine parent-child hierarchy
      tScene->FixIdValidity();
     
      m_MetaToSpatialConverter.WriteMeta(tScene,
                                         m_FullFileName.c_str());
      m_SpatialObject = 0;
      }
}


} // namespace itk

#endif
