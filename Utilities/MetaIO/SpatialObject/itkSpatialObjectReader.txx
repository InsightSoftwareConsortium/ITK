/*=========================================================================

  Program:   itkUNC
  Module:    itkSpatialObjectReader.txx
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
#ifndef _itkSpatialObjectReader_txx
#define _itkSpatialObjectReader_txx

#include "itkSpatialObject.h"
#include "itkSpatialObjectReader.h"


namespace itk
{

template <unsigned int NDimensions, class PixelType>
SpatialObjectReader<NDimensions,PixelType>
::SpatialObjectReader()
{
  m_FullFileName = "";
  m_Scene = 0;
  m_Group = 0;
}

template <unsigned int NDimensions, class PixelType>
SpatialObjectReader<NDimensions,PixelType>
::~SpatialObjectReader()
{
}


template <unsigned int NDimensions, class PixelType>
void
SpatialObjectReader<NDimensions,PixelType>
::Update()
{ 
  m_Scene = m_MetaToSpatialConverter.ReadMeta(m_FullFileName.c_str());
  m_Group = GroupType::New();

  typename SceneType::ObjectListType::iterator it;
  it = m_Scene->GetObjects().begin();
  typename SceneType::ObjectListType::iterator it_end;
  it_end = m_Scene->GetObjects().end();
  while(it != it_end)
    {
    m_Group->AddSpatialObject(static_cast < SpatialObjectType * > ( *it ));
    it++;
    }
}

} // namespace itk

#endif
