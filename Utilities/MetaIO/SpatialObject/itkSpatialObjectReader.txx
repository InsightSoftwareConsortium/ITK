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
  m_FileName = "";
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
  if(m_FullFileName == "")
    {
    m_FullFileName = m_FileName;
    }
  m_Scene = m_MetaToSpatialConverter.ReadMeta(m_FullFileName.c_str());

  if(m_Scene->GetNumberOfObjects(0) == 1)
    {
    typename SceneType::ObjectListType * list = m_Scene->GetObjects(0);
    typename SceneType::ObjectListType::iterator it = list->begin();
    if(!strncmp((*it)->GetTypeName(), "Group", 5))
      {
      m_Group = static_cast<GroupType *>(*it);
      }
    else
      {
      m_Group = GroupType::New();
      m_Group->AddSpatialObject(static_cast<SpatialObjectType *>(*it));
      }
    delete list;
    }
  else
    {
    m_Group = GroupType::New();
    typename SceneType::ObjectListType * list = m_Scene->GetObjects(0);
    typename SceneType::ObjectListType::iterator it = list->begin();
    typename SceneType::ObjectListType::iterator it_end = list->end();
    while(it != it_end)
      {
      m_Group->AddSpatialObject(static_cast<SpatialObjectType *>(*it));
      it++;
      }
    delete list;
    }
}

} // namespace itk

#endif
