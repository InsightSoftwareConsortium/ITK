/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

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

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
SpatialObjectReader<NDimensions,PixelType,TMeshTraits>
::SpatialObjectReader()
{
  m_FileName = "";
  m_Scene = 0;
  m_Group = 0;
}

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
SpatialObjectReader<NDimensions,PixelType,TMeshTraits>
::~SpatialObjectReader()
{
}


template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
void
SpatialObjectReader<NDimensions,PixelType,TMeshTraits>
::Update()
{ 
  m_Scene = m_MetaToSpatialConverter.ReadMeta(m_FileName.c_str());

  if(m_Scene->GetNumberOfObjects(0) == 0)
    {
    itkExceptionMacro("No groups were found in file " << m_FileName );
    }
  
  if(m_Scene->GetNumberOfObjects(0) == 1)
    {
    typename SceneType::ObjectListType * list = m_Scene->GetObjects(0);
    typename SceneType::ObjectListType::iterator it = list->begin();
    if(!strncmp((*it)->GetTypeName(), "Group", 5))
      {
      m_Group = static_cast<GroupType *>((*it).GetPointer());
      }
    else
      {
      m_Group = GroupType::New();
      m_Group->AddSpatialObject(static_cast<SpatialObjectType *>((*it).GetPointer()));
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
