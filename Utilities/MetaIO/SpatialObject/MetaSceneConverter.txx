/*=========================================================================

  Program:   itkUNC
  Module:    MetaSceneConverter.txx
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
#ifndef __MetaSceneConverter__txx
#define __MetaSceneConverter__txx

#include "MetaSceneConverter.h"
#include "MetaEllipseConverter.h"
#include "MetaTubeConverter.h"
#include "MetaImageConverter.h"
#include "MetaBlobConverter.h"
#include "MetaLineConverter.h"
#include "MetaSurfaceConverter.h"
#include <algorithm>
#include "itkTubeSpatialObject.h"

/** Constructor */ 
template <unsigned int NDimensions, class PixelType>                                        
MetaSceneConverter<NDimensions,PixelType>
::MetaSceneConverter()
{
  m_Scene = NULL;
}

/** Destructor */ 
template <unsigned int NDimensions, class PixelType>                                        
MetaSceneConverter<NDimensions,PixelType>
::~MetaSceneConverter()
{
  delete m_Scene;
}

/** Convert a metaScene into a Composite Spatial Object 
 *  Also Managed Composite Spatial Object to keep a hierarchy */
template <unsigned int NDimensions, class PixelType> 
typename MetaSceneConverter<NDimensions,PixelType>::SpatialObjectPointer
MetaSceneConverter<NDimensions,PixelType>
::MetaSceneToSpatialObject(MetaScene * scene)
{

  SceneType::Pointer sceneSpatialObject = SceneType::New();

  MetaScene::ObjectListType list = scene->GetObjectList();
  MetaScene::ObjectListType::iterator it = list.begin();

  std::list<int> ParentIDList; 
  std::list<int>::iterator ParentID_it;

  while(it != list.end())
  {
    unsigned int attached = -1;
    if( ((*it)->ParentID() != -1))
    {
      ParentIDList.push_back((*it)->ParentID());
      ParentID_it = std::find(ParentIDList.begin(), ParentIDList.end(),(*it)->ParentID());
      if( *ParentID_it )
      {
        attached = *ParentID_it;
      }
    }

    /** New object goes here */
    if(!strncmp((*it)->ObjectTypeName(),"Tube",4))
    {
      MetaTubeConverter<NDimensions> tubeConverter;
      typename itk::TubeSpatialObject<NDimensions>::Pointer so = tubeConverter.MetaTubeToTubeSpatialObject((MetaTube*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      { 
        static_cast<itk::SpatialObject<NDimensions>* >(sceneSpatialObject->GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        sceneSpatialObject->AddSpatialObject((NDimSpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Ellipse",5))
    {
      MetaEllipseConverter<NDimensions> ellipseConverter;
      typename itk::EllipseSpatialObject<NDimensions>::Pointer so = ellipseConverter.MetaEllipseToEllipseSpatialObject((MetaEllipse*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(sceneSpatialObject->GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        sceneSpatialObject->AddSpatialObject((NDimSpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Image",5))
    {
      MetaImageConverter<NDimensions,PixelType> imageConverter;
      typename itk::ImageSpatialObject<NDimensions,PixelType>::Pointer so = imageConverter.MetaImageToImageSpatialObject((MetaImage*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(sceneSpatialObject->GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        sceneSpatialObject->AddSpatialObject((NDimSpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Blob",4))
    {
      MetaBlobConverter<NDimensions> blobConverter;
      typename itk::BlobSpatialObject<NDimensions>::Pointer
      so = blobConverter.MetaBlobToBlobSpatialObject((MetaBlob*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(sceneSpatialObject->GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        sceneSpatialObject->AddSpatialObject((NDimSpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Surface",7))
    {
      MetaSurfaceConverter<NDimensions> surfaceConverter;
      typename itk::SurfaceSpatialObject<NDimensions>::Pointer
                                                     so = surfaceConverter.MetaSurfaceToSurfaceSpatialObject((MetaSurface*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(sceneSpatialObject->GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        sceneSpatialObject->AddSpatialObject((NDimSpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Line",4))
    {
      MetaLineConverter<NDimensions> lineConverter;
      typename itk::LineSpatialObject<NDimensions>::Pointer
                                                     so = lineConverter.MetaLineToLineSpatialObject((MetaLine*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(sceneSpatialObject->GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        sceneSpatialObject->AddSpatialObject((NDimSpatialObjectType*)so.GetPointer());
      }
    }
    it++;
  }

  return sceneSpatialObject;
}



/** Read a meta file give the type */
template <unsigned int NDimensions, class PixelType>   
typename MetaSceneConverter<NDimensions,PixelType>::SpatialObjectPointer
MetaSceneConverter<NDimensions,PixelType>
::ReadMeta(const char* name)
{
  MetaScene* scene = new MetaScene;
  scene->Read(name);
  SpatialObjectPointer spatialObject = MetaSceneToSpatialObject(scene);
  delete scene;
  //std::cout << "test" << std::endl;
  return spatialObject;
}


/** Write a meta file give the type */
template <unsigned int NDimensions, class PixelType>    
bool
MetaSceneConverter<NDimensions,PixelType>
::CreateScene(NDimSpatialObjectType* spatialObject,int parentID)
{
  /** SceneSpatialObject */
  if(!strncmp(spatialObject->GetTypeName(),"SceneSpatialObject",17))
  {    
    typedef SceneType::ObjectListType ListType;
    ListType childrenList = dynamic_cast<SceneType*>(spatialObject)->GetObjects();
    ListType::iterator it = childrenList.begin();
    
    while(it != childrenList.end())
    {    
      CreateScene(*it,dynamic_cast<SceneType*>(spatialObject)->GetParentId());
      it++;
    }
    return true;
  }
  
  /** Objects should be added here */

  if(spatialObject->GetDimension() != NDimensions)
  {
    std::cout << "Dimension mismatch : " << NDimensions << "D asked != " << spatialObject->GetDimension() << "D found" << std::endl;
    return false;
  }

  if(!strncmp(spatialObject->GetTypeName(),"TubeSpatialObject",16))
  {
    static MetaTubeConverter<NDimensions> converter;
    MetaTube* tube = converter.TubeSpatialObjectToMetaTube(dynamic_cast<itk::TubeSpatialObject<NDimensions>*>(spatialObject));
    tube->ParentID(parentID);
    tube->Name(spatialObject->GetProperty()->GetName().c_str());
    m_Scene->AddObject(tube);
    return true;
  }

  if(!strncmp(spatialObject->GetTypeName(),"EllipseSpatialObject",20))
  {
    static MetaEllipseConverter<NDimensions> converter;
    MetaEllipse* ellipse = converter.EllipseSpatialObjectToMetaEllipse(dynamic_cast<itk::EllipseSpatialObject<NDimensions>*>(spatialObject));
    ellipse->ParentID(parentID);
    ellipse->Name(spatialObject->GetProperty()->GetName().c_str());
    m_Scene->AddObject(ellipse);
    return true;
  }

  if(!strncmp(spatialObject->GetTypeName(),"ImageSpatialObject",20))
  {
    static MetaImageConverter<NDimensions,PixelType> converter;
    MetaImage* image = converter.ImageSpatialObjectToMetaImage
                                (dynamic_cast<itk::ImageSpatialObject<NDimensions,
                                                                      PixelType>*>(spatialObject)
                                );
    image->ParentID(parentID);
    image->Name(spatialObject->GetProperty()->GetName().c_str());
    m_Scene->AddObject(image);
    return true;
  }

  if(!strncmp(spatialObject->GetTypeName(),"BlobSpatialObject",20))
  {
    static MetaBlobConverter<NDimensions> converter;
    MetaBlob* blob = converter.BlobSpatialObjectToMetaBlob
                                (dynamic_cast<itk::BlobSpatialObject<NDimensions
                                            >*>(spatialObject)
                                );
    blob->ParentID(parentID);
    blob->BinaryData(true);
    blob->Name(spatialObject->GetProperty()->GetName().c_str());
    m_Scene->AddObject(blob);
    return true;
  }

  if(!strncmp(spatialObject->GetTypeName(),"SurfaceSpatialObject",20))
  {
    static MetaSurfaceConverter<NDimensions> converter;
    MetaSurface* surface = converter.SurfaceSpatialObjectToMetaSurface
                                (dynamic_cast<itk::SurfaceSpatialObject<NDimensions
                                            >*>(spatialObject)
                                );
    surface->ParentID(parentID);
    surface->Name(spatialObject->GetProperty()->GetName().c_str());
    m_Scene->AddObject(surface);
    return true;
  }

  if(!strncmp(spatialObject->GetTypeName(),"LineSpatialObject",20))
  {
    static MetaLineConverter<NDimensions> converter;
    MetaLine* line = converter.LineSpatialObjectToMetaLine
                                (dynamic_cast<itk::LineSpatialObject<NDimensions
                                            >*>(spatialObject)
                                );
    line->ParentID(parentID);
    line->Name(spatialObject->GetProperty()->GetName().c_str());
    m_Scene->AddObject(line);
    return true;
  }

  return true;
}


/** Write a meta file give the type */
template <unsigned int NDimensions, class PixelType>   
bool
MetaSceneConverter<NDimensions,PixelType>
::WriteMeta(NDimSpatialObjectType* spatialObject, const char* name)
{
  m_Scene = new MetaScene(spatialObject->GetDimension());
  CreateScene(spatialObject,-1);

  ObjectListType::const_iterator it = m_ObjectList.begin();

  while(it != m_ObjectList.end())
  {
    m_Scene->AddObject(*it);
    it++;
  }

  float* spacing = new float[spatialObject->GetDimension()];
  for(unsigned int i=0;i<spatialObject->GetDimension();i++)
  {
    spacing[i]=1;//spatialObject->GetSpacing()[i];
  }

  m_Scene->ElementSpacing(spacing);
  m_Scene->Write(name);
  delete m_Scene;
  m_Scene = NULL;
  return true;
}

#endif
