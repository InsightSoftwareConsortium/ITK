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
#include "MetaGroupConverter.h"
#include "MetaImageConverter.h"
#include "MetaBlobConverter.h"
#include "MetaLineConverter.h"
#include "MetaSurfaceConverter.h"

#include "itkScene.h"
#include "itkEllipseSpatialObject.h"
#include "itkTubeSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkImageSpatialObject.h"
#include "itkBlobSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkSurfaceSpatialObject.h"

#include <algorithm>

/** Constructor */ 
template <unsigned int NDimensions, class PixelType>                                        
MetaSceneConverter<NDimensions,PixelType>
::MetaSceneConverter()
{
}

/** Destructor */ 
template <unsigned int NDimensions, class PixelType>                                        
MetaSceneConverter<NDimensions,PixelType>
::~MetaSceneConverter()
{
}

/** Convert a metaScene into a Composite Spatial Object 
 *  Also Managed Composite Spatial Object to keep a hierarchy */
template <unsigned int NDimensions, class PixelType> 
typename MetaSceneConverter<NDimensions,PixelType>::ScenePointer
MetaSceneConverter<NDimensions,PixelType>
::CreateSpatialObjectScene(MetaScene * mScene)
{

  ScenePointer soScene = SceneType::New();

  MetaScene::ObjectListType list = mScene->GetObjectList();
  MetaScene::ObjectListType::iterator it = list.begin();
  MetaScene::ObjectListType::iterator itEnd = list.end();

  std::list<int> ParentIDList; 
  std::list<int>::iterator ParentID_it;

  while(it != itEnd)
  {
    unsigned int attached = -1;
    if( ((*it)->ParentID() != -1))
    {
      ParentIDList.push_back((*it)->ParentID());
      ParentID_it = std::find(ParentIDList.begin(), ParentIDList.end(),
                              (*it)->ParentID());
      if( *ParentID_it )
      {
        attached = *ParentID_it;
      }
    }

    /** New object goes here */
    if(!strncmp((*it)->ObjectTypeName(),"Tube",4))
    {
      MetaTubeConverter<NDimensions> tubeConverter;
      typename itk::TubeSpatialObject<NDimensions>::Pointer so =
               tubeConverter.MetaTubeToTubeSpatialObject((MetaTube*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      { 
        static_cast<itk::SpatialObject<NDimensions>* >(soScene->
            GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        soScene->AddSpatialObject((SpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Group",5))
    {
      MetaGroupConverter<NDimensions> groupConverter;
      typename itk::GroupSpatialObject<NDimensions>::Pointer so =
               groupConverter.MetaGroupToGroupSpatialObject((MetaGroup*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      { 
        static_cast<itk::SpatialObject<NDimensions>* >(soScene->
            GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        soScene->AddSpatialObject((SpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Ellipse",5))
    {
      MetaEllipseConverter<NDimensions> ellipseConverter;
      typename itk::EllipseSpatialObject<NDimensions>::Pointer so = 
          ellipseConverter.MetaEllipseToEllipseSpatialObject((MetaEllipse*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(soScene->
            GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        soScene->AddSpatialObject( (SpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Image",5))
    {
      MetaImageConverter<NDimensions,PixelType> imageConverter;
      typename itk::ImageSpatialObject<NDimensions,PixelType>::Pointer so =
          imageConverter.MetaImageToImageSpatialObject((MetaImage*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(soScene->
            GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        soScene->AddSpatialObject((SpatialObjectType*) so.GetPointer());
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
        static_cast<itk::SpatialObject<NDimensions>* >(soScene->
            GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        soScene->AddSpatialObject((SpatialObjectType*) so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Surface",7))
    {
      MetaSurfaceConverter<NDimensions> surfaceConverter;
      typename itk::SurfaceSpatialObject<NDimensions>::Pointer so =
          surfaceConverter.MetaSurfaceToSurfaceSpatialObject((MetaSurface*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(soScene->
            GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        soScene->AddSpatialObject( (SpatialObjectType*)so.GetPointer());
      }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Line",4))
    {
      MetaLineConverter<NDimensions> lineConverter;
      typename itk::LineSpatialObject<NDimensions>::Pointer so =
          lineConverter.MetaLineToLineSpatialObject((MetaLine*)*it);
      so->SetReferenceCount(2);
      if(attached != -1)
      {
        static_cast<itk::SpatialObject<NDimensions>* >(soScene->
            GetObjectById(attached))->AddSpatialObject(so.GetPointer());
      }
      else
      {
        soScene->AddSpatialObject( (SpatialObjectType*)so.GetPointer());
      }
    }
    it++;
  }

  return soScene;
}



/** Read a meta file give the type */
template <unsigned int NDimensions, class PixelType>   
typename MetaSceneConverter<NDimensions,PixelType>::ScenePointer
MetaSceneConverter<NDimensions,PixelType>
::ReadMeta(const char* name)
{
  MetaScene* mScene = new MetaScene;
  mScene->Read(name);
  ScenePointer soScene = CreateSpatialObjectScene(mScene);
  delete mScene;
  return soScene;
}


/** Write a meta file give the type */
template <unsigned int NDimensions, class PixelType>    
MetaScene *
MetaSceneConverter<NDimensions,PixelType>
::CreateMetaScene(SceneType * scene, unsigned int depth, char * name)
{
  MetaScene * metaScene = new MetaScene(NDimensions);

  float* spacing = new float[NDimensions];
  for(unsigned int i=0;i<NDimensions;i++)
    {
    spacing[i]=1;
    }
  metaScene->ElementSpacing(spacing);
  delete spacing;

  typedef typename SceneType::ObjectListType ListType;
  ListType * childrenList = scene->GetObjects(depth, name);

  typename ListType::iterator it = childrenList->begin();
  typename ListType::iterator itEnd = childrenList->end();
    
  while(it != itEnd)
    {    
    if(!strncmp((*it)->GetTypeName(),"GroupSpatialObject",17))
      {
      static MetaGroupConverter<NDimensions> converter;
      MetaGroup* group = converter.GroupSpatialObjectToMetaGroup(
          dynamic_cast<itk::GroupSpatialObject<NDimensions>*>((*it)));
      group->ParentID((*it)->GetParentId());
      group->Name((*it)->GetProperty()->GetName().c_str());
      std::cout << "Group id = " << group->ParentID() << std::endl;
      metaScene->AddObject(group);
      }
  
    if(!strncmp((*it)->GetTypeName(),"TubeSpatialObject",16))
      {
      static MetaTubeConverter<NDimensions> converter;
      MetaTube* tube = converter.TubeSpatialObjectToMetaTube(
          dynamic_cast<itk::TubeSpatialObject<NDimensions>*>((*it)));
      tube->ParentID((*it)->GetParentId());
      tube->Name((*it)->GetProperty()->GetName().c_str());
      metaScene->AddObject(tube);
      }
  
    if(!strncmp((*it)->GetTypeName(),"EllipseSpatialObject",20))
      {
      static MetaEllipseConverter<NDimensions> converter;
      MetaEllipse* ellipse = converter.EllipseSpatialObjectToMetaEllipse(
          dynamic_cast<itk::EllipseSpatialObject<NDimensions>*>((*it)));
      ellipse->ParentID((*it)->GetParentId());
      ellipse->Name((*it)->GetProperty()->GetName().c_str());
      metaScene->AddObject(ellipse);
      }
  
    if(!strncmp((*it)->GetTypeName(),"ImageSpatialObject",20))
      {
      static MetaImageConverter<NDimensions,PixelType> converter;
      MetaImage* image = converter.ImageSpatialObjectToMetaImage(
          dynamic_cast<itk::ImageSpatialObject<NDimensions, PixelType>*>(
            (*it)));
      image->ParentID((*it)->GetParentId());
      image->Name((*it)->GetProperty()->GetName().c_str());
      metaScene->AddObject(image);
      }
  
    if(!strncmp((*it)->GetTypeName(),"BlobSpatialObject",20))
      {
      static MetaBlobConverter<NDimensions> converter;
      MetaBlob* blob = converter.BlobSpatialObjectToMetaBlob(
          dynamic_cast<itk::BlobSpatialObject<NDimensions >*>(
            (*it)));
      blob->ParentID((*it)->GetParentId());
      blob->BinaryData(true);
      blob->Name((*it)->GetProperty()->GetName().c_str());
      metaScene->AddObject(blob);
      }
  
    if(!strncmp((*it)->GetTypeName(),"SurfaceSpatialObject",20))
      {
      static MetaSurfaceConverter<NDimensions> converter;
      MetaSurface* surface = converter.SurfaceSpatialObjectToMetaSurface(
          dynamic_cast<itk::SurfaceSpatialObject<NDimensions>*>((*it)));
      surface->ParentID((*it)->GetParentId());
      surface->Name((*it)->GetProperty()->GetName().c_str());
      metaScene->AddObject(surface);
      }
  
    if(!strncmp((*it)->GetTypeName(),"LineSpatialObject",20))
      {
      static MetaLineConverter<NDimensions> converter;
      MetaLine* line = converter.LineSpatialObjectToMetaLine(
          dynamic_cast<itk::LineSpatialObject<NDimensions>*>((*it)));
      line->ParentID((*it)->GetParentId());
      line->Name((*it)->GetProperty()->GetName().c_str());
      metaScene->AddObject(line);
      }
  
    it++;
    }
 
  delete childrenList;

  return metaScene;
}


/** Write a meta file give the type */
template <unsigned int NDimensions, class PixelType>   
bool
MetaSceneConverter<NDimensions,PixelType>
::WriteMeta(SceneType * scene, const char* fileName,
            unsigned int depth, char * soName)
{
  MetaScene * metaScene = CreateMetaScene(scene, depth, soName);

  metaScene->Write(fileName);

  delete metaScene;

  return true;
}

#endif
