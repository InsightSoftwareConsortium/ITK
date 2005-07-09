/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaSceneConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaSceneConverter__txx
#define __MetaSceneConverter__txx

#include "itkMetaSceneConverter.h"
#include "itkMetaEllipseConverter.h"
#include "itkMetaTubeConverter.h"
#include "itkMetaDTITubeConverter.h"
#include "itkMetaVesselTubeConverter.h"
#include "itkMetaGroupConverter.h"
#include "itkMetaImageConverter.h"
#include "itkMetaBlobConverter.h"
#include "itkMetaMeshConverter.h"
#include "itkMetaLandmarkConverter.h"
#include "itkMetaLineConverter.h"
#include "itkMetaSurfaceConverter.h"
#include "itkMetaLandmarkConverter.h"
#include "itkMetaArrowConverter.h"

#include "itkSceneSpatialObject.h"
#include "itkEllipseSpatialObject.h"
#include "itkTubeSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkImageSpatialObject.h"
#include "itkBlobSpatialObject.h"
#include "itkLandmarkSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkSurfaceSpatialObject.h"
#include "itkLandmarkSpatialObject.h"
#include "itkMeshSpatialObject.h"
#include "itkArrowSpatialObject.h"

#include <algorithm>


namespace itk  
{

/** Constructor */ 
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>                                        
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::MetaSceneConverter()
{
  // default behaviour of scene converter is not to save transform 
  // with each spatial object.
  m_Event = NULL;
  m_BinaryPoints = false;
  m_TransformPrecision = 6;
  m_WriteImagesInSeparateFile = false;
}

/** Destructor */ 
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>                                          
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::~MetaSceneConverter()
{
}

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>   
void
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::SetTransform(MetaObject* obj, TransformType* transform)
{
  typename SpatialObjectType::TransformType::InputPointType center = 
                                     transform->GetCenter();
  typename SpatialObjectType::TransformType::MatrixType matrix =
                                     transform->GetMatrix();
  typename SpatialObjectType::TransformType::OffsetType offset =
                                     transform->GetOffset();

  unsigned int p = 0;
  for ( unsigned int row = 0 ; row<NDimensions; row++)
    {
    for( unsigned int col = 0; col<NDimensions; col++)
      {
      m_Orientation[p++] = matrix[row][col];
      }
    }

  for ( unsigned int i = 0; i<NDimensions; i++)
    {
    m_Position[i] = offset[i] ;
    m_CenterOfRotation[i] = center[i] ;
    }

  obj->CenterOfRotation(m_CenterOfRotation);
  obj->TransformMatrix(m_Orientation) ;
  obj->Offset(m_Position) ;
  obj->SetDoublePrecision(m_TransformPrecision);
}

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>   
void
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::SetTransform(SpatialObjectType* so, MetaObject* meta)
{
  typename SpatialObjectType::TransformType::Pointer transform = 
    SpatialObjectType::TransformType::New() ;

  typename SpatialObjectType::TransformType::InputPointType center;
  typename SpatialObjectType::TransformType::MatrixType matrix;
  typename SpatialObjectType::TransformType::OffsetType offset;

  unsigned int p = 0;
  for ( unsigned int row = 0 ; row<NDimensions; row++)
    {
    for( unsigned int col = 0; col<NDimensions; col++)
      {
      matrix[row][col] = (meta->Orientation())[p++];
      }
    }

  for ( unsigned int i = 0; i<NDimensions; i++)
    {
    offset[i] = (meta->Position())[i];
    center[i] = (meta->CenterOfRotation())[i];
    }

  so->GetObjectToParentTransform()->SetCenter(center);
  so->GetObjectToParentTransform()->SetMatrix(matrix);
  so->GetObjectToParentTransform()->SetOffset(offset);
}

/** Convert a metaScene into a Composite Spatial Object 
 *  Also Managed Composite Spatial Object to keep a hierarchy */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>   
typename MetaSceneConverter<NDimensions,PixelType,TMeshTraits>::ScenePointer
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::CreateSpatialObjectScene(MetaScene * mScene)
{
  ScenePointer soScene = SceneType::New();

  MetaScene::ObjectListType * list = mScene->GetObjectList();
  MetaScene::ObjectListType::iterator it = list->begin();
  MetaScene::ObjectListType::iterator itEnd = list->end();

  while(it != itEnd)
  {
    /** New object goes here */
    if(!strncmp((*it)->ObjectTypeName(),"Tube",4))
    {
      // If there is the subtype is a vessel
      if(!strncmp((*it)->ObjectSubTypeName(),"Vessel",6))
        {
        MetaVesselTubeConverter<NDimensions> tubeConverter;
        typename itk::VesselTubeSpatialObject<NDimensions>::Pointer so =
                 tubeConverter.MetaVesselTubeToVesselTubeSpatialObject((MetaVesselTube*)*it);
        this->SetTransform(so, *it) ;
        soScene->AddSpatialObject(so);
        }
      else if(!strncmp((*it)->ObjectSubTypeName(),"DTI",3))
        {
        MetaDTITubeConverter<NDimensions> tubeConverter;
        typename itk::DTITubeSpatialObject<NDimensions>::Pointer so =
                 tubeConverter.MetaDTITubeToDTITubeSpatialObject((MetaDTITube*)*it);
        this->SetTransform(so, *it) ;
        soScene->AddSpatialObject(so);
        }
      else
        {
        MetaTubeConverter<NDimensions> tubeConverter;
        typename itk::TubeSpatialObject<NDimensions>::Pointer so =
                 tubeConverter.MetaTubeToTubeSpatialObject((MetaTube*)*it);
        this->SetTransform(so, *it) ;
        soScene->AddSpatialObject(so);
        }
    }

    if(!strncmp((*it)->ObjectTypeName(),"Group",5) ||
       !strncmp((*it)->ObjectTypeName(),"AffineTransform",15))
    {
      MetaGroupConverter<NDimensions> groupConverter;
      typename itk::GroupSpatialObject<NDimensions>::Pointer so =
               groupConverter.MetaGroupToGroupSpatialObject((MetaGroup*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject(so);
    }

    if(!strncmp((*it)->ObjectTypeName(),"Ellipse",7))
    {
      MetaEllipseConverter<NDimensions> ellipseConverter;
      typename itk::EllipseSpatialObject<NDimensions>::Pointer so = 
          ellipseConverter.MetaEllipseToEllipseSpatialObject((MetaEllipse*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject( so);
    }

    if(!strncmp((*it)->ObjectTypeName(),"Arrow",5))
    {
      MetaArrowConverter<NDimensions> arrowConverter;
      typename itk::ArrowSpatialObject<NDimensions>::Pointer so = 
          arrowConverter.MetaArrowToArrowSpatialObject((MetaArrow*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject( so);
    }

    if(!strncmp((*it)->ObjectTypeName(),"Image",5))
    {
      MetaImageConverter<NDimensions,PixelType> imageConverter;
      typename itk::ImageSpatialObject<NDimensions,PixelType>::Pointer so =
          imageConverter.MetaImageToImageSpatialObject((MetaImage*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject(so);
    }

    if(!strncmp((*it)->ObjectTypeName(),"Blob",4))
    {
      MetaBlobConverter<NDimensions> blobConverter;
      typename itk::BlobSpatialObject<NDimensions>::Pointer
      so = blobConverter.MetaBlobToBlobSpatialObject((MetaBlob*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject((SpatialObjectType*) so.GetPointer());
    }

    if(!strncmp((*it)->ObjectTypeName(),"Landmark",8))
    {
      MetaLandmarkConverter<NDimensions> landmarkConverter;
      typename itk::LandmarkSpatialObject<NDimensions>::Pointer
      so = landmarkConverter.MetaLandmarkToLandmarkSpatialObject(
                                              (MetaLandmark*)*it);
      soScene->AddSpatialObject((SpatialObjectType*) so.GetPointer());
    }

    if(!strncmp((*it)->ObjectTypeName(),"Surface",7))
    {
      MetaSurfaceConverter<NDimensions> surfaceConverter;
      typename itk::SurfaceSpatialObject<NDimensions>::Pointer so =
          surfaceConverter.MetaSurfaceToSurfaceSpatialObject((MetaSurface*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject( so);
    }

    if(!strncmp((*it)->ObjectTypeName(),"Line",4))
    {
      MetaLineConverter<NDimensions> lineConverter;
      typename itk::LineSpatialObject<NDimensions>::Pointer so =
          lineConverter.MetaLineToLineSpatialObject((MetaLine*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject( so);
    }
  
    if(!strncmp((*it)->ObjectTypeName(),"Mesh",4))
    {
      typedef itk::Mesh<PixelType,NDimensions,TMeshTraits> MeshType;
      MetaMeshConverter<NDimensions,PixelType,TMeshTraits> meshConverter;
      typename itk::MeshSpatialObject<MeshType>::Pointer so =
          meshConverter.MetaMeshToMeshSpatialObject((MetaMesh*)*it);
      this->SetTransform(so, *it) ;
      soScene->AddSpatialObject( so);
    }

    it++;
  }

  soScene->FixHierarchy();

  return soScene;
}



/** Read a meta file give the type */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>     
typename MetaSceneConverter<NDimensions,PixelType,TMeshTraits>::ScenePointer
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::ReadMeta(const char* name)
{
  MetaScene* mScene = new MetaScene;
  if(m_Event)
    {
    mScene->SetEvent(m_Event);
    }
  mScene->Read(name);
  ScenePointer soScene = CreateSpatialObjectScene(mScene);
  delete mScene;
  return soScene;
}


/** Write a meta file give the type */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>      
MetaScene *
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::CreateMetaScene(SceneType * scene, unsigned int depth, char * name)
{
  MetaScene * metaScene = new MetaScene(NDimensions);

  metaScene->BinaryData(m_BinaryPoints);

  float* spacing = new float[NDimensions];
  for(unsigned int i=0;i<NDimensions;i++)
    {
    spacing[i]=1;
    }
  metaScene->ElementSpacing(spacing);
  delete []spacing;

  typedef typename SceneType::ObjectListType ListType;

  ListType * childrenList = scene->GetObjects(depth, name);
  typename ListType::iterator it = childrenList->begin();
  typename ListType::iterator itEnd = childrenList->end();
    
  while(it != itEnd)
    {    
    if(!strncmp((*it)->GetTypeName(),"GroupSpatialObject",18) ||
       !strncmp((*it)->GetTypeName(),"AffineTransformSpatialObject",28))
      {
      MetaGroupConverter<NDimensions> converter;
      MetaGroup* group = converter.GroupSpatialObjectToMetaGroup(
          dynamic_cast<itk::GroupSpatialObject<NDimensions>*>(
               (*it).GetPointer()));
      if((*it)->GetParent())
        {
        group->ParentID((*it)->GetParent()->GetId());
        }
      group->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(group, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(group);
      }
    if(!strncmp((*it)->GetTypeName(),"TubeSpatialObject",17))
      {
      MetaTubeConverter<NDimensions> converter;
      MetaTube* tube = converter.TubeSpatialObjectToMetaTube(
          dynamic_cast<itk::TubeSpatialObject<NDimensions>*>(
               (*it).GetPointer()));
      if((*it)->GetParent())
        {
        tube->ParentID((*it)->GetParent()->GetId());
        }
      tube->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(tube, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(tube);
      }
   
    if(!strncmp((*it)->GetTypeName(),"VesselTubeSpatialObject",23))
      {
      MetaVesselTubeConverter<NDimensions> converter;
      MetaVesselTube* tube = converter.VesselTubeSpatialObjectToMetaVesselTube(
          dynamic_cast<itk::VesselTubeSpatialObject<NDimensions>*>((*it).GetPointer()));
      if((*it)->GetParent())
        {
        tube->ParentID((*it)->GetParent()->GetId());
        }
      tube->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(tube, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(tube);
      }

    if(!strncmp((*it)->GetTypeName(),"DTITubeSpatialObject",20))
      {
      MetaDTITubeConverter<NDimensions> converter;
      MetaDTITube* tube = converter.DTITubeSpatialObjectToMetaDTITube(
          dynamic_cast<itk::DTITubeSpatialObject<NDimensions>*>((*it).GetPointer()));
      if((*it)->GetParent())
        {
        tube->ParentID((*it)->GetParent()->GetId());
        }
      tube->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(tube, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(tube);
      }

    if(!strncmp((*it)->GetTypeName(),"EllipseSpatialObject",20))
      {
      MetaEllipseConverter<NDimensions> converter;
      MetaEllipse* ellipse = converter.EllipseSpatialObjectToMetaEllipse(
          dynamic_cast<itk::EllipseSpatialObject<NDimensions>*>(
               (*it).GetPointer()));
      
      if((*it)->GetParent())
        {
        ellipse->ParentID((*it)->GetParent()->GetId());
        }
      ellipse->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(ellipse, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(ellipse);
      }

    
    if(!strncmp((*it)->GetTypeName(),"ArrowSpatialObject",18))
      {
      MetaArrowConverter<NDimensions> converter;
      MetaArrow* arrow = converter.ArrowSpatialObjectToMetaArrow(
          dynamic_cast<itk::ArrowSpatialObject<NDimensions>*>(
               (*it).GetPointer()));
      
      if((*it)->GetParent())
        {
        arrow->ParentID((*it)->GetParent()->GetId());
        }
      arrow->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(arrow, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(arrow);
      }
    
    if(!strncmp((*it)->GetTypeName(),"ImageSpatialObject",17))
      {
      MetaImageConverter<NDimensions,PixelType> converter;
      MetaImage* image = converter.ImageSpatialObjectToMetaImage(
          dynamic_cast<itk::ImageSpatialObject<NDimensions, PixelType>*>(
            (*it).GetPointer()));
      if((*it)->GetParent())
        {
        image->ParentID((*it)->GetParent()->GetId());
        }

      if(m_WriteImagesInSeparateFile)
        {
        if((*it)->GetProperty()->GetName().size() == 0)
          {
          std::cout << "Error: you should set the image name when using WriteImagesInSeparateFile." << std::endl;
          std::cout << "The image will be written locally." << std::endl;
          }
        else
          {
          std::string filename = (*it)->GetProperty()->GetName();
          filename += ".raw";
          image->ElementDataFileName(filename.c_str());
          }
        }

      image->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(image, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(image);
      }
  
    if(!strncmp((*it)->GetTypeName(),"BlobSpatialObject",17))
      {
      MetaBlobConverter<NDimensions> converter;
      MetaBlob* blob = converter.BlobSpatialObjectToMetaBlob(
          dynamic_cast<itk::BlobSpatialObject<NDimensions >*>(
            (*it).GetPointer()));
      if((*it)->GetParent())
        {
        blob->ParentID((*it)->GetParent()->GetId());
        }
      blob->BinaryData(true);
      blob->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(blob, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(blob);
      }
  
    if(!strncmp((*it)->GetTypeName(),"LandmarkSpatialObject",20))
      {
      MetaLandmarkConverter<NDimensions> converter;
      MetaLandmark* landmark = converter.LandmarkSpatialObjectToMetaLandmark(
          dynamic_cast<itk::LandmarkSpatialObject<NDimensions >*>(
            (*it).GetPointer()));
      if((*it)->GetParent())
        {
        landmark->ParentID((*it)->GetParent()->GetId());
      }
      landmark->BinaryData(true);
      landmark->Name((*it)->GetProperty()->GetName().c_str());
      metaScene->AddObject(landmark);
      }
  
    if(!strncmp((*it)->GetTypeName(),"SurfaceSpatialObject",20))
      {
      MetaSurfaceConverter<NDimensions> converter;
      MetaSurface* surface = converter.SurfaceSpatialObjectToMetaSurface(
          dynamic_cast<itk::SurfaceSpatialObject<NDimensions>*>(
               (*it).GetPointer()));
      if((*it)->GetParent())
        {
        surface->ParentID((*it)->GetParent()->GetId());
        }
      surface->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(surface, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(surface);
      }
  
    if(!strncmp((*it)->GetTypeName(),"LineSpatialObject",17))
      {
      MetaLineConverter<NDimensions> converter;
      MetaLine* line = converter.LineSpatialObjectToMetaLine(
          dynamic_cast<itk::LineSpatialObject<NDimensions>*>(
               (*it).GetPointer()));
      if((*it)->GetParent())
        {
        line->ParentID((*it)->GetParent()->GetId());
        }
      line->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(line, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(line);
      }

    if(!strncmp((*it)->GetTypeName(),"MeshSpatialObject",17))
      {
      typedef itk::Mesh<PixelType,NDimensions,TMeshTraits> MeshType;
      MetaMeshConverter<NDimensions,PixelType,TMeshTraits> converter;
      MetaMesh* mesh = converter.MeshSpatialObjectToMetaMesh(
          dynamic_cast<itk::MeshSpatialObject<MeshType>*>((*it).GetPointer()));
      if((*it)->GetParent())
        {
        mesh->ParentID((*it)->GetParent()->GetId());
        }
      mesh->Name((*it)->GetProperty()->GetName().c_str());
      this->SetTransform(mesh, (*it)->GetObjectToParentTransform()) ;
      metaScene->AddObject(mesh);
      }

    it++;
    }
 
  delete childrenList;

  return metaScene;
}


/** Write a meta file give the type */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>     
bool
MetaSceneConverter<NDimensions,PixelType,TMeshTraits>
::WriteMeta(SceneType * scene, const char* fileName,
            unsigned int depth, char * soName)
{
  MetaScene * metaScene = this->CreateMetaScene(scene, depth, soName);

  metaScene->Write(fileName);

  delete metaScene;

  return true;
}

} // end namespace itk 


#endif
