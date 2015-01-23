/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMetaSceneConverter_hxx
#define itkMetaSceneConverter_hxx

#include "itkMetaSceneConverter.h"
#include "itkMetaEllipseConverter.h"
#include "itkMetaTubeConverter.h"
#include "itkMetaDTITubeConverter.h"
#include "itkMetaVesselTubeConverter.h"
#include "itkMetaGroupConverter.h"
#include "itkMetaImageConverter.h"
#include "itkMetaImageMaskConverter.h"
#include "itkMetaBlobConverter.h"
#include "itkMetaGaussianConverter.h"
#include "itkMetaMeshConverter.h"
#include "itkMetaLandmarkConverter.h"
#include "itkMetaLineConverter.h"
#include "itkMetaSurfaceConverter.h"
#include "itkMetaLandmarkConverter.h"
#include "itkMetaArrowConverter.h"
#include "itkMetaContourConverter.h"

#include <algorithm>

namespace itk
{
/** Constructor */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::MetaSceneConverter()
{
  // default behaviour of scene converter is not to save transform
  // with each spatial object.
  m_Event = ITK_NULLPTR;
  m_BinaryPoints = false;
  m_TransformPrecision = 6;
  m_WriteImagesInSeparateFile = false;
}

/** Destructor */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::~MetaSceneConverter()
{}

template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::SetTransform(MetaObject *obj, TransformType *transform)
{
  typename SpatialObjectType::TransformType::InputPointType center =
    transform->GetCenter();
  typename SpatialObjectType::TransformType::MatrixType matrix =
    transform->GetMatrix();
  typename SpatialObjectType::TransformType::OffsetType offset =
    transform->GetOffset();

  unsigned int p = 0;
  for ( unsigned int row = 0; row < NDimensions; row++ )
    {
    for ( unsigned int col = 0; col < NDimensions; col++ )
      {
      m_Orientation[p++] = matrix[row][col];
      }
    }

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Position[i] = offset[i];
    m_CenterOfRotation[i] = center[i];
    }

  obj->CenterOfRotation(m_CenterOfRotation);
  obj->TransformMatrix(m_Orientation);
  obj->Offset(m_Position);
  obj->SetDoublePrecision(m_TransformPrecision);
}

template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::SetTransform(SpatialObjectType *so, MetaObject *meta)
{
  typename SpatialObjectType::TransformType::InputPointType center;
  typename SpatialObjectType::TransformType::MatrixType matrix;
  typename SpatialObjectType::TransformType::OffsetType offset;

  unsigned int p = 0;
  for ( unsigned int row = 0; row < NDimensions; row++ )
    {
    for ( unsigned int col = 0; col < NDimensions; col++ )
      {
      matrix[row][col] = ( meta->Orientation() )[p++];
      }
    }

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    offset[i] = ( meta->Position() )[i];
    center[i] = ( meta->CenterOfRotation() )[i];
    }

  so->GetObjectToParentTransform()->SetCenter(center);
  so->GetObjectToParentTransform()->SetMatrix(matrix);
  so->GetObjectToParentTransform()->SetOffset(offset);
}

/** Convert a metaScene into a Composite Spatial Object
 *  Also Managed Composite Spatial Object to keep a hierarchy */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
typename MetaSceneConverter< NDimensions, PixelType, TMeshTraits >::ScenePointer
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::CreateSpatialObjectScene(MetaScene *mScene)
{
  ScenePointer soScene = SceneType::New();

  MetaScene::ObjectListType *         list = mScene->GetObjectList();
  MetaScene::ObjectListType::iterator it = list->begin();
  MetaScene::ObjectListType::iterator itEnd = list->end();

  SpatialObjectPointer currentSO;

  while ( it != itEnd )
    {
    const std::string objectTypeName(( *it )->ObjectTypeName());
    const std::string objectSubTypeName(( *it )->ObjectSubTypeName());
    /** New object goes here */
    if ( objectTypeName == "Tube" )
      {
      // If there is the subtype is a vessel
      if ( objectSubTypeName == "Vessel" )
        {
        currentSO = this->MetaObjectToSpatialObject<MetaVesselTubeConverter< NDimensions > > ( *it );
        }
      else if ( objectSubTypeName == "DTI" )
        {
        currentSO = this->MetaObjectToSpatialObject< MetaDTITubeConverter<NDimensions> > ( *it );
        }
      else
        {
        currentSO = this->MetaObjectToSpatialObject< MetaTubeConverter<NDimensions> > ( *it );
        }
      }
    else if ( objectTypeName == "Group" || objectTypeName == "AffineTransform" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaGroupConverter< NDimensions > > ( *it );
      }
    else if ( objectTypeName == "Ellipse" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaEllipseConverter< NDimensions > > ( *it );
      }
    else if ( objectTypeName == "Arrow" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaArrowConverter< NDimensions > > ( *it );
      }
    else if ( objectTypeName == "Image" )
      {
      // If there is the subtype is a mask
      if ( objectSubTypeName == "Mask" )
        {
        currentSO = this->MetaObjectToSpatialObject< MetaImageMaskConverter< NDimensions > > ( *it );
        }
      else
        {
        currentSO = this->MetaObjectToSpatialObject< MetaImageConverter<NDimensions,PixelType> > ( *it );
        }
      }
    else if ( objectTypeName == "Blob" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaBlobConverter<NDimensions> > ( *it );
      }
    else if ( objectTypeName == "Gaussian" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaGaussianConverter<NDimensions> > ( *it );
      }
    else if ( objectTypeName == "Landmark" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaLandmarkConverter<NDimensions> > ( *it );
      }
    else if ( objectTypeName == "Surface" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaSurfaceConverter< NDimensions > > ( *it );
      }
    else if ( objectTypeName == "Line" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaLineConverter< NDimensions > > ( *it );
      }
    else if ( objectTypeName == "Mesh" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaMeshConverter< NDimensions, PixelType, TMeshTraits > > ( *it );
      }
    else if ( objectTypeName == "Contour" )
      {
      currentSO = this->MetaObjectToSpatialObject< MetaContourConverter< NDimensions > > ( *it );
      }
    else
      {
      typename ConverterMapType::iterator converterIt = this->m_ConverterMap.find(objectTypeName);

      if(converterIt == this->m_ConverterMap.end())
        {
        itkGenericExceptionMacro(<< "Unable to find MetaObject -> SpatialObject converter for "
                          << objectTypeName);
        }
      currentSO = converterIt->second->MetaObjectToSpatialObject( *it );
      }
    this->SetTransform(currentSO, *it);
    soScene->AddSpatialObject(currentSO);
    it++;
    }

  soScene->FixHierarchy();

  return soScene;
}

/** Read a meta file give the type */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
typename MetaSceneConverter< NDimensions, PixelType, TMeshTraits >::ScenePointer
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::ReadMeta(const char *name)
{
  MetaScene *mScene = new MetaScene;

  if ( m_Event )
    {
    mScene->SetEvent(m_Event);
    }
  mScene->Read(name);
  ScenePointer soScene = CreateSpatialObjectScene(mScene);
  delete mScene;
  return soScene;
}

/** Write a meta file give the type */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
MetaScene *
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::CreateMetaScene(SceneType *scene, unsigned int depth, char *name)
{
  MetaScene *metaScene = new MetaScene(NDimensions);

  metaScene->BinaryData(m_BinaryPoints);

  float *spacing = new float[NDimensions];
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    spacing[i] = 1;
    }
  metaScene->ElementSpacing(spacing);
  delete[] spacing;

  typedef typename SceneType::ObjectListType ListType;

  ListType *childrenList = scene->GetObjects(depth, name);
  typename ListType::iterator it = childrenList->begin();
  typename ListType::iterator itEnd = childrenList->end();

  MetaObject *currentMeta;

  while ( it != itEnd )
    {
    std::string spatialObjectTypeName( ( *it )->GetTypeName() );
    if ( spatialObjectTypeName == "GroupSpatialObject"
         || spatialObjectTypeName == "AffineTransformSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaGroupConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "TubeSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaTubeConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "VesselTubeSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaVesselTubeConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "DTITubeSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaDTITubeConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "EllipseSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaEllipseConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "ArrowSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaArrowConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "ImageSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaImageConverter< NDimensions, PixelType > > ( *it );
      }
    else if ( spatialObjectTypeName == "ImageMaskSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaImageMaskConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "BlobSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaBlobConverter<NDimensions> > ( *it );
      }
    else if ( spatialObjectTypeName == "GaussianSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaGaussianConverter<NDimensions> > ( *it );
      }
    else if ( spatialObjectTypeName == "LandmarkSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaLandmarkConverter<NDimensions> > ( *it );
      }
    else if ( spatialObjectTypeName == "ContourSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaContourConverter<NDimensions> > ( *it );
      }
    else if ( spatialObjectTypeName == "SurfaceSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaSurfaceConverter< NDimensions > > ( *it );
      }
    else if ( spatialObjectTypeName == "LineSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaLineConverter<NDimensions> > ( *it );
      }
    else if ( spatialObjectTypeName == "MeshSpatialObject" )
      {
      currentMeta = this->SpatialObjectToMetaObject< MetaMeshConverter< NDimensions, PixelType, TMeshTraits > > ( *it );
      }
    else
      {
      typename ConverterMapType::iterator converterIt = this->m_ConverterMap.find(spatialObjectTypeName);
      if(converterIt == this->m_ConverterMap.end())
        {
        itkGenericExceptionMacro(<< "Unable to find MetaObject -> SpatialObject converter for "
                          << spatialObjectTypeName);
        }
      currentMeta = converterIt->second->SpatialObjectToMetaObject( *it );
      }
    if ( ( *it )->GetParent() )
      {
      currentMeta->ParentID( ( *it )->GetParent()->GetId() );
      }
    currentMeta->Name( ( *it )->GetProperty()->GetName().c_str() );
    this->SetTransform( currentMeta, ( *it )->GetObjectToParentTransform() );
    metaScene->AddObject(currentMeta);
    it++;
    }

  delete childrenList;

  return metaScene;
}

/** Write a meta file give the type */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
bool
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::WriteMeta(SceneType *scene, const char *fileName,
            unsigned int depth, char *soName)
{
  MetaScene *metaScene = this->CreateMetaScene(scene, depth, soName);

  metaScene->Write(fileName);

  delete metaScene;

  return true;
}

template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
void
MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
::RegisterMetaConverter(const char *metaTypeName,
                      const char *spatialObjectTypeName,
                      MetaConverterBaseType *converter)
{
  std::string metaType(metaTypeName);
  std::string spatialObjectType(spatialObjectTypeName);
  this->m_ConverterMap[metaType] = converter;
  this->m_ConverterMap[spatialObjectType] = converter;
}

} // end namespace itk

#endif
