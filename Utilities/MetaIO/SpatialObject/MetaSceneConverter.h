/*=========================================================================

  Program:   itkUNC
  Module:    MetaSceneConverter.h
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
#ifndef __MetaSceneConverter__h
#define __MetaSceneConverter__h

#include "itkSpatialObject.h"
#include "metaScene.h"
#include "itkScene.h"

template <unsigned int NDimensions, class PixelType = unsigned char>
class MetaSceneConverter
{

public:

  MetaSceneConverter();
  ~MetaSceneConverter();

  itkStaticConstMacro(MaximumDepth, unsigned int, 9999999);

  typedef itk::Scene<NDimensions>  SceneType;
  typedef typename  SceneType::Pointer ScenePointer;

  ScenePointer ReadMeta(const char* name);

  bool WriteMeta(SceneType * scene,const char* fileName,
                 unsigned int depth=MaximumDepth,
                 char * spatialObjectTypeName=NULL);

  void SetUseTransform(bool arg) 
  { m_UseTransform = arg ; }

  bool GetUseTransform() 
  { return m_UseTransform ; }

private:

  typedef itk::SpatialObject<NDimensions> SpatialObjectType;
  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;
  typedef typename SpatialObjectType::TransformType TransformType ;

  typedef std::list<MetaObject*>     MetaObjectListType;

  MetaScene * CreateMetaScene(SceneType * scene,
                              unsigned int depth=MaximumDepth,
                              char * name=NULL);

  ScenePointer CreateSpatialObjectScene( MetaScene * scene );

  void SetTransform(MetaObject* obj, TransformType* transform) ;
  void SetTransform(SpatialObjectType* so, MetaObject* obj) ;

  bool m_UseTransform ;
  float m_Orientation[100] ;
  float m_Position[10] ;

};


#ifndef ITK_MANUAL_INSTANTIATION
#include "MetaSceneConverter.txx"
#endif


#endif
