/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaSceneConverter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaSceneConverter__h
#define __MetaSceneConverter__h

#include "itkSpatialObject.h"
#include "metaScene.h"
#include "itkMetaEvent.h"
#include "itkSceneSpatialObject.h"

namespace itk 
{

template <unsigned int NDimensions, class PixelType = unsigned char>
class MetaSceneConverter
{

public:

  MetaSceneConverter();
  ~MetaSceneConverter();

  itkStaticConstMacro(MaximumDepth, unsigned int, 9999999);

  typedef itk::SceneSpatialObject<NDimensions>  SceneType;
  typedef typename  SceneType::Pointer ScenePointer;

  ScenePointer ReadMeta(const char* name);

  bool WriteMeta(SceneType * scene,const char* fileName,
                 unsigned int depth=MaximumDepth,
                 char * spatialObjectTypeName=NULL);


  const MetaEvent* GetEvent() const {return m_Event;}
  void  SetEvent(MetaEvent* event) {m_Event = event;}

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

  float m_Orientation[100] ;
  float m_Position[10] ;
  float m_CenterOfRotation[10] ;

  MetaEvent* m_Event;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMetaSceneConverter.txx"
#endif


#endif
