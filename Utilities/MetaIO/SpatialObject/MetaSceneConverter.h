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
//#include "itkCompositeSpatialObject.h"
#include "metaScene.h"
#include "MetaTubeConverter.h"
#include "itkScene.h"

template <unsigned int NDimensions, class PixelType = unsigned char>
class MetaSceneConverter
{

public:

  MetaSceneConverter();
  ~MetaSceneConverter();

  typedef itk::Scene<>  SceneType;

  typedef itk::NDimensionalSpatialObject<> NDimSpatialObjectType;

  typedef typename SceneType::Pointer SpatialObjectPointer;

  typedef std::list<MetaObject*>     ObjectListType;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(NDimSpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaSceneToSpatialObject(MetaScene * scene);

private:

  bool CreateScene(NDimSpatialObjectType* spatialObject,int parentID);

  MetaScene* m_Scene;
  ObjectListType m_ObjectList;

};


#ifndef ITK_MANUAL_INSTANTIATION
#include "MetaSceneConverter.txx"
#endif


#endif
