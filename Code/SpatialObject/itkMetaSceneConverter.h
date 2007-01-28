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
#ifndef __itkMetaSceneConverter_h
#define __itkMetaSceneConverter_h

#include "itkSpatialObject.h"
#include "metaScene.h"
#include "itkMetaEvent.h"
#include "itkSceneSpatialObject.h"
#include "itkDefaultStaticMeshTraits.h"

namespace itk 
{

template <unsigned int NDimensions, 
          typename PixelType = unsigned char,
          typename TMeshTraits = 
            DefaultStaticMeshTraits< PixelType , NDimensions, NDimensions >
         >
class MetaSceneConverter
{

public:

  MetaSceneConverter();
  ~MetaSceneConverter();

  itkStaticConstMacro(MaximumDepth, unsigned int, 9999999);

  typedef itk::SceneSpatialObject<NDimensions> SceneType;
  typedef typename  SceneType::Pointer         ScenePointer;

  ScenePointer ReadMeta(const char* name);

  bool WriteMeta(SceneType * scene,const char* fileName,
                 unsigned int depth=MaximumDepth,
                 char * spatialObjectTypeName=NULL);


  const MetaEvent* GetEvent() const {return m_Event;}
  void  SetEvent(MetaEvent* event) {m_Event = event;}

  /** Set if the points should be saved in binary/ASCII */
  void SetBinaryPoints(bool binary) {m_BinaryPoints = binary;}

  void SetTransformPrecision(unsigned int precision)
    {
    m_TransformPrecision = precision;
    }
  unsigned int GetTransformPrecision(){return m_TransformPrecision;}

  /** Set if the images should be written in different files */
  void SetWriteImagesInSeparateFile(bool separate) 
    {
    m_WriteImagesInSeparateFile = separate;
    }


private:

  typedef itk::SpatialObject<NDimensions>           SpatialObjectType;
  typedef typename SpatialObjectType::Pointer       SpatialObjectPointer;
  typedef typename SpatialObjectType::TransformType TransformType;

  typedef std::list<MetaObject*>     MetaObjectListType;

  MetaScene * CreateMetaScene(SceneType * scene,
                              unsigned int depth=MaximumDepth,
                              char * name=NULL);

  ScenePointer CreateSpatialObjectScene( MetaScene * scene );

  void SetTransform(MetaObject* obj, TransformType* transform);
  void SetTransform(SpatialObjectType* so, MetaObject* obj);

  double m_Orientation[100];
  double m_Position[10];
  double m_CenterOfRotation[10];

  MetaEvent*   m_Event;
  bool         m_BinaryPoints;
  bool         m_WriteImagesInSeparateFile;
  unsigned int m_TransformPrecision;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMetaSceneConverter.txx"
#endif


#endif
