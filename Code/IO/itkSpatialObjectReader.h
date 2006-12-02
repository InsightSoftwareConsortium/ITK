/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectReader_h
#define __itkSpatialObjectReader_h

#include "itkMetaSceneConverter.h"
#include "itkSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkProcessObject.h"
#include "itkSceneSpatialObject.h"
#include "itkMetaEvent.h"
#include <itkDefaultStaticMeshTraits.h>

namespace itk
{

template <unsigned int NDimensions = 3, 
          typename PixelType = unsigned char,
          typename TMeshTraits = DefaultStaticMeshTraits< PixelType , NDimensions, NDimensions >
         >
class SpatialObjectReader : public Object
{
public:

  /** SmartPointer typedef support */
  typedef SpatialObjectReader Self;
  typedef SmartPointer<Self> Pointer;

  typedef SpatialObject<NDimensions> SpatialObjectType;
  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  typedef GroupSpatialObject<NDimensions> GroupType;
  typedef typename GroupType::Pointer GroupPointer;
  
  typedef SceneSpatialObject<NDimensions> SceneType;
  typedef typename SceneType::Pointer ScenePointer;

   /** Method for creation through the object factory */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(SpatialObjectReader, Object);

  /** Load a scene file. */
  void Update(void);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Get the output */
  ScenePointer GetScene(void) {return m_Scene;}
  GroupPointer GetGroup(void) {return m_Group;}

  /** Set/GetEvent */
//  const MetaEvent* GetEvent() {return m_MetaToSpatialConverter.GetEvent();}
//  void SetEvent(MetaEvent* event) {m_MetaToSpatialConverter.SetEvent(event);} 

protected:
  SpatialObjectReader(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  std::string m_FileName;

  SpatialObjectReader();
  virtual ~SpatialObjectReader();

private:

  ScenePointer m_Scene;
  GroupPointer m_Group;
  MetaSceneConverter<NDimensions,PixelType,TMeshTraits> m_MetaToSpatialConverter;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectReader.txx"
#endif

#endif // __itkSpatialObjectReader_h
