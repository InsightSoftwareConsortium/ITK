/*=========================================================================

  Program:   itkUNC
  Module:    itkSpatialObjectReader.h
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
#ifndef __itkSpatialObjectReader_h
#define __itkSpatialObjectReader_h

#include "MetaSceneConverter.h"
#include "itkSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkProcessObject.h"
#include "itkScene.h"

namespace itk
{

template <unsigned int NDimensions = 3, class PixelType = unsigned char>
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
  
  typedef Scene<> SceneType;
  typedef typename SceneType::Pointer ScenePointer;

   /** Method for creation through the object factory */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(Self, Superclass);

  /** Load a scene file. */
  void Update(void);

  /** Set the filename  */
  itkSetStringMacro(FullFileName);

  /** Get the filename */
  itkGetStringMacro(FullFileName);

  void SetUseTransform(bool arg)
  { m_MetaToSpatialConverter.SetUseTransform(arg) ; }

  bool GetUseTransform()
  { return m_MetaToSpatialConverter.GetUseTransform() ; }

  /** Get the output */
  ScenePointer GetScene(void) {return m_Scene;}
  GroupPointer GetGroup(void) {return m_Group;}

protected:
  std::string m_FullFileName;

  SpatialObjectReader();
  virtual ~SpatialObjectReader();

private:

  ScenePointer m_Scene;
  GroupPointer m_Group;
  MetaSceneConverter<NDimensions,PixelType> m_MetaToSpatialConverter;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectReader.txx"
#endif

#endif // __itkSpatialObjectReader_h
