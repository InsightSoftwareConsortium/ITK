/*=========================================================================

  Program:   itkUNC
  Module:    itkSpatialObjectWriter.h
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
#ifndef __itkSpatialObjectWriter_h
#define __itkSpatialObjectWriter_h

#include "MetaSceneConverter.h"
#include "itkProcessObject.h"
#include "itkSpatialObject.h"
#include "itkScene.h"


namespace itk
{

template <unsigned int NDimensions = 3, class PixelType = unsigned char>
class SpatialObjectWriter : public Object
{
public:

  /** SmartPointer typedef support */
  typedef SpatialObjectWriter Self;
  typedef SmartPointer<Self> Pointer;

  typedef SpatialObject<NDimensions> SpatialObjectType; 
  typedef Scene<NDimensions> SceneType; 

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(Self, Superclass);


  /** Load a tube file. */
  void Update(void);

  /** Set the filename  */
  itkSetStringMacro(FullFileName);

  /** Get the filename */
  itkGetStringMacro(FullFileName);

  void SetSaveTransform(bool arg)
  { m_MetaToSpatialConverter.SetSaveTransform(arg) ; }

  bool GetSaveTransform()
  { return m_MetaToSpatialConverter.GetSaveTransform(arg) ; }

  /** Set the Input  */
  void SetInput(SpatialObjectType * input){m_SpatialObject=input;}

  void SetInput(SceneType * input){m_Scene=input;}

protected:
  std::string m_FullFileName;

  SpatialObjectWriter();
  virtual ~SpatialObjectWriter();

private:

  SpatialObjectType * m_SpatialObject;
  SceneType * m_Scene;

  MetaSceneConverter<NDimensions,PixelType> m_MetaToSpatialConverter;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectWriter.txx"
#endif

#endif // __itkSpatialObjectWriter_h
