/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectWriter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectWriter_h
#define __itkSpatialObjectWriter_h

#include "itkMetaSceneConverter.h"
#include "itkProcessObject.h"
#include "itkSpatialObject.h"
#include "itkSceneSpatialObject.h"
#include <itkDefaultStaticMeshTraits.h>

namespace itk
{

template <unsigned int NDimensions = 3, 
          typename PixelType = unsigned char,
          typename TMeshTraits = DefaultStaticMeshTraits< PixelType ,
                                                          NDimensions,
                                                          NDimensions >
         >
class SpatialObjectWriter : public Object
{
public:

  /** SmartPointer typedef support */
  typedef SpatialObjectWriter Self;
  typedef SmartPointer<Self> Pointer;

  typedef SpatialObject<NDimensions> SpatialObjectType; 
  typedef typename SpatialObjectType::Pointer SpatialObjectPointer; 
  typedef SceneSpatialObject<NDimensions> SceneType; 

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(SpatialObjectWriter, Object);

  /** Load a tube file. */
  void Update(void);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Set the Input  */
  void SetInput(SpatialObjectType * input){m_SpatialObject=input;}

  void SetInput(SceneType * input){m_Scene=input;}

  itkSetMacro(BinaryPoints,bool);
  itkGetMacro(BinaryPoints,bool);

  void SetTransformPrecision(unsigned int precision);
  unsigned int GetTransformPrecision();

  /** Set/Get if the images should be written in a different file */
  itkSetMacro(WriteImagesInSeparateFile,bool);
  itkGetMacro(WriteImagesInSeparateFile,bool);


protected:

  std::string m_FileName;
  bool        m_BinaryPoints;
  bool        m_WriteImagesInSeparateFile;

  SpatialObjectWriter();
  virtual ~SpatialObjectWriter();

private:

  SpatialObjectPointer           m_SpatialObject;
  SceneType *                    m_Scene;

  MetaSceneConverter<NDimensions,PixelType,TMeshTraits> 
                                 m_MetaToSpatialConverter;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectWriter.txx"
#endif

#endif // __itkSpatialObjectWriter_h
