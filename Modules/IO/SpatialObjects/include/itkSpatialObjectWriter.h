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
#ifndef __itkSpatialObjectWriter_h
#define __itkSpatialObjectWriter_h
#include "ITKIOSpatialObjectsExport.h"

#include "itkMetaSceneConverter.h"
#include "itkMetaConverterBase.h"
#include "itkSpatialObject.h"

namespace itk
{
/** \class SpatialObjectWriter
 *
 * \brief TODO
 * \ingroup ITKIOSpatialObjects
 */
template< unsigned int NDimensions = 3,
          typename PixelType = unsigned char,
          typename TMeshTraits = DefaultStaticMeshTraits< PixelType,
                                                          NDimensions,
                                                          NDimensions >
          >
class SpatialObjectWriter:public Object
{
public:

  /** SmartPointer typedef support */
  typedef SpatialObjectWriter  Self;
  typedef SmartPointer< Self > Pointer;

  typedef SpatialObject< NDimensions >        SpatialObjectType;
  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;
  typedef SceneSpatialObject< NDimensions >   SceneType;

  /** base type for MetaConverters -- bidirections conversion btw
   *  SpatialObject & MetaObject
   */
  typedef MetaConverterBase< NDimensions >  MetaConverterBaseType;

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
  void SetInput(SpatialObjectType *input){ m_SpatialObject = input; }

  void SetInput(SceneType *input){ m_Scene = input; }

  itkSetMacro(BinaryPoints, bool);
  itkGetConstMacro(BinaryPoints, bool);

  void SetTransformPrecision(unsigned int precision);

  unsigned int GetTransformPrecision();

  /** Set/Get if the images should be written in a different file */
  itkSetMacro(WriteImagesInSeparateFile, bool);
  itkGetConstMacro(WriteImagesInSeparateFile, bool);

  /** Add a converter for a new MetaObject/SpatialObject type */
  void RegisterMetaConverter(const char *metaTypeName,
                             const char *spatialObjectTypeName,
                             MetaConverterBaseType *converter);

protected:
  SpatialObjectWriter(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  std::string m_FileName;
  bool        m_BinaryPoints;
  bool        m_WriteImagesInSeparateFile;

  SpatialObjectWriter();
  virtual ~SpatialObjectWriter();

private:

  SpatialObjectPointer m_SpatialObject;
  SceneType *          m_Scene;

  MetaSceneConverter< NDimensions, PixelType, TMeshTraits >
  m_MetaToSpatialConverter;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectWriter.hxx"
#endif

#endif // __itkSpatialObjectWriter_h
