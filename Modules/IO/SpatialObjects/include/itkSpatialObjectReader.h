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
#ifndef __itkSpatialObjectReader_h
#define __itkSpatialObjectReader_h

#include "itkMetaSceneConverter.h"
#include "itkMetaConverterBase.h"
#include "itkGroupSpatialObject.h"
#include "itkProcessObject.h"

namespace itk
{
/** \class SpatialObjectReader
 *
 * \brief TODO
 * \ingroup ITKIOSpatialObjects
 */
template< unsigned int NDimensions = 3,
          typename PixelType = uint8_t,
          typename TMeshTraits = DefaultStaticMeshTraits< PixelType, NDimensions, NDimensions >
          >
class SpatialObjectReader:public Object
{
public:

  /** SmartPointer typedef support */
  typedef SpatialObjectReader                 Self;
  typedef SmartPointer< Self >                Pointer;
  typedef SpatialObject< NDimensions >        SpatialObjectType;
  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  typedef GroupSpatialObject< NDimensions > GroupType;
  typedef typename GroupType::Pointer       GroupPointer;

  typedef SceneSpatialObject< NDimensions > SceneType;
  typedef typename SceneType::Pointer       ScenePointer;

  /** base type for MetaConverters -- bidirections conversion btw
   *  SpatialObject & MetaObject
   */
  typedef MetaConverterBase< NDimensions >  MetaConverterBaseType;

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
  ScenePointer GetScene(void) { return m_Scene; }
  GroupPointer GetGroup(void) { return m_Group; }

  /** Set/GetEvent */
  const MetaEvent * GetEvent() { return m_MetaToSpatialConverter.GetEvent(); }
  void SetEvent(MetaEvent *event) { m_MetaToSpatialConverter.SetEvent(event); }

  /** Add a converter for a new MetaObject/SpatialObject type */
  void RegisterMetaConverter(const char *metaTypeName,
                             const char *spatialObjectTypeName,
                             MetaConverterBaseType *converter);

protected:
  SpatialObjectReader(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  std::string m_FileName;

  SpatialObjectReader();
  virtual ~SpatialObjectReader();

private:

  ScenePointer m_Scene;
  GroupPointer m_Group;

  MetaSceneConverter< NDimensions, PixelType, TMeshTraits > m_MetaToSpatialConverter;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectReader.hxx"
#endif

#endif // __itkSpatialObjectReader_h
