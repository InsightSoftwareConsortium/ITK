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
#ifndef itkMetaSceneConverter_h
#define itkMetaSceneConverter_h

#include "metaScene.h"
#include "itkMetaEvent.h"
#include "itkGroupSpatialObject.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkMetaConverterBase.h"
#include <string>
#include <map>

namespace itk
{
/** \class MetaSceneConverter
 *  \brief Converts between MetaObject and SpaitalObject scenes.
 *
 *  SpatialObject hierarchies are written to disk using the MetaIO
 *  library. This class is responsible for converting between MetaIO
 *  scenes and SpatialObject scenes
 *
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template< unsigned int NDimensions,
          typename PixelType = unsigned char,
          typename TMeshTraits =
            DefaultStaticMeshTraits< PixelType, NDimensions, NDimensions >
          >
class ITK_TEMPLATE_EXPORT MetaSceneConverter
{
public:

  /** SpatialObject Scene types */
  using SceneType = itk::GroupSpatialObject< NDimensions >;
  using ScenePointer = typename  SceneType::Pointer;

  /** Typedef for auxiliary conversion classes */
  using MetaConverterBaseType = MetaConverterBase< NDimensions >;
  using MetaConverterPointer = typename MetaConverterBaseType::Pointer;
  using ConverterMapType = std::map< std::string, MetaConverterPointer >;

  MetaSceneConverter();
  ~MetaSceneConverter() = default;

  static constexpr unsigned int MaximumDepth = 9999999;

  /** Read a MetaFile and create a Scene SpatialObject */
  ScenePointer ReadMeta(const char *name);

  /** write out a Scene SpatialObject */
  bool WriteMeta(SceneType *scene, const char *fileName,
                 unsigned int depth = MaximumDepth,
                 char *spatialObjectTypeName = nullptr);

  const MetaEvent * GetEvent() const { return m_Event; }
  void  SetEvent(MetaEvent *event) { m_Event = event; }

  /** Set if the points should be saved in binary/ASCII */
  void SetBinaryPoints(bool binary) { m_BinaryPoints = binary; }

  /** set/get the precision for writing out numbers as plain text */
  void SetTransformPrecision(unsigned int precision)
  {
    m_TransformPrecision = precision;
  }
  unsigned int GetTransformPrecision(){ return m_TransformPrecision; }

  /** Set if the images should be written in different files */
  void SetWriteImagesInSeparateFile(bool separate)
  {
    m_WriteImagesInSeparateFile = separate;
  }
  /** add new SpatialObject/MetaObject converters at runtime
   *
   *  Every Converter is mapped to both a metaObject type name
   * and a spatialObject type name -- these need to match what
   * gets read from & written to the MetaIO file
   */
  void RegisterMetaConverter(const char *metaTypeName,
                             const char *spatialObjectTypeName,
                             MetaConverterBaseType *converter);

  MetaScene * CreateMetaScene(SceneType *scene,
                              unsigned int depth = MaximumDepth,
                              const std::string & name = nullptr);

  ScenePointer CreateSpatialObjectScene(MetaScene *scene);

private:

  using SpatialObjectType = itk::SpatialObject< NDimensions >;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using TransformType = typename SpatialObjectType::TransformType;

  using MetaObjectListType = std::list< MetaObject * >;

  template <typename TConverter>
    MetaObject *SpatialObjectToMetaObject(SpatialObjectPointer &so)
  {
    typename TConverter::Pointer converter = TConverter::New();
    // needed just for Image & ImageMask
    converter->SetWriteImagesInSeparateFile(this->m_WriteImagesInSeparateFile);
    return converter->SpatialObjectToMetaObject(so);
  }
  template <typename TConverter>
    SpatialObjectPointer MetaObjectToSpatialObject(const MetaObject *mo)
  {
    typename TConverter::Pointer converter = TConverter::New();
    return converter->MetaObjectToSpatialObject(mo);
  }
  void SetTransform(MetaObject *obj, TransformType *transform);

  void SetTransform(SpatialObjectType *so, MetaObject *obj);

  double m_Orientation[100];
  double m_Position[10];
  double m_CenterOfRotation[10];

  MetaEvent *      m_Event;
  bool             m_BinaryPoints;
  bool             m_WriteImagesInSeparateFile;
  unsigned int     m_TransformPrecision;
  ConverterMapType m_ConverterMap;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMetaSceneConverter.hxx"
#endif

#endif
