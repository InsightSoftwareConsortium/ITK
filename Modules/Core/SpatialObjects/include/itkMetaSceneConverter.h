/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkObject.h"
#include "itkDefaultStaticMeshTraits.h"

#include "metaScene.h"
#include "itkMetaEvent.h"
#include "itkSpatialObject.h"
#include "itkMetaConverterBase.h"

#include <string>
#include <map>

namespace itk
{
/**
 *\class MetaSceneConverter
 *  \brief Converts between MetaObject and SpaitalObject group.
 *
 *  SpatialObject hierarchies are written to disk using the MetaIO
 *  library. This class is responsible for converting between MetaIO
 *  group and SpatialObject group
 *
 *  \sa MetaConverterBase
 *  \ingroup ITKSpatialObjects
 */
template <unsigned int NDimensions = 3,
          typename PixelType = unsigned char,
          typename TMeshTraits = DefaultStaticMeshTraits<PixelType, NDimensions, NDimensions>>
class ITK_TEMPLATE_EXPORT MetaSceneConverter : public Object
{
public:
  /** standard class type alias */
  using Self = MetaSceneConverter;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaSceneConverter, Object);

  /** SpatialObject Scene types */
  using SpatialObjectType = itk::SpatialObject<NDimensions>;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;

  /** Typedef for auxiliary conversion classes */
  using MetaConverterBaseType = MetaConverterBase<NDimensions>;
  using MetaConverterPointer = typename MetaConverterBaseType::Pointer;
  using ConverterMapType = std::map<std::string, MetaConverterPointer>;

  /** Read a MetaFile and create a Scene SpatialObject */
  SpatialObjectPointer
  ReadMeta(const std::string & name);

  /** write out a SpatialObject */
  bool
  WriteMeta(SpatialObjectType * soScene,
            const std::string & fileName,
            unsigned int        depth = SpatialObjectType::MaximumDepth,
            const std::string & spatialObjectTypeName = "");

  itkGetMacro(Event, MetaEvent *);
  itkSetObjectMacro(Event, MetaEvent);

  /** Set if the points should be saved in binary/ASCII */
  itkSetMacro(BinaryPoints, bool);
  itkGetMacro(BinaryPoints, bool);

  /** set/get the precision for writing out numbers as plain text */
  itkSetMacro(TransformPrecision, unsigned int);
  itkGetMacro(TransformPrecision, unsigned int);

  /** Set if the images should be written in different files */
  itkSetMacro(WriteImagesInSeparateFile, bool);
  itkGetConstMacro(WriteImagesInSeparateFile, bool);

  /** add new SpatialObject/MetaObject converters at runtime
   *
   *  Every Converter is mapped to both a metaObject type name
   * and a spatialObject type name -- these need to match what
   * gets read from & written to the MetaIO file
   */
  void
  RegisterMetaConverter(const std::string &     metaTypeName,
                        const std::string &     spatialObjectTypeName,
                        MetaConverterBaseType * converter);

  MetaScene *
  CreateMetaScene(SpatialObjectType * soScene,
                  unsigned int        depth = SpatialObjectType::MaximumDepth,
                  const std::string & name = "");

  SpatialObjectPointer
  CreateSpatialObjectScene(MetaScene * mScene);

protected:
  MetaSceneConverter();
  ~MetaSceneConverter() override = default;

private:
  using TransformType = typename SpatialObjectType::TransformType;

  using MetaObjectListType = std::list<MetaObject *>;

  template <typename TConverter>
  MetaObject *
  SpatialObjectToMetaObject(SpatialObjectPointer & so)
  {
    typename TConverter::Pointer converter = TConverter::New();
    // needed just for Image & ImageMask
    converter->SetWriteImagesInSeparateFile(this->m_WriteImagesInSeparateFile);
    return converter->SpatialObjectToMetaObject(so);
  }
  template <typename TConverter>
  SpatialObjectPointer
  MetaObjectToSpatialObject(const MetaObject * mo)
  {
    typename TConverter::Pointer converter = TConverter::New();
    return converter->MetaObjectToSpatialObject(mo);
  }
  void
  SetTransform(MetaObject * obj, const TransformType * transform);

  void
  SetTransform(SpatialObjectType * so, const MetaObject * meta);

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
#  include "itkMetaSceneConverter.hxx"
#endif

#endif
