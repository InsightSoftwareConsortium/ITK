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
#ifndef itkSpatialObjectReader_h
#define itkSpatialObjectReader_h

#include "itkMetaSceneConverter.h"
#include "itkMetaConverterBase.h"
#include "itkSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkProcessObject.h"

namespace itk
{
/** \class SpatialObjectReader
 *
 * \ingroup ITKIOSpatialObjects
 */
template <unsigned int NDimensions = 3,
          typename PixelType = unsigned char,
          typename TMeshTraits = DefaultStaticMeshTraits<PixelType, NDimensions, NDimensions>>
class ITK_TEMPLATE_EXPORT SpatialObjectReader : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpatialObjectReader);

  /** SmartPointer type alias support */
  using Self = SpatialObjectReader;
  using Pointer = SmartPointer<Self>;
  using SpatialObjectType = SpatialObject<NDimensions>;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using GroupType = GroupSpatialObject<NDimensions>;
  using GroupPointer = typename GroupType::Pointer;

  /** base type for MetaConverters -- bidirections conversion btw
   *  SpatialObject & MetaObject
   */
  using MetaConverterBaseType = MetaConverterBase<NDimensions>;
  using MetaSceneConverterType = MetaSceneConverter<NDimensions, PixelType, TMeshTraits>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  using Superclass = Object;
  itkTypeMacro(SpatialObjectReader, Object);

  /** Load a scene file. */
  void
  Update();

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Get the output */
  SpatialObjectPointer
  GetOutput()
  {
    return m_SpatialObject;
  }

  /** Get the output, with a group spatial object added to the top.  This
   *    addition makes it easy to use GetChildren() to get the list of
   *    objects read. */
  GroupPointer
  GetGroup()
  {
    if (m_Group == nullptr)
    {
      if (m_SpatialObject->GetTypeName() == "GroupSpatialObject")
      {
        m_Group = static_cast<GroupType *>(m_SpatialObject.GetPointer());
      }
      else
      {
        m_Group = GroupType::New();
        m_Group->AddChild(m_SpatialObject);
      }
    }
    return m_Group;
  }

  /** Set/GetEvent */
  const MetaEvent *
  GetEvent()
  {
    return m_MetaToSpatialConverter->GetEvent();
  }

  void
  SetEvent(MetaEvent * event)
  {
    m_MetaToSpatialConverter->SetEvent(event);
  }

  /** Add a converter for a new MetaObject/SpatialObject type */
  void
  RegisterMetaConverter(const char *            metaTypeName,
                        const char *            spatialObjectTypeName,
                        MetaConverterBaseType * converter);

protected:
  std::string m_FileName;

  SpatialObjectReader();
  ~SpatialObjectReader() override = default;

private:
  GroupPointer m_Group;

  SpatialObjectPointer m_SpatialObject;

  typename MetaSceneConverterType::Pointer m_MetaToSpatialConverter;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpatialObjectReader.hxx"
#endif

#endif // itkSpatialObjectReader_h
