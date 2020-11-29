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
#ifndef itkAttributeLabelObject_h
#define itkAttributeLabelObject_h

#include "itkLabelObject.h"
#include "itkLabelMap.h"

namespace itk
{


namespace Functor
{

template <typename TLabelObject>
class AttributeLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::AttributeValueType;

  inline const AttributeValueType
  operator()(const LabelObjectType * labelObject)
  {
    return labelObject->GetAttribute();
  }

  inline void
  operator()(LabelObjectType * labelObject, AttributeValueType value)
  {
    labelObject->SetAttribute(value);
  }
};

} // namespace Functor


/**
 *\class AttributeLabelObject
 *  \brief A LabelObject with a generic attribute
 *
 * The attribute type is defined in the third template parameter.
 * The attribute is then accessible with the GetAttribute() and SetAttribute
 * methods or through the accessor itk::Functor::AttributeLabelObjectAccessor.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelObject, ShapeLabelObject, StatisticsLabelObject
 *
 * \ingroup DataRepresentation
 * \ingroup ITKLabelMap
 */
template <typename TLabel, unsigned int VImageDimension, typename TAttributeValue>
class AttributeLabelObject : public LabelObject<TLabel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AttributeLabelObject);

  /** Standard class type aliases */
  using Self = AttributeLabelObject;
  using Superclass = LabelObject<TLabel, VImageDimension>;
  using Pointer = SmartPointer<Self>;
  using LabelObjectType = typename Superclass::LabelObjectType;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AttributeLabelObject, LabelObject);

  using LabelMapType = LabelMap<Self>;

  static constexpr unsigned int ImageDimension = VImageDimension;

  using IndexType = typename Superclass::IndexType;

  using LabelType = TLabel;

  using LineType = typename Superclass::LineType;

  using LengthType = typename Superclass::LengthType;

  using AttributeValueType = TAttributeValue;

  void
  SetAttribute(const AttributeValueType & v)
  {
    m_Attribute = v;
  }

  const AttributeValueType &
  GetAttribute() const
  {
    return m_Attribute;
  }

  AttributeValueType
  GetAttribute()
  {
    return m_Attribute;
  }

  template <typename TSourceLabelObject>
  void
  CopyAttributesFrom(const TSourceLabelObject * src)
  {
    itkAssertOrThrowMacro((src != nullptr), "Null Pointer");
    Superclass::template CopyAttributesFrom<TSourceLabelObject>(src);

    m_Attribute = src->GetAttribute();
  }

  template <typename TSourceLabelObject>
  void
  CopyAllFrom(const TSourceLabelObject * src)
  {
    itkAssertOrThrowMacro((src != nullptr), "Null Pointer");
    this->template CopyLinesFrom<TSourceLabelObject>(src);
    this->template CopyAttributesFrom<TSourceLabelObject>(src);
  }

protected:
  AttributeLabelObject()
  {
    // how to initialize the attribute ?
  }


  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);

    os << indent << "Attribute: " << m_Attribute << std::endl;
  }

private:
  AttributeValueType m_Attribute;
};

} // end namespace itk

#endif
