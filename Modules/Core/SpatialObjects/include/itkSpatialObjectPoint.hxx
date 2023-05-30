/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSpatialObjectPoint_hxx
#define itkSpatialObjectPoint_hxx


namespace itk
{

template <unsigned int TPointDimension>
SpatialObjectPoint<TPointDimension>::SpatialObjectPoint()
{
  m_Id = -1;

  m_PositionInObjectSpace.Fill(0);

  m_Color.SetRed(1.0); // red by default
  m_Color.SetGreen(0);
  m_Color.SetBlue(0);
  m_Color.SetAlpha(1);

  m_ScalarDictionary.clear();

  m_SpatialObject = nullptr;
}

template <unsigned int TPointDimension>
SpatialObjectPoint<TPointDimension>::SpatialObjectPoint(const SpatialObjectPoint & other)
{
  this->SetId(other.GetId());
  this->SetPositionInObjectSpace(other.GetPositionInObjectSpace());
  this->SetColor(other.GetColor());
  this->SetSpatialObject(other.GetSpatialObject());
  this->SetTagScalarDictionary(other.GetTagScalarDictionary());
}

template <unsigned int TPointDimension>
void
SpatialObjectPoint<TPointDimension>::SetPositionInWorldSpace(const PointType & point)
{
  if (m_SpatialObject == nullptr)
  {
    itkExceptionMacro("The SpatialObject must be set prior to calling.");
  }

  m_PositionInObjectSpace = m_SpatialObject->GetObjectToWorldTransform()->GetInverseTransform()->TransformPoint(point);
}

template <unsigned int TPointDimension>
auto
SpatialObjectPoint<TPointDimension>::GetPositionInWorldSpace() const -> PointType
{
  if (m_SpatialObject == nullptr)
  {
    itkExceptionMacro("The SpatialObject must be set prior to calling.");
  }

  return m_SpatialObject->GetObjectToWorldTransform()->TransformPoint(m_PositionInObjectSpace);
}

template <unsigned int TPointDimension>
void
SpatialObjectPoint<TPointDimension>::SetColor(double r, double g, double b, double a)
{
  m_Color.SetRed(r);
  m_Color.SetGreen(g);
  m_Color.SetBlue(b);
  m_Color.SetAlpha(a);
}

template <unsigned int TPointDimension>
auto
SpatialObjectPoint<TPointDimension>::operator=(const SpatialObjectPoint & rhs) -> Self &
{
  if (this != &rhs)
  {
    this->SetId(rhs.GetId());
    this->SetPositionInObjectSpace(rhs.GetPositionInObjectSpace());
    this->SetColor(rhs.GetColor());
    this->SetTagScalarDictionary(rhs.GetTagScalarDictionary());
    this->SetSpatialObject(rhs.GetSpatialObject());
  }
  return *this;
}

template <unsigned int TPointDimension>
void
SpatialObjectPoint<TPointDimension>::SetTagScalarValue(const std::string & tag, double value)
{
  m_ScalarDictionary[tag] = value;
}

template <unsigned int TPointDimension>
bool
SpatialObjectPoint<TPointDimension>::GetTagScalarValue(const std::string & tag, double & value) const
{
  auto iter = m_ScalarDictionary.find(tag);
  if (iter != m_ScalarDictionary.end())
  {
    value = iter->second;
    return true;
  }
  else
  {
    return false;
  }
}

template <unsigned int TPointDimension>
double
SpatialObjectPoint<TPointDimension>::GetTagScalarValue(const std::string & tag) const
{
  auto iter = m_ScalarDictionary.find(tag);
  if (iter != m_ScalarDictionary.end())
  {
    return iter->second;
  }
  return -1;
}

template <unsigned int TPointDimension>
std::map<std::string, double> &
SpatialObjectPoint<TPointDimension>::GetTagScalarDictionary()
{
  return m_ScalarDictionary;
}

template <unsigned int TPointDimension>
const std::map<std::string, double> &
SpatialObjectPoint<TPointDimension>::GetTagScalarDictionary() const
{
  return m_ScalarDictionary;
}

template <unsigned int TPointDimension>
void
SpatialObjectPoint<TPointDimension>::SetTagScalarDictionary(const std::map<std::string, double> & dict)
{
  m_ScalarDictionary = dict;
}

template <unsigned int TPointDimension>
void
SpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Id: " << m_Id << std::endl;
  os << indent
     << "PositionInObjectSpace: " << static_cast<typename NumericTraits<PointType>::PrintType>(m_PositionInObjectSpace)
     << std::endl;
  os << indent << "Color: " << static_cast<typename NumericTraits<ColorType>::PrintType>(m_Color) << std::endl;

  os << indent << "ScalarDictionary: " << std::endl;
  for (const auto & keyval : m_ScalarDictionary)
  {
    os << indent.GetNextIndent() << keyval.first << ": " << keyval.second << std::endl;
  }

  os << indent << "SpatialObject: " << m_SpatialObject << std::endl;
}
} // end namespace itk

#endif
