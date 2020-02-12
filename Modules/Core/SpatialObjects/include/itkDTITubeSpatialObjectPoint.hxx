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
#ifndef itkDTITubeSpatialObjectPoint_hxx
#define itkDTITubeSpatialObjectPoint_hxx

#include "itkDTITubeSpatialObjectPoint.h"
#include "itksys/SystemTools.hxx"

namespace itk
{
/** Constructor */
template <unsigned int TPointDimension>
DTITubeSpatialObjectPoint<TPointDimension>::DTITubeSpatialObjectPoint()
{
  // Initialize the tensor matrix to identity
  for (auto & i : m_TensorMatrix)
  {
    i = 0;
  }
  m_TensorMatrix[0] = 1;
  m_TensorMatrix[3] = 1;
  m_TensorMatrix[5] = 1;
}

template <unsigned int TPointDimension>
void
DTITubeSpatialObjectPoint<TPointDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/** Translate the enumerated types to a string */
template <unsigned int TPointDimension>
std::string
DTITubeSpatialObjectPoint<TPointDimension>::TranslateEnumToChar(DTITubeSpatialObjectPointFieldEnum name) const
{
  // Do the translation
  switch (static_cast<int>(name))
  {
    case 0:
      return std::string("FA");
    case 1:
      return std::string("ADC");
    case 2:
      return std::string("GA");
    default:
      // Just fall through.
      break;
  }
  return std::string("");
}

/** Add a field to the point list */
template <unsigned int TPointDimension>
void
DTITubeSpatialObjectPoint<TPointDimension>::AddField(const char * name, float value)
{
  FieldType field(itksys::SystemTools::LowerCase(name), value);

  m_Fields.push_back(field);
}

/** Set a field value to the point list */
template <unsigned int TPointDimension>
void
DTITubeSpatialObjectPoint<TPointDimension>::SetField(const char * name, float value)
{
  auto it = m_Fields.begin();

  while (it != m_Fields.end())
  {
    if (!strcmp((*it).first.c_str(), itksys::SystemTools::LowerCase(name).c_str()))
    {
      (*it).second = value;
    }
    it++;
  }
}

/** Set a value to a field in the point list */
template <unsigned int TPointDimension>
void
DTITubeSpatialObjectPoint<TPointDimension>::SetField(DTITubeSpatialObjectPointFieldEnum name, float value)
{
  std::string charname = this->TranslateEnumToChar(name);

  if (!charname.empty())
  {
    this->SetField(charname.c_str(), value);
  }
  else
  {
    std::cout << "DTITubeSpatialObjectPoint::SetField(): enum not defined" << std::endl;
  }
}

/** Add a field to the point list */
template <unsigned int TPointDimension>
void
DTITubeSpatialObjectPoint<TPointDimension>::AddField(DTITubeSpatialObjectPointFieldEnum name, float value)
{
  std::string charname = this->TranslateEnumToChar(name);

  if (!charname.empty())
  {
    FieldType field(itksys::SystemTools::LowerCase(charname).c_str(), value);
    m_Fields.push_back(field);
  }
  else
  {
    std::cout << "DTITubeSpatialObjectPoint::AddField() : enum not defined" << std::endl;
  }
}

/** Return the value of the given field */
template <unsigned int TPointDimension>
float
DTITubeSpatialObjectPoint<TPointDimension>::GetField(const char * name) const
{
  auto it = m_Fields.begin();

  while (it != m_Fields.end())
  {
    if (!strcmp((*it).first.c_str(), itksys::SystemTools::LowerCase(name).c_str()))
    {
      return (*it).second;
    }
    it++;
  }
  return -1;
}

/** Add a field to the point list */
template <unsigned int TPointDimension>
float
DTITubeSpatialObjectPoint<TPointDimension>::GetField(DTITubeSpatialObjectPointFieldEnum name) const
{
  std::string charname = this->TranslateEnumToChar(name);
  if (!charname.empty())
  {
    return this->GetField(itksys::SystemTools::LowerCase(charname).c_str());
  }
  std::cout << "DTITubeSpatialObjectPoint::GetField() : enum not defined" << std::endl;
  return -1;
}

template <unsigned int TPointDimension>
typename DTITubeSpatialObjectPoint<TPointDimension>::Self &
DTITubeSpatialObjectPoint<TPointDimension>::operator=(const DTITubeSpatialObjectPoint & rhs)
{
  if (this != &rhs)
  {
    // Point
    this->SetId(rhs.GetId());
    this->SetPositionInObjectSpace(rhs.GetPositionInObjectSpace());
    this->SetColor(rhs.GetColor());
    this->SetSpatialObject(rhs.GetSpatialObject());

    // Tube
    this->SetRadiusInObjectSpace(rhs.GetRadiusInObjectSpace());
    this->SetTangentInObjectSpace(rhs.GetTangentInObjectSpace());
    this->SetNormal1InObjectSpace(rhs.GetNormal1InObjectSpace());
    this->SetNormal2InObjectSpace(rhs.GetNormal2InObjectSpace());

    this->SetRidgeness(rhs.GetRidgeness());
    this->SetMedialness(rhs.GetMedialness());
    this->SetBranchness(rhs.GetBranchness());
    this->SetAlpha1(rhs.GetAlpha1());
    this->SetAlpha2(rhs.GetAlpha2());
    this->SetAlpha3(rhs.GetAlpha3());

    // Class
    m_Fields.clear();
    const FieldListType & fields = rhs.GetFields();
    auto                  it = fields.begin();
    while (it != fields.end())
    {
      this->AddField((*it).first.c_str(), (*it).second);
      it++;
    }
    for (unsigned int i = 0; i < 6; i++)
    {
      m_TensorMatrix[i] = rhs.m_TensorMatrix[i];
    }
  }
  return *this;
}
} // end namespace itk

#endif
