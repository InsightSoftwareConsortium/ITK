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
#ifndef itkDTITubeSpatialObjectPoint_hxx
#define itkDTITubeSpatialObjectPoint_hxx

#include "itkDTITubeSpatialObjectPoint.h"
#include "itksys/SystemTools.hxx"

namespace itk
{
/** Constructor */
template< unsigned int TPointDimension >
DTITubeSpatialObjectPoint< TPointDimension >
::DTITubeSpatialObjectPoint(void)
{
  // Initialize the tensor matrix to identity
  for ( unsigned int i = 0; i < 6; i++ )
    {
    m_TensorMatrix[i] = 0;
    }
  m_TensorMatrix[0] = 1;
  m_TensorMatrix[3] = 1;
  m_TensorMatrix[5] = 1;
}

/** Destructor */
template< unsigned int TPointDimension >
DTITubeSpatialObjectPoint< TPointDimension >
::~DTITubeSpatialObjectPoint(void)
{}

template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/** Translate the enumerated types to a string */
template< unsigned int TPointDimension >
std::string
DTITubeSpatialObjectPoint< TPointDimension >
::TranslateEnumToChar(FieldEnumType name) const
{
  // Do the translation
  switch ( name )
    {
    case 0:
      return std::string("FA");
    case 1:
      return std::string("ADC");
    case 2:
      return std::string("GA");
    default:
    //Just fall through.
      break;
    }
  return std::string("");
}

/** Add a field to the point list */
template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::AddField(const char *name, float value)
{
  FieldType field(itksys::SystemTools::LowerCase(name), value);

  m_Fields.push_back(field);
}

/** Set a field value to the point list */
template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::SetField(const char *name, float value)
{
  FieldListType::iterator it = m_Fields.begin();

  while ( it != m_Fields.end() )
    {
    if ( !strcmp( ( *it ).first.c_str(),
                  itksys::SystemTools::LowerCase(name).c_str() ) )
      {
      ( *it ).second = value;
      }
    it++;
    }
}

/** Set a value to a field in the point list */
template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::SetField(FieldEnumType name, float value)
{
  std::string charname = this->TranslateEnumToChar(name);

  if ( charname.size() > 0 )
    {
    this->SetField(charname.c_str(), value);
    }
  else
    {
    std::cout << "DTITubeSpatialObjectPoint::SetField(): enum not defined"
              << std::endl;
    }
}

/** Add a field to the point list */
template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::AddField(FieldEnumType name, float value)
{
  std::string charname = this->TranslateEnumToChar(name);

  if ( charname.size() > 0 )
    {
    FieldType field(itksys::SystemTools::LowerCase(charname).c_str(), value);
    m_Fields.push_back(field);
    }
  else
    {
    std::cout << "DTITubeSpatialObjectPoint::AddField() : enum not defined"
              << std::endl;
    }
}

/** Return the value of the given field */
template< unsigned int TPointDimension >
float
DTITubeSpatialObjectPoint< TPointDimension >
::GetField(const char *name) const
{
  FieldListType::const_iterator it = m_Fields.begin();

  while ( it != m_Fields.end() )
    {
    if ( !strcmp( ( *it ).first.c_str(),
                  itksys::SystemTools::LowerCase(name).c_str() ) )
      {
      return ( *it ).second;
      }
    it++;
    }
  return -1;
}

/** Add a field to the point list */
template< unsigned int TPointDimension >
float
DTITubeSpatialObjectPoint< TPointDimension >
::GetField(FieldEnumType name) const
{
  std::string charname = this->TranslateEnumToChar(name);
  if ( charname.size() > 0 )
    {
    return this->GetField( itksys::SystemTools::LowerCase(charname).c_str() );
    }
  std::cout << "DTITubeSpatialObjectPoint::GetField() : enum not defined"
            << std::endl;
  return -1;
}

template< unsigned int TPointDimension >
typename DTITubeSpatialObjectPoint< TPointDimension >::Self &
DTITubeSpatialObjectPoint< TPointDimension >
::operator=(const DTITubeSpatialObjectPoint & rhs)
{
  if(this != &rhs)
    {
    // Copy the extra fields
    m_Fields.clear();
    const FieldListType &         fields = rhs.GetFields();
    FieldListType::const_iterator it = fields.begin();
    while ( it != fields.end() )
      {
      this->AddField( ( *it ).first.c_str(), ( *it ).second );
      it++;
      }

    this->m_ID = rhs.m_ID;

    for ( unsigned int i = 0; i < 6; i++ )
      {
      m_TensorMatrix[i] = rhs.m_TensorMatrix[i];
      }

    this->m_NumDimensions = rhs.m_NumDimensions;
    this->m_X = rhs.m_X;
    this->m_T = rhs.m_T;
    this->m_R = rhs.m_R;
    this->m_Normal1 = rhs.m_Normal1;
    this->m_Normal2 = rhs.m_Normal2;
    this->m_Color = rhs.m_Color;
    }
  return *this;
}
} // end namespace itk

#endif
