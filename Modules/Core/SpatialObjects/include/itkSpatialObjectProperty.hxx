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
#ifndef itkSpatialObjectProperty_hxx
#define itkSpatialObjectProperty_hxx

#include "itkSpatialObjectProperty.h"

namespace itk
{
template< typename TComponentType >
SpatialObjectProperty< TComponentType >
::SpatialObjectProperty()
{
  m_MTime = 0;
  m_Color.SetRed(1);
  m_Color.SetGreen(1);
  m_Color.SetBlue(1);
  m_Color.SetAlpha(1);
  m_Name = "";
  m_ScalarDictionary.clear();
  m_StringDictionary.clear();
}

template< typename TComponentType >
SpatialObjectProperty< TComponentType >
::~SpatialObjectProperty() = default;

template< typename TComponentType >
const typename SpatialObjectProperty< TComponentType >::PixelType &
SpatialObjectProperty< TComponentType >
::GetColor() const
{
  return m_Color;
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetColor(const PixelType & color)
{
  m_Color = color;
  this->Modified();
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetColor(TComponentType r, TComponentType g, TComponentType b)
{
  m_Color.SetRed(r);
  m_Color.SetGreen(g);
  m_Color.SetBlue(b);
  this->Modified();
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetRed(TComponentType r)
{
  m_Color.SetRed(r);
  this->Modified();
}

template< typename TComponentType >
TComponentType
SpatialObjectProperty< TComponentType >
::GetRed() const
{
  return m_Color.GetRed();
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetGreen(TComponentType g)
{
  m_Color.SetGreen(g);
  this->Modified();
}

template< typename TComponentType >
TComponentType
SpatialObjectProperty< TComponentType >
::GetGreen() const
{
  return m_Color.GetGreen();
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetBlue(TComponentType b)
{
  m_Color.SetBlue(b);
  this->Modified();
}

template< typename TComponentType >
TComponentType
SpatialObjectProperty< TComponentType >
::GetBlue() const
{
  return m_Color.GetBlue();
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetAlpha(TComponentType a)
{
  m_Color.SetAlpha(a);
  this->Modified();
}

template< typename TComponentType >
TComponentType
SpatialObjectProperty< TComponentType >
::GetAlpha() const
{
  return m_Color.GetAlpha();
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetName(const std::string & name)
{
  m_Name = name;
  this->Modified();
}

template< typename TComponentType >
const std::string &
SpatialObjectProperty< TComponentType >
::GetName() const
{
  return m_Name;
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetTagScalarValue( const std::string & tag, double value )
{
  m_ScalarDictionary[ tag ] = value;
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetTagStringValue( const std::string & tag, const std::string & value )
{
  m_StringDictionary[ tag ] = value;
}

template< typename TComponentType >
double
SpatialObjectProperty< TComponentType >
::GetTagScalarValue( const std::string & tag )
{
  return m_ScalarDictionary[ tag ];
}

template< typename TComponentType >
std::map< std::string, double > &
SpatialObjectProperty< TComponentType >
::GetTagScalarDictionary()
{
  return m_ScalarDictionary;
}

template< typename TComponentType >
std::map< std::string, std::string > &
SpatialObjectProperty< TComponentType >
::GetTagStringDictionary()
{
  return m_StringDictionary;
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetTagScalarDictionary( const std::map< std::string, double > & dict )
{
  m_StringDictionary = dict;
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::SetTagStringDictionary(const std::map< std::string, std::string > & dict )
{
  m_StringDictionary = dict;
}

template< typename TComponentType >
Self &
SpatialObjectProperty< TComponentType >
::operator=( const Self & rhs )
{
  this->SetName( rhs.GetName() );
  this->SetColor( rhs.GetColor() );

  this->SetScalarDictionary( rhs.GetScalarDictionary() );
  this->SetStringDictionary( rhs.GetStringDictionary() );
}

template< typename TComponentType >
void
SpatialObjectProperty< TComponentType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Name: " << m_Name << std::endl;
  os << indent << "RGBA: " << m_Color.GetRed() << " ";
  os << m_Color.GetGreen() << " ";
  os << m_Color.GetBlue() << std::endl;
}
} // end of namespace itk

#endif // __SpatialObjectProperty_hxx
