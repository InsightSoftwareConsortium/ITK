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
#ifndef itkSpatialObjectProperty_h
#define itkSpatialObjectProperty_h

#include <string>

#include "itkRGBAPixel.h"
#include "itkLightObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 * This class contains the objects properties such as colors, opacity, etc...
 * it's templated over the representation to use for each color component.
 */

template< typename TComponentType = double >
class ITK_TEMPLATE_EXPORT SpatialObjectProperty:
  public LightObject
{
public:
  //ITK_DISALLOW_COPY_AND_ASSIGN(SpatialObjectProperty);

  using Self = SpatialObjectProperty< TComponentType >;
  using Superclass = LightObject;
  using PixelType = RGBAPixel< TComponentType >;

  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  itkNewMacro(Self);
  itkTypeMacro(SpatialObjectProperty, LightObject);

  const PixelType & GetColor() const;

  void SetColor(const PixelType & color);

  void SetColor(TComponentType r, TComponentType g, TComponentType b);

  void SetRed(TComponentType r);

  TComponentType GetRed() const;

  void SetGreen(TComponentType g);

  TComponentType GetGreen() const;

  void SetBlue(TComponentType b);

  TComponentType GetBlue() const;

  void SetAlpha(TComponentType a);

  TComponentType GetAlpha() const;

  SpatialObjectProperty();
  ~SpatialObjectProperty() override = default;

  void SetName(const std::string & name);
  const std::string & GetName() const;

  void SetTagScalarValue( const std::string & tag, double value );
  void SetTagStringValue( const std::string & tag, const std::string & value );

  double      GetTagScalarValue( const std::string & tag );
  std::string SetTagStringValue( const std::string & tag );

  std::map< std::string, double >      & GetTagScalarDictionary();
  std::map< std::string, std::string > & GetTagStringDictionary();

  void SetTagScalarDictionary( const std::map< std::string, double > & dict );
  void SetTagStringDictionary( const std::map< std::string, std::string > & dict );

  Self & operator=(const Self & rhs );

  ModifiedTimeType GetMTime()
  { return m_MTime; }

protected:

  void PrintSelf(std::ostream & os, Indent indent) const override;

  void Modified() { m_MTime++; }

private:

  PixelType        m_Color;
  std::string      m_Name;

  std::map< std::string, double >      m_ScalarDictionary;
  std::map< std::string, std::string > m_StringDictionary;

  ModifiedTimeType m_MTime;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectProperty.hxx"
#endif

#endif // __SpatialObjectProperty_h
