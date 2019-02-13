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
#include <map>

#include "itkLightObject.h"
#include "itkRGBAPixel.h"
#include "itkTimeStamp.h"
#include "itkSpatialObjectFactory.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 * This class contains the objects properties such as colors, opacity, etc...
 * it's templated over the representation to use for each color component.
 */

class SpatialObjectProperty
  : public LightObject
{
public:
  //ITK_DISALLOW_COPY_AND_ASSIGN(SpatialObjectProperty);

  using Self = SpatialObjectProperty;
  using Superclass = LightObject;

  using ColorType = RGBAPixel< double >;

  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  itkNewMacro(Self);
  itkTypeMacro(SpatialObjectProperty, LightObject);

  itkSetMacro( Color, ColorType );
  itkGetConstMacro( Color, ColorType );

  void SetColor(double r, double g, double b);

  void SetRed(double r);
  double GetRed() const;

  void SetGreen(double g);
  double GetGreen() const;

  void SetBlue(double b);
  double GetBlue() const;

  void SetAlpha(double a);
  double GetAlpha() const;

  SpatialObjectProperty();
  ~SpatialObjectProperty() override;

  itkSetMacro( Name, std::string );
  itkGetConstMacro( Name, std::string );

  void SetTagScalarValue( const std::string & tag, double value );
  void SetTagStringValue( const std::string & tag, const std::string & value );

  double      GetTagScalarValue( const std::string & tag ) const;
  std::string SetTagStringValue( const std::string & tag );

  std::map< std::string, double > &            GetTagScalarDictionary();
  const std::map< std::string, double > &      GetTagScalarDictionary() const;
  std::map< std::string, std::string > &       GetTagStringDictionary();
  const std::map< std::string, std::string > & GetTagStringDictionary() const;

  void SetTagScalarDictionary( const std::map< std::string, double > & dict );
  void SetTagStringDictionary( const std::map< std::string,
    std::string > & dict );

  void DeepCopy(const SpatialObjectProperty * rhs );

  itkGetConstMacro( MTime, ModifiedTimeType );

protected:

  void PrintSelf(std::ostream & os, Indent indent) const override;

  void Modified() { m_MTime++; }

private:

  ColorType        m_Color;
  std::string      m_Name;

  std::map< std::string, double >      m_ScalarDictionary;
  std::map< std::string, std::string > m_StringDictionary;

  ModifiedTimeType m_MTime;
};

}

#endif // __SpatialObjectProperty_h
