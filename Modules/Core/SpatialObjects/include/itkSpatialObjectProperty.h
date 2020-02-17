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
#ifndef itkSpatialObjectProperty_h
#define itkSpatialObjectProperty_h

#include <string>
#include <map>

#include "itkLightObject.h"
#include "itkRGBAPixel.h"
#include "itkTimeStamp.h"
#include "itkObjectFactory.h"

#include "ITKSpatialObjectsExport.h"

namespace itk
{
/**
 * This class contains the objects properties such as colors, opacity, etc...
 * it's templated over the representation to use for each color component.
 */

class ITKSpatialObjects_EXPORT SpatialObjectProperty
{
public:
  SpatialObjectProperty();

  virtual ~SpatialObjectProperty() = default;

  using Self = SpatialObjectProperty;

  using ColorType = RGBAPixel<double>;

  virtual void
  Clear();

  void
  SetColor(const ColorType & color)
  {
    m_Color = color;
  }

  ColorType &
  GetColor()
  {
    return m_Color;
  }

  const ColorType &
  GetColor() const
  {
    return m_Color;
  }

  void
  SetColor(double r, double g, double b);

  void
  SetRed(double r);
  double
  GetRed() const;

  void
  SetGreen(double g);
  double
  GetGreen() const;

  void
  SetBlue(double b);
  double
  GetBlue() const;

  void
  SetAlpha(double a);
  double
  GetAlpha() const;

  void
  SetName(const std::string & name)
  {
    m_Name = name;
  }

  std::string &
  GetName()
  {
    return m_Name;
  }

  const std::string &
  GetName() const
  {
    return m_Name;
  }

  void
  SetTagScalarValue(const std::string & tag, double value);
  void
  SetTagStringValue(const std::string & tag, const std::string & value);

  bool
  GetTagScalarValue(const std::string & tag, double & value) const;
  bool
  GetTagStringValue(const std::string & tag, std::string & value) const;

  std::map<std::string, double> &
  GetTagScalarDictionary();
  const std::map<std::string, double> &
  GetTagScalarDictionary() const;
  std::map<std::string, std::string> &
  GetTagStringDictionary();
  const std::map<std::string, std::string> &
  GetTagStringDictionary() const;

  void
  SetTagScalarDictionary(const std::map<std::string, double> & dict);
  void
  SetTagStringDictionary(const std::map<std::string, std::string> & dict);

  void
  Print(std::ostream & os) const
  {
    this->PrintSelf(os, 3);
  }

  Self &
  operator=(const SpatialObjectProperty & rhs);

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const;

private:
  ColorType m_Color;

  std::string m_Name;

  std::map<std::string, double>      m_ScalarDictionary;
  std::map<std::string, std::string> m_StringDictionary;
};

} // namespace itk

#endif // __SpatialObjectProperty_h
