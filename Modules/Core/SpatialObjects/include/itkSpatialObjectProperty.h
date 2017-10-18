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

template< typename TComponentType = float >
class ITK_TEMPLATE_EXPORT SpatialObjectProperty:
  public LightObject
{
public:

  typedef SpatialObjectProperty< TComponentType > Self;
  typedef LightObject                             Superclass;
  typedef RGBAPixel< TComponentType >             PixelType;
  typedef std::string                             StringType;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

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
  virtual ~SpatialObjectProperty() ITK_OVERRIDE;

  void SetName(const char *name);

  StringType GetName() const;

  ModifiedTimeType GetMTime(void){ return m_MTime; }

protected:

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void Modified(void){ m_MTime++; }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpatialObjectProperty);

  PixelType        m_Color;
  StringType       m_Name;
  ModifiedTimeType m_MTime;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectProperty.hxx"
#endif

#endif // __SpatialObjectProperty_h
