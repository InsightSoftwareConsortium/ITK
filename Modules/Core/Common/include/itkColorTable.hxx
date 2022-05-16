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
#ifndef itkColorTable_hxx
#define itkColorTable_hxx

#include "itkNumericTraits.h"
#include "vnl/vnl_sample.h"

#include <sstream>
#include <iomanip>

namespace itk
{

template <typename TComponent>
void
ColorTable<TComponent>::DeleteColors()
{
  m_Color.resize(0);
  m_ColorName.resize(0);
}

template <typename TComponent>
void
ColorTable<TComponent>::UseDiscreteColors()
{
  this->DeleteColors();

  m_NumberOfColors = 8;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);

  typename NumericTraits<TComponent>::RealType scale;
  typename NumericTraits<TComponent>::RealType shift;
  if (NumericTraits<TComponent>::is_integer)
  {
    scale = static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::max()) -
            static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::NonpositiveMin());
    shift = static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::NonpositiveMin());
  }
  else
  {
    scale = NumericTraits<TComponent>::OneValue();
    shift = NumericTraits<TComponent>::ZeroValue();
  }

  m_Color[0].Set((TComponent)(0.9 * scale + shift), (TComponent)(shift), (TComponent)(shift));
  m_ColorName[0] = "Red";

  m_Color[1].Set((TComponent)(0.8 * scale + shift), (TComponent)(shift), (TComponent)(0.8 * scale + shift));
  m_ColorName[1] = "Purple";

  m_Color[2].Set((TComponent)(shift), (TComponent)(0.8 * scale + shift), (TComponent)(0.8 * scale + shift));
  m_ColorName[2] = "Aqua";

  m_Color[3].Set((TComponent)(0.8 * scale + shift), (TComponent)(0.8 * scale + shift), (TComponent)(shift));
  m_ColorName[3] = "Yellow";

  m_Color[4].Set((TComponent)(shift), (TComponent)(0.9 * scale + shift), (TComponent)(shift));
  m_ColorName[4] = "Green";

  m_Color[5].Set((TComponent)(shift), (TComponent)(shift), (TComponent)(0.9 * scale + shift));
  m_ColorName[5] = "Blue";

  m_Color[6].Set(
    (TComponent)(0.7 * scale + shift), (TComponent)(0.7 * scale + shift), (TComponent)(0.7 * scale + shift));
  m_ColorName[6] = "Grey0.70";
  //
  // to avoid numeric exception, need to make
  // sure that the value assigned is clamped at
  // max for TComponent.  Exceptions were happening
  // on this assignment, even if realMax was
  // set to NumericTraits<TComponent>::max().
  typename NumericTraits<TComponent>::RealType realMax(1.0 * scale + shift);
  TComponent                                   pixelMax(NumericTraits<TComponent>::max());
  // Converting from TComponent to RealType may introduce a rounding error, so do static_cast
  constexpr auto max_value_converted =
    static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::max());
  if (realMax < max_value_converted)
  {
    pixelMax = static_cast<TComponent>(realMax);
  }
  m_Color[7].Set(pixelMax, pixelMax, pixelMax);
  m_ColorName[7] = "White";
}

template <typename TComponent>
void
ColorTable<TComponent>::UseGrayColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);

  typename NumericTraits<TComponent>::RealType range;
  typename NumericTraits<TComponent>::RealType minimum;
  if (NumericTraits<TComponent>::is_integer)
  {
    range = static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::max()) -
            static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::NonpositiveMin());
    minimum = NumericTraits<TComponent>::NonpositiveMin();
  }
  else
  {
    range = NumericTraits<TComponent>::OneValue();
    minimum = NumericTraits<TComponent>::ZeroValue();
  }
  typename NumericTraits<TComponent>::RealType delta;
  if (m_NumberOfColors > 1)
  {
    delta = range / (m_NumberOfColors - 1);
  }
  else
  {
    delta = 0.0;
  }

  // Converting from TComponent to RealType may introduce a rounding error, so do static_cast
  constexpr auto max_value_converted =
    static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::max());
  for (i = 0; i < m_NumberOfColors; ++i)
  {
    typename NumericTraits<TComponent>::RealType realGray(minimum + i * delta);

    TComponent gray = NumericTraits<TComponent>::max();
    if (realGray < max_value_converted)
    {
      gray = static_cast<TComponent>(realGray);
    }

    m_Color[i].Set(gray, gray, gray);
    std::ostringstream name;
    name << "Gray" << std::fixed << std::setprecision(2) << static_cast<float>(gray);
    m_ColorName[i] = name.str();
  }
}

template <typename TComponent>
void
ColorTable<TComponent>::UseHeatColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);

  typename NumericTraits<TComponent>::RealType scale;
  typename NumericTraits<TComponent>::RealType shift;
  if (NumericTraits<TComponent>::is_integer)
  {
    scale = static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::max()) -
            static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::NonpositiveMin());
    shift = static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::NonpositiveMin());
  }
  else
  {
    scale = NumericTraits<TComponent>::OneValue();
    shift = NumericTraits<TComponent>::ZeroValue();
  }
  // Converting from TComponent to RealType may introduce a rounding error, so do static_cast
  constexpr auto max_value_converted =
    static_cast<typename NumericTraits<TComponent>::RealType>(NumericTraits<TComponent>::max());
  for (i = 0; i < n / 2.0; ++i)
  {
    //
    // avoid overflow
    typename NumericTraits<TComponent>::RealType realR(((i + 1) / (n / 2.0 + 1)) * scale + shift);
    TComponent                                   r(NumericTraits<TComponent>::max());
    if (realR < max_value_converted)
    {
      r = static_cast<TComponent>(realR);
    }
    auto g(static_cast<TComponent>(shift));
    auto b(static_cast<TComponent>(shift));
    m_Color[i].Set(r, g, b);
    std::ostringstream name;
    name << "Heat" << std::fixed << std::setprecision(2) << i / static_cast<float>(n);
    m_ColorName[i] = name.str();
  }

  for (i = 0; i < n / 2; ++i)
  {
    typename NumericTraits<TComponent>::RealType rdouble(1.0 * scale + shift);
    TComponent                                   r(NumericTraits<TComponent>::max());
    if (rdouble < max_value_converted)
    {
      r = static_cast<TComponent>(rdouble);
    }
    auto g = static_cast<TComponent>(((i + 1) / (n / 2.0 + 1)) * scale + shift);
    auto b = static_cast<TComponent>(((i + 1) / (n / 2.0 + 1)) * scale + shift);
    m_Color[(size_t)(i + n / 2.0)].Set(r, g, b);
    std::ostringstream name;
    name << "Heat" << std::fixed << std::setprecision(2) << (i + n / 2.0) / static_cast<float>(n);
    m_ColorName[static_cast<size_t>((i + n / 2.0))] = name.str();
  }
}

template <typename TComponent>
void
ColorTable<TComponent>::UseRandomColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);
  TComponent r, g, b;
  TComponent minimum, maximum;
  if (NumericTraits<TComponent>::is_integer)
  {
    minimum = NumericTraits<TComponent>::NonpositiveMin();
    maximum = NumericTraits<TComponent>::max();
  }
  else
  {
    minimum = NumericTraits<TComponent>::ZeroValue();
    maximum = NumericTraits<TComponent>::OneValue();
  }
  for (i = 0; i < n; ++i)
  {
    r = static_cast<TComponent>(vnl_sample_uniform(minimum, maximum));
    m_Color[i][0] = r;
    g = static_cast<TComponent>(vnl_sample_uniform(minimum, maximum));
    m_Color[i][1] = g;
    b = static_cast<TComponent>(vnl_sample_uniform(minimum, maximum));
    m_Color[i][2] = b;
    std::ostringstream name;
    name << "Random(" << std::fixed << std::setprecision(2) << static_cast<float>(r) << "," << static_cast<float>(g)
         << "," << static_cast<float>(b) << ")";
    m_ColorName[i] = name.str();
  }
}

template <typename TComponent>
bool
ColorTable<TComponent>::SetColor(unsigned int c, RGBPixel<TComponent> pixel, const char * name)
{
  return this->SetColor(c, pixel[0], pixel[1], pixel[2], name);
}

template <typename TComponent>
bool
ColorTable<TComponent>::SetColor(unsigned int c, TComponent r, TComponent g, TComponent b, const char * name)
{
  if (c < m_NumberOfColors)
  {
    m_Color[c][0] = r;
    m_Color[c][1] = g;
    m_Color[c][2] = b;
    m_ColorName[c] = name;
    return true;
  }
  return false;
}

template <typename TComponent>
RGBPixel<TComponent>
ColorTable<TComponent>::GetColor(unsigned int c)
{
  if (c < m_NumberOfColors)
  {
    return m_Color[c];
  }
  else
  {
    RGBPixel<TComponent> pixel;
    pixel.Set(0, 0, 0);
    return pixel;
  }
}

template <typename TComponent>
TComponent
ColorTable<TComponent>::GetColorComponent(unsigned int c, char rgb)
{
  if (c < m_NumberOfColors)
  {
    switch (rgb)
    {
      case 'r':
      {
        return m_Color[c][0];
      }
      case 'g':
      {
        return m_Color[c][1];
      }
      case 'b':
      {
        return m_Color[c][2];
      }
      default:
      {
        return 0;
      }
    }
  }
  else
  {
    return 0;
  }
}

template <typename TComponent>
std::string
ColorTable<TComponent>::GetColorName(unsigned int c)
{
  if (c < m_NumberOfColors)
  {
    return m_ColorName[c];
  }
  else
  {
    return "";
  }
}

template <typename TComponent>
unsigned int
ColorTable<TComponent>::GetClosestColorTableId(TComponent r, TComponent g, TComponent b)
{
  double       bestMatch = 0.0;
  unsigned int bestMatchColor = 0;

  for (unsigned int i = 0; i < m_NumberOfColors; ++i)
  {
    double match;
    match = (r - static_cast<double>(m_Color[i].GetRed())) * (r - static_cast<double>(m_Color[i].GetRed()));
    match += (g - static_cast<double>(m_Color[i].GetGreen())) * (g - static_cast<double>(m_Color[i].GetGreen()));
    match += (b - static_cast<double>(m_Color[i].GetGreen())) * (b - static_cast<double>(m_Color[i].GetBlue()));
    if (i == 0 || match < bestMatch)
    {
      bestMatch = match;
      bestMatchColor = i;
    }
  }
  return bestMatchColor;
}

template <typename TComponent>
void
ColorTable<TComponent>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfColors = " << m_NumberOfColors << std::endl;
  for (unsigned int i = 0; i < m_NumberOfColors; ++i)
  {
    os << indent << "ColorName[" << i << "] = " << m_ColorName[i] << ", "
       << "Color[" << i << "] = " << m_Color[i] << std::endl;
  }
}
} // namespace itk

#endif
