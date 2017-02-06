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
#ifndef itkColorTable_hxx
#define itkColorTable_hxx

#include "itkColorTable.h"
#include "itkNumericTraits.h"
#include "vnl/vnl_sample.h"

#include <sstream>
#include <iomanip>

namespace itk
{
template< typename TPixel >
ColorTable< TPixel >
::ColorTable() :
  m_NumberOfColors( 0 )
{
}

template< typename TPixel >
void
ColorTable< TPixel >
::DeleteColors()
{
  m_Color.resize(0);
  m_ColorName.resize(0);
}

template< typename TPixel >
void
ColorTable< TPixel >
::UseDiscreteColors(void)
{
  this->DeleteColors();

  m_NumberOfColors = 8;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);

  typename NumericTraits< TPixel >::RealType scale;
  typename NumericTraits< TPixel >::RealType shift;
  if (NumericTraits<TPixel>::is_integer)
    {
    scale =
      static_cast< typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::max()) -
      static_cast<  typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::NonpositiveMin());
    shift =
      static_cast<  typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::NonpositiveMin());
    }
  else
    {
    scale = NumericTraits< TPixel >::OneValue();
    shift = NumericTraits< TPixel >::ZeroValue();
    }

  m_Color[0].Set((TPixel)(0.9 * scale + shift),
                 (TPixel)(              shift),
                 (TPixel)(              shift) );
  m_ColorName[0] = "Red";

  m_Color[1].Set((TPixel)(0.8 * scale + shift),
                 (TPixel)(              shift),
                 (TPixel)(0.8 * scale + shift));
  m_ColorName[1] = "Purple";

  m_Color[2].Set((TPixel)(              shift),
                 (TPixel)(0.8 * scale + shift),
                 (TPixel)(0.8 * scale + shift));
  m_ColorName[2] = "Aqua";

  m_Color[3].Set((TPixel)(0.8 * scale + shift),
                 (TPixel)(0.8 * scale + shift),
                 (TPixel)(              shift));
  m_ColorName[3] = "Yellow";

  m_Color[4].Set((TPixel)(              shift),
                 (TPixel)(0.9 * scale + shift),
                 (TPixel)(              shift));
  m_ColorName[4] = "Green";

  m_Color[5].Set((TPixel)(              shift),
                 (TPixel)(              shift),
                 (TPixel)(0.9 * scale + shift));
  m_ColorName[5] = "Blue";

  m_Color[6].Set((TPixel)(0.7 * scale + shift),
                 (TPixel)(0.7 * scale + shift),
                 (TPixel)(0.7 * scale + shift));
  m_ColorName[6] = "Grey0.70";
  //
  // to avoid numeric exception, need to make
  // sure that the value assigned is clamped at
  // max for TPixel.  Exceptions were happening
  // on this assignment, even if realMax was
  // set to NumericTraits<TPixel>::max().
  typename NumericTraits< TPixel >::RealType
    realMax(1.0 * scale + shift);
  TPixel pixelMax(NumericTraits< TPixel >::max());
  if(realMax < NumericTraits< TPixel >::max())
    {
    pixelMax = static_cast< TPixel >(realMax);
    }
  m_Color[7].Set(pixelMax,pixelMax,pixelMax);
  m_ColorName[7] = "White";
}

template< typename TPixel >
void
ColorTable< TPixel >
::UseGrayColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);

  typename NumericTraits< TPixel >::RealType range;
  typename NumericTraits< TPixel >::RealType minimum;
  if (NumericTraits<TPixel>::is_integer)
    {
    range =
      static_cast< typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::max()) -
      static_cast<  typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::NonpositiveMin());
    minimum = NumericTraits< TPixel >::NonpositiveMin();
    }
  else
    {
    range = NumericTraits< TPixel >::OneValue();
    minimum = NumericTraits< TPixel >::ZeroValue();
    }
  typename NumericTraits< TPixel >::RealType delta;
  if (m_NumberOfColors > 1)
    {
    delta = range / ( m_NumberOfColors - 1 );
    }
  else
    {
    delta = 0.0;
    }

  for ( i = 0; i < m_NumberOfColors; i++ )
    {
    typename NumericTraits< TPixel >::RealType
      realGray( minimum + i * delta );

    TPixel gray = NumericTraits< TPixel >::max();
    if( realGray < NumericTraits< TPixel >::max() )
      {
      gray = static_cast< TPixel >(realGray);
      }

    m_Color[i].Set(gray, gray, gray);
    std::ostringstream name;
    name << "Gray" << std::fixed << std::setprecision(2)
         << static_cast<float>(gray);
    m_ColorName[i] = name.str();
    }
}

template< typename TPixel >
void
ColorTable< TPixel >
::UseHeatColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);

  typename NumericTraits< TPixel >::RealType scale;
  typename NumericTraits< TPixel >::RealType shift;
  if (NumericTraits<TPixel>::is_integer)
    {
    scale =
      static_cast< typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::max()) -
      static_cast<  typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::NonpositiveMin());
    shift =
      static_cast<  typename NumericTraits< TPixel >::RealType>(NumericTraits< TPixel >::NonpositiveMin());
    }
  else
    {
    scale = NumericTraits< TPixel >::OneValue();
    shift = NumericTraits< TPixel >::ZeroValue();
    }
  for ( i = 0; i < n / 2.0; i++ )
    {
    //
    // avoid overflow
    typename NumericTraits < TPixel >::RealType
      realR((( i + 1 ) / ( n / 2.0 + 1 ) ) * scale + shift);
    TPixel r(NumericTraits< TPixel >::max());
    if(realR < NumericTraits< TPixel >::max())
      {
      r = static_cast< TPixel >(realR);
      }
    TPixel g(static_cast<TPixel>( shift));
    TPixel b(static_cast<TPixel>( shift));
    m_Color[i].Set(r,g,b);
    std::ostringstream name;
    name << "Heat" << std::fixed << std::setprecision(2)
         << i / static_cast<float>(n);
    m_ColorName[i] = name.str();
    }

  for ( i = 0; i < n / 2; i++ )
    {
    typename NumericTraits< TPixel >::RealType
      rdouble(1.0 * scale + shift);
    TPixel r(NumericTraits<TPixel>::max());
    if( rdouble < NumericTraits<TPixel>::max() )
      {
      r = static_cast<TPixel>(rdouble);
      }
    TPixel g = static_cast<TPixel>((( i + 1 ) / ( n / 2.0 + 1 )) * scale + shift);
    TPixel b = static_cast<TPixel>((( i + 1 ) / ( n / 2.0 + 1 )) * scale + shift);
    m_Color[(size_t)(i + n / 2.0 )].Set(r,g,b);
    std::ostringstream name;
    name << "Heat" << std::fixed << std::setprecision(2)
         << ( i + n / 2.0 ) / (float)n;
    m_ColorName[static_cast<size_t>(( i + n / 2.0 ))] = name.str();
    }
}

template< typename TPixel >
void
ColorTable< TPixel >
::UseRandomColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color.resize(m_NumberOfColors);
  m_ColorName.resize(m_NumberOfColors);
  TPixel r, g, b;
  TPixel minimum, maximum;
  if (NumericTraits<TPixel>::is_integer)
    {
    minimum = NumericTraits< TPixel >::NonpositiveMin();
    maximum = NumericTraits< TPixel >::max();
    }
  else
    {
    minimum = NumericTraits< TPixel >::ZeroValue();
    maximum  = NumericTraits< TPixel >::OneValue();
    }
  for ( i = 0; i < n; i++ )
    {
      r = static_cast< TPixel >( vnl_sample_uniform( minimum, maximum));
    m_Color[i][0] = r;
    g = static_cast< TPixel >( vnl_sample_uniform( minimum, maximum));
    m_Color[i][1] = g;
    b = static_cast< TPixel >( vnl_sample_uniform( minimum, maximum));
    m_Color[i][2] = b;
    std::ostringstream name;
    name << "Random(" << std::fixed << std::setprecision(2)
         << static_cast< float >( r ) << ","
         << static_cast< float >( g ) << ","
         << static_cast< float >( b ) << ")";
    m_ColorName[i] = name.str();
    }
}

template< typename TPixel >
bool
ColorTable< TPixel >
::SetColor(unsigned int c, RGBPixel<TPixel> pixel, const char *name)
{
  return this->SetColor(c, pixel[0], pixel[1], pixel[2], name);
}

template< typename TPixel >
bool
ColorTable< TPixel >
::SetColor(unsigned int c, TPixel r, TPixel g, TPixel b, const char *name)
{
  if ( c < m_NumberOfColors )
    {
    m_Color[c][0] = r;
    m_Color[c][1] = g;
    m_Color[c][2] = b;
    m_ColorName[c] = name;
    return true;
    }
  return false;
}

template< typename TPixel >
RGBPixel< TPixel >
ColorTable< TPixel >
::GetColor(unsigned int c)
{
  if ( c < m_NumberOfColors )
    {
    return m_Color[c];
    }
  else
    {
    RGBPixel<TPixel> pixel;
    pixel.Set(0, 0, 0);
    return pixel;
    }
}

template< typename TPixel >
TPixel
ColorTable< TPixel >
::GetColorComponent(unsigned int c, char rgb)
{
  if ( c < m_NumberOfColors )
    {
    switch ( rgb )
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

template< typename TPixel >
std::string
ColorTable< TPixel >
::GetColorName(unsigned int c)
{
  if ( c < m_NumberOfColors )
    {
    return m_ColorName[c];
    }
  else
    {
    return "";
    }
}

template< typename TPixel >
unsigned int
ColorTable< TPixel >
::GetClosestColorTableId(TPixel r, TPixel g, TPixel b)
{
  double       bestMatch = 0.0;
  unsigned int bestMatchColor = 0;

  for ( unsigned int i = 0; i < m_NumberOfColors; i++ )
    {
    double match;
    match = ( r - (double)m_Color[i].GetRed() )
            * ( r - (double)m_Color[i].GetRed() );
    match += ( g - (double)m_Color[i].GetGreen() )
             * ( g - (double)m_Color[i].GetGreen() );
    match += ( b - (double)m_Color[i].GetGreen() )
             * ( b - (double)m_Color[i].GetBlue() );
    if ( i == 0 || match < bestMatch )
      {
      bestMatch = match;
      bestMatchColor = i;
      }
    }
  return bestMatchColor;
}

template< typename TPixel >
void
ColorTable< TPixel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfColors = " << m_NumberOfColors << std::endl;
  for ( unsigned int i = 0; i < m_NumberOfColors; i++ )
    {
    os << indent
       << "ColorName[" << i << "] = " << m_ColorName[i] << ", "
       << "Color[" << i << "] = " << m_Color[i] << std::endl;
    }
}
} // namespace itk

#endif
