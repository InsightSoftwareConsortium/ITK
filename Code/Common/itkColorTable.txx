/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkColorTable.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkColorTable_txx
#define __itkColorTable_txx

#include "itkColorTable.h"
#include "vnl/vnl_sample.h"
#include <stdio.h>

namespace itk
{

template<class TPixel>
ColorTable<TPixel>
::ColorTable()
{
  m_NumberOfColors = 0;
  m_Color = NULL;
  m_ColorName = NULL;
}

template<class TPixel>
ColorTable<TPixel>
::~ColorTable()
{
  this->DeleteColors();
}

template<class TPixel>
void
ColorTable<TPixel>
::DeleteColors()
{
  if(m_Color != NULL)
    {
    delete [] m_Color;
    }
  m_Color = NULL;

  if(m_ColorName != NULL) 
    {
    unsigned int i;
    for(i=0; i < m_NumberOfColors; i++)
      {
      delete [] m_ColorName[i];
      }
    delete [] m_ColorName;
    }
  m_ColorName = NULL;
}

template<class TPixel>
void
ColorTable<TPixel>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "m_NumberOfColors = " << m_NumberOfColors << std::endl;
  os << indent << "m_Color = " << m_Color << std::endl;
  for (unsigned int i = 0; i < m_NumberOfColors; i++)
    {
    os << indent << "m_ColorName[" << i << "] = " 
       << m_ColorName[i] << std::endl;
    }
}

template<class TPixel>
void 
ColorTable<TPixel>
::UseDiscreteColors(void)
{
  this->DeleteColors();

  m_NumberOfColors = 8;
  m_Color = new RGBPixel<TPixel>[m_NumberOfColors];
  m_ColorName = new char*[m_NumberOfColors];
  for(unsigned int i=0; i<m_NumberOfColors; i++)
    {
    m_ColorName[i] = new char [80];
    }

  m_Color[0].Set((TPixel)0.9,(TPixel)0.0,(TPixel)0.0);
  sprintf(m_ColorName[0], "Red");

  m_Color[1].Set((TPixel)0.8,(TPixel)0.0,(TPixel)0.8);
  sprintf(m_ColorName[1], "Purple");

  m_Color[2].Set((TPixel)0.0,(TPixel)0.8,(TPixel)0.8);
  sprintf(m_ColorName[2], "Aqua");

  m_Color[3].Set((TPixel)0.8,(TPixel)0.8,(TPixel)0.0);
  sprintf(m_ColorName[3], "Yellow");

  m_Color[4].Set((TPixel)0.0,(TPixel)0.9,(TPixel)0.0);
  sprintf(m_ColorName[4], "Green");
    
  m_Color[5].Set((TPixel)0.0,(TPixel)0.0,(TPixel)0.9);
  sprintf(m_ColorName[5], "Blue");

  m_Color[6].Set((TPixel)0.7,(TPixel)0.7,(TPixel)0.7);
  sprintf(m_ColorName[6], "Grey0.70");

  m_Color[7].Set((TPixel)1.0,(TPixel)1.0,(TPixel)1.8);
  sprintf(m_ColorName[7], "White");
}

template<class TPixel>
void 
ColorTable<TPixel>
::UseGrayColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color = new RGBPixel<TPixel>[m_NumberOfColors];
  m_ColorName = new char * [m_NumberOfColors];
  for(i=0; i<m_NumberOfColors; i++)
    {
    m_ColorName[i] = new char [80];
    }

  typename NumericTraits<TPixel>::RealType range =
    NumericTraits<TPixel>::max() - NumericTraits<TPixel>::NonpositiveMin();
  typename NumericTraits<TPixel>::RealType delta = range / (n - 1);  
  TPixel gray;
  for(i=0; i<n; i++) 
    {
    gray = NumericTraits<TPixel>::NonpositiveMin()
      + static_cast<TPixel>(i * delta);
    m_Color[i].Set(gray, gray, gray);
    sprintf(m_ColorName[i], "Gray%.02f", static_cast<float>(gray));
    }
}

template<class TPixel>
void
ColorTable<TPixel>
::UseHeatColors(unsigned int n)
{
  unsigned int i;

  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color = new RGBPixel<TPixel>[m_NumberOfColors];
  m_ColorName = new char * [m_NumberOfColors];
  for(i=0; i<m_NumberOfColors; i++)
    {
    m_ColorName[i] = new char [80];
    }

  for(i=0; i<n/2.0; i++) 
    {
    m_Color[i].SetRed( (i+1)/(TPixel)(n/2.0+1) );
    m_Color[i].SetGreen( 0 );
    m_Color[i].SetBlue ( 0 );
    sprintf(m_ColorName[i], "Heat%.02f", i/(float)n);
    }

  for(i=0; i<n/2; i++) 
    {
    m_Color[(int)(i+n/2.0)].SetRed(static_cast<TPixel>(1));
    m_Color[(int)(i+n/2.0)].SetGreen((i+1)/(TPixel)(n/2.0+1));
    m_Color[(int)(i+n/2.0)].SetBlue((i+1)/(TPixel)(n/2.0+1));
    sprintf(m_ColorName[(int)(i+n/2.0)], "Heat%.02f", (i+n/2.0)/(float)n);
    }
}

template<class TPixel>
void
ColorTable<TPixel>
::UseRandomColors(unsigned int n)
{
  unsigned int i;
  this->DeleteColors();

  m_NumberOfColors = n;
  m_Color = new RGBPixel<TPixel>[m_NumberOfColors];
  m_ColorName = new char * [m_NumberOfColors];
  for(i=0; i<m_NumberOfColors; i++)
    {
    m_ColorName[i] = new char [80];
    }

  TPixel r, g, b;
  for(i=0; i<n; i++) 
    {
    r = static_cast<TPixel>(vnl_sample_uniform(NumericTraits<TPixel>
                           ::NonpositiveMin(), NumericTraits<TPixel>::max()));
    m_Color[i].SetRed( r );
    g = static_cast<TPixel>(vnl_sample_uniform(NumericTraits<TPixel>
                           ::NonpositiveMin(), NumericTraits<TPixel>::max()));
    m_Color[i].SetGreen( g );
    b = static_cast<TPixel>(vnl_sample_uniform(NumericTraits<TPixel>
                           ::NonpositiveMin(), NumericTraits<TPixel>::max()));
    m_Color[i].SetBlue( b );
    sprintf(m_ColorName[i], "Random(%.02f,%.02f,%.02f)",
            static_cast<float>(r),
            static_cast<float>(g),
            static_cast<float>(b));
    }
}

template<class TPixel>
unsigned int
ColorTable<TPixel>
::size(void)
{
  itkWarningMacro(<< "Call to itkColorTable.size() is being depreciated. " \
                  << "Use itkColorTable.GetNumberOfColors() instead.");
  return m_NumberOfColors;
}

template<class TPixel>
bool 
ColorTable<TPixel>
::SetColor(unsigned int c, TPixel r, TPixel g, TPixel b, const char *name)
{
  if(c < m_NumberOfColors)
    {
    m_Color[c].SetRed( r );
    m_Color[c].SetGreen( g );
    m_Color[c].SetBlue ( b );
    strcpy(m_ColorName[c], name);
    return true;
    }
  return false;
}

template<class TPixel>
RGBPixel<TPixel> * 
ColorTable<TPixel>
::GetColor(unsigned int c)
{
  if(c < m_NumberOfColors)
    {
    return & m_Color[c];
    }
  else
    {
    return NULL;
    }
}

template<class TPixel>
RGBPixel<TPixel>* 
ColorTable<TPixel>
::color(unsigned int c)
{
  itkWarningMacro(<< "Call to itkColorTable.color(unsigned int colorID) " \
               << "is being depreciated. " \
               << "Use itkColorTable.GetColor(unsigned int colorID) instead.");
  return this->GetColor(c);
}

template<class TPixel>
TPixel
ColorTable<TPixel>
::GetColorComponent(unsigned int c, char rgb)
{
  if(c < m_NumberOfColors)
    {
    switch(rgb) 
      {
      case 'r' : 
        {
        return m_Color[c].GetRed();
        }
      case 'g' : 
        {
        return m_Color[c].GetGreen();
        }
      case 'b' : 
        {
        return m_Color[c].GetBlue();
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

template<class TPixel>
TPixel
ColorTable<TPixel>
::color(unsigned int c, char rgb)
{
  itkWarningMacro(<< "Call to itkColorTable.color(unsigned int colorID, "
                  << "char rgb) is being depreciated. "
                  << "Use itkColorTable.GetColorComponent("
                  << "unsigned int colorID, char rgb) instead.");
  return this->GetColorComponent(c, rgb);
}

template<class TPixel>
char * 
ColorTable<TPixel>
::GetColorName(unsigned int c)
{
  if(c<m_NumberOfColors)
    {
    return m_ColorName[c];
    }
  else
    {
    return NULL;
    }
}

template<class TPixel>
char * 
ColorTable<TPixel>
::colorName(unsigned int c)
{
  itkWarningMacro(<< "Call to itkColorTable.colorName(unsigned int colorID)" \
                  << "is being depreciated." \
                  << "Use itkColorTable.GetColorName(unsigned int colorID)"
                  << "instead.");
  return this->GetColorName(c);
}

template<class TPixel>
unsigned int 
ColorTable<TPixel>
::GetClosestColorTableId(TPixel r, TPixel g, TPixel b)
{
  double match;
  double bestMatch = 0; 
  unsigned int bestMatchColor = 0;
  for(unsigned int i=0; i<m_NumberOfColors; i++)
    {
    match = (r - (double)m_Color[i].GetRed()) 
            * (r - (double)m_Color[i].GetRed());
    match += (g - (double)m_Color[i].GetGreen()) 
            * (g - (double)m_Color[i].GetGreen());
    match += (b - (double)m_Color[i].GetGreen()) 
            * (b - (double)m_Color[i].GetBlue());
    if(i == 0 || match < bestMatch)
      {
      bestMatch = match;
      bestMatchColor = i;
      }
    }
  return bestMatchColor;
}

} // namespace itk


#endif
