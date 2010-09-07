/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelOverlayFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLabelOverlayFunctor_h
#define __itkLabelOverlayFunctor_h

#include "itkLabelToRGBFunctor.h"

namespace itk
{
namespace Functor
{
/** \class LabelOverlayFunctor
 *  \brief Functor for applying a colormap to a label image and combine it
 * with a grayscale image
 *
 * This functor class used internally by LabelOverlayImageFilter
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa LabelOverlayImageFilter LabelToRGBFunctor
 *
 */
template< class TInputPixel, class TLabel, class TRGBPixel >
class LabelOverlayFunctor
{
public:
  LabelOverlayFunctor()
  {
    // provide some default value for external use (outside
    // LabelOverlayFunctorImageFilter) Inside LabelOverlayFunctorImageFilter,
    // the values are always initialized
    m_BackgroundValue = NumericTraits< TLabel >::Zero;
  }

  inline TRGBPixel operator()(const TInputPixel & p1, const TLabel & p2) const
  {
    TRGBPixel rgbPixel;

    if ( p2 == m_BackgroundValue )
      {
      // value is background
      // return a gray pixel with the same intensity than the input pixel
      typename TRGBPixel::ValueType p =
        static_cast< typename TRGBPixel::ValueType >( p1 );
      rgbPixel[0] = p;
      rgbPixel[1] = p;
      rgbPixel[2] = p;
      return rgbPixel;
      }

    // taint the input pixel with the colored one returned by
    // the color functor.
    TRGBPixel opaque = m_RGBFunctor(p2);
    for ( unsigned int i = 0; i < 3; i++ )
      {
      rgbPixel[i] = static_cast< typename TRGBPixel::ValueType >(
        opaque[i] * m_Opacity + p1 * ( 1.0 - m_Opacity ) );
      }
    return rgbPixel;
  }

  bool operator!=(const LabelOverlayFunctor & l) const
  {
    bool value = l.m_Opacity != m_Opacity
                 || m_BackgroundValue != l.m_BackgroundValue;

    return value;
  }

  ~LabelOverlayFunctor() {}

  void SetOpacity(double opacity)
  {
    m_Opacity = opacity;
  }

  void SetBackgroundValue(TLabel v)
  {
    m_BackgroundValue = v;
    m_RGBFunctor.SetBackgroundValue(v);
  }

  void ResetColors()
  {
    m_RGBFunctor.ResetColors();
  }

  unsigned int GetNumberOfColors() const
  {
    return m_RGBFunctor.GetNumberOfColors();
  }

  /** type of the color component */
  typedef typename TRGBPixel::ComponentType ComponentType;

  void AddColor(ComponentType r, ComponentType g, ComponentType b)
  {
    m_RGBFunctor.AddColor(r, g, b);
  }

protected:
private:
  double m_Opacity;
  TLabel m_BackgroundValue;

  typename Functor::LabelToRGBFunctor< TLabel, TRGBPixel > m_RGBFunctor;
};
}  // end namespace functor
}  // end namespace itk

#endif
