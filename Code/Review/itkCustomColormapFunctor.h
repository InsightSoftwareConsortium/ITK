/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCustomColormapFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCustomColormapFunctor_h
#define __itkCustomColormapFunctor_h

#include "itkColormapFunctor.h"

#include <vector>

namespace itk
{
namespace Functor
{
/**
 * \class CustomColormapFunctor
 * \brief Function object which maps a scalar value into an RGB colormap value.
 *
 *
 * \author Nicholas Tustison, Hui Zhang, Gaetan Lehmann, Paul Yushkevich and James C. Gee
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * http://www.insight-journal.org/browse/publication/285
 * http://hdl.handle.net/1926/1452
 *
 */
template< class TScalar, class TRGBPixel >
class ITK_EXPORT CustomColormapFunctor:
  public ColormapFunctor< TScalar, TRGBPixel >
{
public:

  typedef CustomColormapFunctor                 Self;
  typedef ColormapFunctor< TScalar, TRGBPixel > Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::RGBPixelType RGBPixelType;
  typedef typename Superclass::ScalarType   ScalarType;
  typedef typename Superclass::RealType     RealType;

  typedef std::vector< RealType > ChannelType;

  virtual RGBPixelType operator()(const TScalar &) const;

  void SetRedChannel(ChannelType red)
  {
    m_RedChannel = red;
  }

  ChannelType GetRedChannel() const
  {
    return m_RedChannel;
  }

  void SetGreenChannel(ChannelType green)
  {
    m_GreenChannel = green;
  }

  ChannelType GetGreenChannel() const
  {
    return m_GreenChannel;
  }

  void SetBlueChannel(ChannelType blue)
  {
    m_BlueChannel = blue;
  }

  ChannelType GetBlueChannel() const
  {
    return m_BlueChannel;
  }

protected:
  CustomColormapFunctor() {}
  ~CustomColormapFunctor() {}
private:
  CustomColormapFunctor(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented

  ChannelType m_RedChannel;
  ChannelType m_GreenChannel;
  ChannelType m_BlueChannel;
};
} // end namespace functor
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCustomColormapFunctor.txx"
#endif

#endif
