/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBlueColormapFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBlueColormapFunctor_h
#define __itkBlueColormapFunctor_h

#include "itkColormapFunctor.h"

namespace itk
{
namespace Functor
{
/**
 * \class BlueColormapFunctor
 * \brief Function object which maps a scalar value into an RGB colormap value.
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
class ITK_EXPORT BlueColormapFunctor:
  public ColormapFunctor< TScalar, TRGBPixel >
{
public:

  typedef BlueColormapFunctor                   Self;
  typedef ColormapFunctor< TScalar, TRGBPixel > Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::RGBPixelType RGBPixelType;
  typedef typename Superclass::ScalarType   ScalarType;
  typedef typename Superclass::RealType     RealType;

  virtual RGBPixelType operator()(const TScalar &) const;

protected:
  BlueColormapFunctor() {}
  ~BlueColormapFunctor() {}
private:
  BlueColormapFunctor(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented
};
} // end namespace functor
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBlueColormapFunctor.txx"
#endif

#endif
