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
#ifndef itkHSVColormapFunction_h
#define itkHSVColormapFunction_h

#include "itkColormapFunction.h"

namespace itk
{
namespace Function
{
/**
 * \class HSVColormapFunction
 * \brief Function object which maps a scalar value into an RGB colormap value.
 *
 * \image html HSVColormapFunction.png "HSV colormap."
 *
 * \author Nicholas Tustison, Hui Zhang, Gaetan Lehmann, Paul Yushkevich
 * and James C. Gee
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * http://www.insight-journal.org/browse/publication/285
 * https://hdl.handle.net/1926/1452
 *
 * \ingroup ITKColormap
 */
template< typename TScalar, typename TRGBPixel >
class ITK_TEMPLATE_EXPORT HSVColormapFunction:
  public ColormapFunction< TScalar, TRGBPixel >
{
public:

  typedef HSVColormapFunction                    Self;
  typedef ColormapFunction< TScalar, TRGBPixel > Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::RGBPixelType RGBPixelType;
  typedef typename Superclass::ScalarType   ScalarType;
  typedef typename Superclass::RealType     RealType;

  virtual RGBPixelType operator()(const TScalar &) const ITK_OVERRIDE;

protected:
  HSVColormapFunction() {}
  ~HSVColormapFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HSVColormapFunction);
};
} // end namespace functor
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHSVColormapFunction.hxx"
#endif

#endif
