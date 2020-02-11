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
#ifndef itkBlueColormapFunction_h
#define itkBlueColormapFunction_h

#include "itkColormapFunction.h"

namespace itk
{
namespace Function
{
/**
 * \class BlueColormapFunction
 * \brief Function object which maps a scalar value into an RGB colormap value.
 *
 * \author Nicholas Tustison, Hui Zhang, Gaetan Lehmann, Paul Yushkevich
 * and James C. Gee
 *
 * \image html BlueColormapFunction.png "Blue colormap."
 * This code was contributed in the Insight Journal paper:
 *
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * http://www.insight-journal.org/browse/publication/285
 * https://hdl.handle.net/1926/1452
 *
 * \ingroup ITKColormap
 */
template <typename TScalar, typename TRGBPixel>
class ITK_TEMPLATE_EXPORT BlueColormapFunction : public ColormapFunction<TScalar, TRGBPixel>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BlueColormapFunction);

  using Self = BlueColormapFunction;
  using Superclass = ColormapFunction<TScalar, TRGBPixel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  using RGBPixelType = typename Superclass::RGBPixelType;
  using ScalarType = typename Superclass::ScalarType;
  using RealType = typename Superclass::RealType;

  RGBPixelType
  operator()(const TScalar &) const override;

protected:
  BlueColormapFunction() = default;
  ~BlueColormapFunction() override = default;
};
} // end namespace Function
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBlueColormapFunction.hxx"
#endif

#endif
