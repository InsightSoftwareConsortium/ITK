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
#ifndef itkCustomColormapFunction_h
#define itkCustomColormapFunction_h

#include "itkColormapFunction.h"

#include <vector>

namespace itk
{
namespace Function
{
/**
 * \class CustomColormapFunction
 * \brief Function object which maps a scalar value into an RGB colormap value.
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
class ITK_TEMPLATE_EXPORT CustomColormapFunction:
  public ColormapFunction< TScalar, TRGBPixel >
{
public:

  typedef CustomColormapFunction                 Self;
  typedef ColormapFunction< TScalar, TRGBPixel > Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::RGBPixelType RGBPixelType;
  typedef typename Superclass::ScalarType   ScalarType;
  typedef typename Superclass::RealType     RealType;

  typedef std::vector< RealType > ChannelType;

  virtual RGBPixelType operator()(const TScalar &) const ITK_OVERRIDE;

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
  CustomColormapFunction() {}
  ~CustomColormapFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CustomColormapFunction);

  ChannelType m_RedChannel;
  ChannelType m_GreenChannel;
  ChannelType m_BlueChannel;
};
} // end namespace functor
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCustomColormapFunction.hxx"
#endif

#endif
