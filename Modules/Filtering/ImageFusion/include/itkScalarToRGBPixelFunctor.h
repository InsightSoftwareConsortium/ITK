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
#ifndef itkScalarToRGBPixelFunctor_h
#define itkScalarToRGBPixelFunctor_h

#include "itkRGBPixel.h"

namespace itk
{
namespace Functor
{
/**
 * \class ScalarToRGBPixelFunctor
 * \brief A Function object which maps a scalar value into an RGB pixel value.
 *
 *  This class is useful for visualizing labeled images which cannot be mapped
 *  succefully into grayscale images.  Images of unsigned long integers, for
 *  example, may have too many graylevels to visualize effectively.
 *
 *  The hashing scheme used is designed to spread close scalar values of very
 *  different colors by using the least significant bits (fastest changing) of
 *  the scalar type to determine the color.  Because labeled images may
 *  typically use sequential values, it is desirable that those values result
 *  in easily discernable colors.
 *
 * \ingroup ITKImageFusion
 */
template< typename TScalar >
class ITK_TEMPLATE_EXPORT ScalarToRGBPixelFunctor
{
public:
  ScalarToRGBPixelFunctor();
  ~ScalarToRGBPixelFunctor() {}

  typedef unsigned char                RGBComponentType;
  typedef RGBPixel< RGBComponentType > RGBPixelType;
  typedef TScalar                      ScalarType;

  RGBPixelType operator()(const TScalar &) const;

  void SetLittleEndian()
  {
    m_UseMSBForHashing = false;
  }

  void SetBigEndian()
  {
    m_UseMSBForHashing = true;
  }

  void SetUseMSBForHashing(bool value)
  {
    m_UseMSBForHashing = value;
  }

  bool GetUseMSBForHashing() const
  {
    return m_UseMSBForHashing;
  }

  void UseMSBForHashingOn()
  {
    m_UseMSBForHashing = true;
  }

  void UseMSBForHashingOff()
  {
    m_UseMSBForHashing = false;
  }


private:
  bool          m_UseMSBForHashing;
  unsigned int  m_ColorIndex[3];
};
} // end namespace functor
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarToRGBPixelFunctor.hxx"
#endif

#endif
