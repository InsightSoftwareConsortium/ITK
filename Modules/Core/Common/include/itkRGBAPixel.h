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
#ifndef itkRGBAPixel_h
#define itkRGBAPixel_h

// Undefine an eventual RGBAPixel macro
#ifdef RGBAPixel
#  undef RGBAPixel
#endif

#include "itkIndent.h"
#include "itkFixedArray.h"
#include "itkMath.h"

namespace itk
{
/** \class RGBAPixel
 * \brief Represent Red, Green, Blue and Alpha components for color images.
 *
 * This class is templated over the representation used for each
 * component.
 *
 * The following syntax for assigning an index is allowed/suggested:
 *
   \code
      RGBAPixel<float> pixel; pixel = 1.0f, 0.0f, .5f, .8;
      RGBAPixel<char> pixelArray[2];
      pixelArray[0] = 255, 255, 255, 230;
      pixelArray[1] = 255, 255, 244, 255;
   \endcode
 *
 * Since RGBAPixel is a subclass of Array, you can access its components as:
 * pixel[0], pixel[1], pixel[2], pixel[3]
 * \ingroup ImageObjects
 *
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/CommonTransparency,Make Part Of An Image Transparent}
 * \endsphinx
 */

template <typename TComponent = unsigned short>
class ITK_TEMPLATE_EXPORT RGBAPixel : public FixedArray<TComponent, 4>
{
public:
  /** Standard class type aliases. */
  using Self = RGBAPixel;
  using Superclass = FixedArray<TComponent, 4>;

  /** Dimension of the vector space. */
  static constexpr unsigned int Dimension = 4;

  /** Length of the pixel. */
  static constexpr unsigned int Length = 4;

  /** Convenience type alias. */
  using BaseArray = FixedArray<TComponent, 4>;

  /**  Define the component type. */
  using ComponentType = TComponent;
  using LuminanceType = typename NumericTraits<ComponentType>::RealType;

  /** Default constructors */
  RGBAPixel() { this->Fill(0); }
  RGBAPixel(const RGBAPixel &) = default;
  RGBAPixel &
  operator=(const RGBAPixel &) = default;
  RGBAPixel(RGBAPixel &&) = default;
  RGBAPixel &
  operator=(RGBAPixel &&) = default;
  ~RGBAPixel() = default;

  /** Pass-through constructor for the Array base class. */
  template <typename TRGBAPixelValueType>
  RGBAPixel(const RGBAPixel<TRGBAPixelValueType> & r)
    : BaseArray(r)
  {}
  RGBAPixel(const ComponentType r[4])
    : BaseArray(r)
  {}
  RGBAPixel(const ComponentType & r) { this->Fill(r); }

  /** Pass-through assignment operator for the Array base class. */
  RGBAPixel &
  operator=(const ComponentType r[4]);

  /** Aritmetic operations between pixels. Return a new RGBAPixel. */
  Self
  operator+(const Self & vec) const;
  Self
       operator-(const Self & vec) const;
  Self operator*(const ComponentType & f) const;
  Self
  operator/(const ComponentType & f) const;

  /** Arithmetic-assignment operators. */
  const Self &
  operator+=(const Self & vec);
  const Self &
  operator-=(const Self & vec);
  const Self &
  operator*=(const ComponentType & f);
  const Self &
  operator/=(const ComponentType & f);

  /** Implements strict weak ordering. For use in STL, e.g. std::map. */
  bool
  operator<(const Self & vec) const;

  bool
  operator==(const Self & vec) const;

  /** Return the number of components. */
  static unsigned int
  GetNumberOfComponents()
  {
    return 4;
  }

  /** Return the value for the Nth component. */
  ComponentType
  GetNthComponent(int c) const
  {
    return this->operator[](c);
  }

  /** Return the Euclidean norm of the vector defined by the RGB components. Alpha is not used. */
  ComponentType
  GetScalarValue() const
  {
    return static_cast<ComponentType>(
      std::sqrt(static_cast<double>(this->operator[](0)) * static_cast<double>(this->operator[](0)) +
                static_cast<double>(this->operator[](1)) * static_cast<double>(this->operator[](1)) +
                static_cast<double>(this->operator[](2)) * static_cast<double>(this->operator[](2))));
  }

  /** Set the Nth component to v. */
  void
  SetNthComponent(int c, const ComponentType & v)
  {
    this->operator[](c) = v;
  }

  /** Set the Red component. */
  void
  SetRed(ComponentType red)
  {
    this->operator[](0) = red;
  }

  /** Set the Green component. */
  void
  SetGreen(ComponentType green)
  {
    this->operator[](1) = green;
  }

  /** Set the Blue component. */
  void
  SetBlue(ComponentType blue)
  {
    this->operator[](2) = blue;
  }

  /** Set the Alpha component. */
  void
  SetAlpha(ComponentType alpha)
  {
    this->operator[](3) = alpha;
  }

  /** Set the four components. */
  void
  Set(ComponentType red, ComponentType green, ComponentType blue, ComponentType alpha)
  {
    this->operator[](0) = red;
    this->operator[](1) = green;
    this->operator[](2) = blue;
    this->operator[](3) = alpha;
  }

  /** Get the Red component. */
  const ComponentType &
  GetRed() const
  {
    return this->operator[](0);
  }

  /** Get the Green component. */
  const ComponentType &
  GetGreen() const
  {
    return this->operator[](1);
  }

  /** Get the Blue component. */
  const ComponentType &
  GetBlue() const
  {
    return this->operator[](2);
  }

  /** Get the Alpha component. */
  const ComponentType &
  GetAlpha() const
  {
    return this->operator[](3);
  }

  /** Get Luminance out of RGB */
  LuminanceType
  GetLuminance() const;
};

template <typename TComponent>
std::ostream &
operator<<(std::ostream & os, const RGBAPixel<TComponent> & c);

template <typename TComponent>
std::istream &
operator>>(std::istream & is, RGBAPixel<TComponent> & c);

template <typename T>
inline void
swap(RGBAPixel<T> & a, RGBAPixel<T> & b)
{
  a.swap(b);
}

} // end namespace itk

//
// Numeric traits must be included after (optionally) including the explicit
// instantiations control of this class, in case the implicit instantiation
// needs to be disabled.
//
// NumericTraits must be included before (optionally) including the .hxx file,
// in case the .hxx requires to use NumericTraits.
//
#include "itkNumericTraitsRGBAPixel.h"

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRGBAPixel.hxx"
#endif

#endif
