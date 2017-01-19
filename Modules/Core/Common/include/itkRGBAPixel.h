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
#ifndef itkRGBAPixel_h
#define itkRGBAPixel_h

// Undefine an eventual RGBAPixel macro
#ifdef RGBAPixel
#undef RGBAPixel
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
 * \code
 *    RGBAPixel<float> pixel; pixel = 1.0f, 0.0f, .5f, .8;
 *    RGBAPixel<char> pixelArray[2];
 *    pixelArray[0] = 255, 255, 255, 230;
 *    pixelArray[1] = 255, 255, 244, 255;
 * \endcode
 *
 * Since RGBAPixel is a subclass of Array, you can access its components as:
 * pixel[0], pixel[1], pixel[2], pixel[3]
 * \ingroup ImageObjects
 *
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{SimpleOperations/Transparency,Make part of an image transparent}
 * \endwiki
 */

template< typename TComponent = unsigned short >
class ITK_TEMPLATE_EXPORT RGBAPixel:public FixedArray< TComponent, 4 >
{
public:
  /** Standard class typedefs. */
  typedef RGBAPixel                   Self;
  typedef FixedArray< TComponent, 4 > Superclass;

  /** Dimension of the vector space. */
  itkStaticConstMacro(Dimension, unsigned int, 4);

  /** Length of the pixel. */
  itkStaticConstMacro(Length, unsigned int, 4);

  /** Convenience typedefs. */
  typedef FixedArray< TComponent, 4 > BaseArray;

  /**  Define the component type. */
  typedef TComponent                                        ComponentType;
  typedef typename NumericTraits< ComponentType >::RealType LuminanceType;

  /** Default constructor has nothing to do. */
  RGBAPixel() { this->Fill(0); }
  RGBAPixel (const ComponentType & r) { this->Fill(r); }

  /** Pass-through constructor for the Array base class. */
  template< typename TRGBAPixelValueType >
  RGBAPixel(const RGBAPixel< TRGBAPixelValueType > & r):BaseArray(r) {}
  RGBAPixel(const ComponentType r[4]):BaseArray(r) {}

  /** Pass-through assignment operator for the Array base class. */
  RGBAPixel & operator=(const Self & r);

  RGBAPixel & operator=(const ComponentType r[4]);

  /** Aritmetic operations between pixels. Return a new RGBPixel. */
  Self operator+(const Self & vec) const;

  Self operator-(const Self & vec) const;

  const Self & operator+=(const Self & vec);

  const Self & operator-=(const Self & vec);

  Self operator *(const ComponentType & f) const;

  bool operator==(const Self & vec) const;

  /** Return the number of componentsxquery-rep. */
  static unsigned int GetNumberOfComponents() { return 4; }

  /** Return the value for the Nth component. */
  ComponentType GetNthComponent(int c) const { return this->operator[](c); }

  /** Return the Euclidean norm of the vector defined by the RGB components. Alpha is not used. */
  ComponentType GetScalarValue() const
  {
    return static_cast< ComponentType >( std::sqrt(
                                           static_cast< double >( this->operator[](0) )
                                           * static_cast< double >( this->operator[](0) )
                                           + static_cast< double >( this->operator[](1) )
                                           * static_cast< double >( this->operator[](1) )
                                           + static_cast< double >( this->operator[](2) )
                                           * static_cast< double >( this->operator[](2) ) ) );
  }

  /** Set the Nth component to v. */
  void SetNthComponent(int c, const ComponentType & v) {  this->operator[](c) = v; }

  /** Set the Red component. */
  void SetRed(ComponentType red) { this->operator[](0) = red; }

  /** Set the Green component. */
  void SetGreen(ComponentType green) { this->operator[](1) = green; }

  /** Set the Blue component. */
  void SetBlue(ComponentType blue) { this->operator[](2) = blue; }

  /** Set the Alpha component. */
  void SetAlpha(ComponentType alpha) { this->operator[](3) = alpha; }

  /** Set the four components. */
  void Set(ComponentType red, ComponentType green, ComponentType blue, ComponentType alpha)
  {
    this->operator[](0) = red;
    this->operator[](1) = green;
    this->operator[](2) = blue;
    this->operator[](3) = alpha;
  }

  /** Get the Red component. */
  const ComponentType & GetRed(void) const { return this->operator[](0); }

  /** Get the Green component. */
  const ComponentType & GetGreen(void) const { return this->operator[](1); }

  /** Get the Blue component. */
  const ComponentType & GetBlue(void) const { return this->operator[](2); }

  /** Get the Alpha component. */
  const ComponentType & GetAlpha(void) const { return this->operator[](3); }

  /** Get Luminance out of RGB */
  LuminanceType GetLuminance() const;
};

template< typename TComponent  >
std::ostream & operator<<(std::ostream & os,
                                     const RGBAPixel< TComponent > & c);

template< typename TComponent  >
std::istream & operator>>(std::istream & is,
                                     RGBAPixel< TComponent > & c);
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
#include "itkRGBAPixel.hxx"
#endif

#endif
