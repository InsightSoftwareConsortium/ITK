/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRGBPixel_h
#define __itkRGBPixel_h

// Undefine an eventual RGBPixel macro
#ifdef RGBPixel
#undef RGBPixel
#endif

#include <itkIndent.h>
#include <itkFixedArray.h>
#include "vnl/vnl_math.h"

namespace itk
{

/** \class RGBPixel
 * \brief Represent Red, Green and Blue component for color images
 *
 * This class is templated over the representation used for each
 * component. 
 *
 * The following syntax for assigning an index is allowed/suggested:
 *
 *    RGBPixel<float> pixel; pixel = 1.0f, 0.0f, .5f;
 *    RGBPixel<char> pixelArray[2];
 *    pixelArray[0] = 255, 255, 255;
 *    pixelArray[1] = 255, 255, 244;
 *
 * Since RGBPixel is a subclass of Array, you can access its components as:
 * pixel[0], pixel[1], pixel[2]
 * \ingroup ImageObjects
 */

template < typename TComponent = unsigned short >
class RGBPixel: public FixedArray<TComponent,3>
{
public:
  /** Standard class typedefs. */
  typedef RGBPixel  Self;
  typedef FixedArray<TComponent, 3> SuperClass;
  
  /** Dimension of the vector space. */
  itkStaticConstMacro(Dimension, unsigned int, 3);

  /** Convenience typedefs. */
  typedef FixedArray<TComponent, 3> BaseArray;
  
  /**  Define the component type. */
  typedef TComponent ComponentType;
  typedef typename SuperClass::ValueType ValueType;
  
  /** Default constructor has nothing to do. */
  RGBPixel() {Fill(0);}
  RGBPixel (const ComponentType& r);
  
  /** Pass-through constructor for the Array base class. */
  RGBPixel(const Self& r): BaseArray(r) {}
  RGBPixel(const ComponentType  r[3]): BaseArray(r) {}  
    
  /** Pass-through assignment operator for the Array base class. */
  Self& operator= (const Self& r);
  Self& operator= (const ComponentType r[3]);

  /** Aritmetic operations between pixels. Return a new RGBPixel. */
  Self operator+(const Self &vec) const;
  Self operator-(const Self &vec) const;
  const Self & operator+=(const Self &vec);
  const Self & operator-=(const Self &vec);
  Self operator*(const ComponentType &f) const;

 
  /** Return the number of components. */
  static int GetNumberOfComponents(){ return 3;}

  /** Return the value for the Nth component. */
  ComponentType GetNthComponent(int c) const
    { return this->operator[](c); }

  /** Return the value for the Nth component. */
  ComponentType GetScalarValue() const
    {
      return static_cast<ComponentType> (vcl_sqrt(
        static_cast<double>(this->operator[](0)) * static_cast<double>(this->operator[](0)) +
        static_cast<double>(this->operator[](1)) * static_cast<double>(this->operator[](1)) +
        static_cast<double>(this->operator[](2)) * static_cast<double>(this->operator[](2)))); 
    }

  /** Set the Nth component to v. */
  void SetNthComponent(int c, const ComponentType& v)  
    {  this->operator[](c) = v; }

  /** Set the Red component. */
  void SetRed( ComponentType red ) { this->operator[](0) = red;}

  /** Set the Green component. */
  void SetGreen( ComponentType green ) {this->operator[](1) = green;}

  /** Set the Blue component. */
  void SetBlue( ComponentType blue ) {this->operator[](2) = blue;}

  /** Set the three components. */
  void Set( ComponentType red, ComponentType green, ComponentType blue )
    { this->operator[](0) = red; this->operator[](1) = green; this->operator[](2) = blue;}

  /** Get the Red component. */
  const ComponentType & GetRed( void ) const { return this->operator[](0);}

  /** Get the Green component. */
  const ComponentType & GetGreen( void ) const { return this->operator[](1);}

  /** Get the Blue component. */
  const ComponentType & GetBlue( void ) const { return this->operator[](2);}
};


template< typename TComponent  >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
                                    const RGBPixel<TComponent> & c); 
template< typename TComponent  >  
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                          RGBPixel<TComponent> & c); 

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBPixel.txx"
#endif

#endif
