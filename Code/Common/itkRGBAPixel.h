/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBAPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRGBAPixel_h
#define __itkRGBAPixel_h

// Undefine an eventual RGBAPixel macro
#ifdef RGBAPixel
#undef RGBAPixel
#endif

#include <itkIndent.h>
#include <itkFixedArray.h>
#include "vnl/vnl_math.h"

namespace itk
{

/** \class RGBAPixel
 * \brief Represent Red, Green, Blue cand Alpha component for color images
 *
 * This class is templated over the representation used for each
 * component. 
 *
 * The following syntax for assigning an index is allowed/suggested:
 *
 *    RGBAPixel<float> pixel; pixel = 1.0f, 0.0f, .5f, .8;
 *    RGBAPixel<char> pixelArray[2];
 *    pixelArray[0] = 255, 255, 255, 230;
 *    pixelArray[1] = 255, 255, 244, 255;
 *
 * Since RGBAPixel is a subclass of Array, you can access its components as:
 * pixel[0], pixel[1], pixel[2], pixel[3]
 * \ingroup ImageObjects
 *
 */

template < typename TComponent = unsigned short >
class RGBAPixel: public FixedArray<TComponent,4>
{
public:
  /** Standard class typedefs. */
  typedef RGBAPixel  Self;
  typedef FixedArray<TComponent, 4> Super;
    
  /** Convenience typedefs. */
  typedef FixedArray<TComponent, 4> BaseArray;
  
  /**  Define the component type. */
  typedef TComponent ComponentType;

  /** Default constructor has nothing to do. */
  RGBAPixel() {Fill(0);}
  RGBAPixel (const ComponentType& r)
  { Fill(r) ;}
  
  /** Pass-through constructor for the Array base class. */
  RGBAPixel(const Self& r): BaseArray(r) {}
  RGBAPixel(const ComponentType  r[4]): BaseArray(r) {}  
    
  /** Pass-through assignment operator for the Array base class. */
  RGBAPixel& operator= (const Self& r);
  RGBAPixel& operator= (const ComponentType r[4]);
  
  /** Return the number of componentsxquery-rep. */
  static int GetNumberOfComponents(){ return 4;}

  /** Return the value for the Nth component. */
  ComponentType GetNthComponent(int c) const
    { return this->operator[](c); }

  /** Return the value for the Nth component. */
  ComponentType GetScalarValue() const
    {
      return static_cast<ComponentType> (vnl_math_sqrt(
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

  /** Set the Alpha component. */
  void SetAlpha( ComponentType alpha ) {this->operator[](3) = alpha;}

  /** Set the four components. */
  void Set( ComponentType red, ComponentType green, ComponentType blue, ComponentType alpha )
    { this->operator[](0) = red; this->operator[](1) = green; this->operator[](2) = blue; this->operator[](3) = alpha;}

  /** Get the Red component. */
  const ComponentType & GetRed( void ) const { return this->operator[](0);}

  /** Get the Green component. */
  const ComponentType & GetGreen( void ) const { return this->operator[](1);}

  /** Get the Blue component. */
  const ComponentType & GetBlue( void ) const { return this->operator[](2);}

  /** Get the Alpha component. */
  const ComponentType & GetAlpha( void ) const { return this->operator[](3);}
};


template< typename TComponent  >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
                                    const RGBAPixel<TComponent> & c); 

template< typename TComponent  >  
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                          RGBAPixel<TComponent> & c); 

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBAPixel.txx"
#endif

#endif
