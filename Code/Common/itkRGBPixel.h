/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkRGBPixel_h
#define __itkRGBPixel_h

// Undefine an eventual RGBPixel macro
#ifdef RGBPixel
#undef RGBPixel
#endif

#include <itkIndent.h>

namespace itk
{

/** \class RGBPixel
 * \brief Represent Red, Green and Blue component for color images
 *
 * This class is templated over the representation used for each
 * component. 
 *
 * For efficiency sake, RGBPixel does not define a
 * copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * RGBPixl is an "aggregate" class.  Its data is public 
 * (m_Red, m_Green, m_Blue)
 * allowing for fast and convenient instantiations/assignments.
 *
 * The following syntax for assigning an index is allowed/suggested:
 *
 *    RGBPixl<float> pixel = {{1.0f, 0.0f, .5f}};
 *    RGBPixl<char> pixelArray[2] = {{255, 255, 255}, {255, 255, 244}};
 *
 * \ingroup ImageObjects
 *
 */

template < typename TComponent = unsigned short >
class RGBPixel
{
public:
  ///! Standard "Self" typedef.
  typedef RGBPixel  Self;
  ///!  Define the component type
  typedef TComponent ComponentType;
  ///! Return the number of components
  static int GetNumberOfComponents(){ return 3;}
  ///! Return the value for the Nth Component
  ComponentType GetNthComponent(int c, const Self& s) 
    { return s.m_Components[c]; }
  ///! Return the value for the Nth Component
  ComponentType GetScalarValue()  
    {
      return sqrt(m_Components[0] * m_Components[0] +
                  m_Components[1] * m_Components[1] +
                  m_Components[2] * m_Components[2]); 
    }
  ///! Set the Nth component to v
  void SetNthComponent(int c, const ComponentType& v)  
    {  m_Components[c] = v; }
  ///! Set the Red component
  void SetRed( ComponentType red ) { m_Components[0] = red;}
  ///! Set the Green component
  void SetGreen( ComponentType green ) {m_Components[1] = green;}
  ///! Set the Blue component
  void SetBlue( ComponentType blue ) {m_Components[2] = blue;}
  ///! Set the three components
  void Set( ComponentType red, ComponentType green, ComponentType blue )
    { m_Components[0] = red; m_Components[1] = green, m_Components[2] = blue;}
  ///! Get the Red component
  const ComponentType & GetRed( void ) const { return m_Components[0];}
  ///! Get the Green component
  const ComponentType & GetGreen( void ) const { return m_Components[1];}
  ///! Get the Blue component
  const ComponentType & GetBlue( void ) const { return m_Components[2];}
  // Data members
  ///! Red, Green, Blue components
  ComponentType  m_Components[3];
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
