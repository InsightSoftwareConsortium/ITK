/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultConvertPixelTraits.h
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

#ifndef __itkDefaultConvertPixelTraits_h
#define __itkDefaultConvertPixelTraits_h
namespace itk
{
  
/**
 * \brief Traits class used to by ConvertPixels to convert blocks of pixels.
 *
 *  TOutputPixelType is the destination type. The input type is infered
 *  by the templated static function Convert.
 *
 *  This implementaion, does a simple assignment operator, so if you are
 *  going from from a higher bit representation to a lower bit one (int to
 *  char), you may want to specialize and add some sort of transfer function.
 * 
 */

template< typename PixelType>
class DefaultConvertPixelTraits
{
public:
  typedef typename PixelType::ComponentType ComponentType;
  static int GetNumberOfComponents() 
    { return PixelType::GetNumberOfComponents();}
  static ComponentType GetNthComponent(int c, const PixelType& pixel) 
    {
      return pixel.GetNthComponent(c);
    }
  static void SetNthComponent(int c, PixelType& pixel, const ComponentType& v) 
    {
      pixel.SetNthComponent(c, v);
    }
  static ComponentType GetScalarValue(const PixelType& pixel)
    {
      return pixel.GetScalarValue();
    }
};

#define ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(type)                    \
template<>                                                               \
class DefaultConvertPixelTraits<type>                                    \
{                                                                        \
public:                                                                  \
  typedef type ComponentType;                                            \
  static int GetNumberOfComponents()                                     \
    {                                                                    \
      return 1;                                                          \
    }                                                                    \
  static void SetNthComponent(int , type& pixel, const ComponentType& v) \
    {                                                                    \
      pixel = v;                                                         \
    }                                                                    \
  static type GetScalarValue(const type& pixel)                          \
    {                                                                    \
      return pixel;                                                      \
    }                                                                    \
};


ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(float)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(double)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(int)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(char)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(short)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned int)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned char)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned short)

#undef ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL
  
} // end namespace itk



#endif
