/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBAPixel.txx
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
#ifndef _itkRGBAPixel_txx
#define _itkRGBAPixel_txx
#include "itkRGBAPixel.h"
#include "itkNumericTraits.h"

namespace itk
{

  /*
 * Assignment Operator
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a Base Array
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const typename BaseArray::Reference& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a Const Base Array
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const typename BaseArray::ConstReference& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assigment from a plain array
 */
template<class T>
RGBAPixel<T>&
RGBAPixel<T>
::operator= (const ComponentType r[4])
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignment from a const scalar value
 */
template<class T>
typename RGBAPixel<T>::ArrayCommaListCopier
RGBAPixel<T>
::operator= (const ComponentType& r)
{
  return BaseArray::operator=(r);
}

/**
 * Print content to an ostream
 */
template<class TComponent>
std::ostream &
operator<<(std::ostream& os,const RGBAPixel<TComponent> & c ) 
{
  os <<  c[0] << "  ";
  os <<  c[1] << "  ";
  os <<  c[2] << "  ";
  os <<  c[3] ;
  return os;
}


/**
 * Read content from an istream
 */
template<class TComponent>
std::istream &
operator>>(std::istream& is, RGBAPixel<TComponent> & c ) 
{
  TComponent red;
  TComponent green;
  TComponent blue;
  TComponent alpha;
  is >> red >> green >> blue;
  c.SetRed( red );
  c.SetGreen( green );
  c.SetBlue( blue );
  c.SetAlpha( alpha );
  return is;
}

} // end namespace itk

#endif
