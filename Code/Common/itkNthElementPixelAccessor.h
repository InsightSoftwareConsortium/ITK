/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNthElementPixelAccessor.h
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
#ifndef __itkNthElementPixelAccessor_h
#define __itkNthElementPixelAccessor_h



namespace itk
{

/**
 * \class NthElementPixelAccessor
 * \brief Give access to the N-th of a Container type 
 *
 * This class is intended to be used as parameter of 
 * an ImageAdaptor to make a  Container appears as being
 * of scalar type T, showing only the N-th component.
 *
 * This class is templated over the container type.
 * Any container type that provides a method:
 * operator[]( unsigned int ) can be used here,
 * for example: itkPoint, itkVector, itkVectorContainer,
 *              and std::vector.
 *
 * For performance, no bound checking is performed during
 * access to the n-th element.
 *
 * \sa ImageAdaptor
 * \sa PixelAccessor
 *
 */

template < class T, class TContainer >
class ITK_EXPORT NthElementPixelAccessor
{
public:
 /**
   * Standard "Self" typedef.
   */
  typedef   NthElementPixelAccessor        Self;

 /** 
   * External typedef. It defines the external aspect
   * that this class will exhibit
   */
  typedef T ExternalType;

  /** 
   * Internal typedef. It defines the internal real
   * representation of data
   */
  typedef   TContainer    InternalType;


  /** 
   * Write access to the NthElement component
   */
  inline void Set( InternalType & output, const ExternalType & input ) const 
    { output[m_ElementNumber] =  input; }


  /** 
   * Read access to the NthElement component
   */
  inline ExternalType Get( const InternalType & input ) const
    { return static_cast<ExternalType>( input[m_ElementNumber] ); }

  /** 
   * Get the element number to access in the container
   */
  unsigned int GetElementNumber(void) const
  { return m_ElementNumber; }

  /** 
   * Set the element number to access in the container
   */
  void SetElementNumber( unsigned int nth )
  { m_ElementNumber = nth; }


private:
  // Identifier of the N-th element to be accessed
  unsigned int    m_ElementNumber;

};

  
}  // end namespace itk

#endif

