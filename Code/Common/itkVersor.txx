/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersor.txx
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
#ifndef _itkVersor_txx
#define _itkVersor_txx

#include "itkVector.h" 
#include "itkNumericTraits.h" 


namespace itk
{


/**
 * Constructor to initialize entire vector to one value.
 */
template<class T>
Versor<T>
::Versor()
{
  m_X = NumericTraits<T>::Zero;
  m_Y = NumericTraits<T>::Zero;
  m_Z = NumericTraits<T>::Zero;
  m_W = NumericTraits<T>::One;
}



/**
 * Copy Constructor 
 */
template<class T>
Versor<T>
::Versor( const Self & v)
{
  m_X = v.m_X;
  m_Y = v.m_Y;
  m_Z = v.m_Z;
  m_W = v.m_W;
}



/**
 * Assignment Operator
 */
template<class T>
const Versor<T> &
Versor<T>
::operator=( const Self & v)
{
  m_X = v.m_X;
  m_Y = v.m_Y;
  m_Z = v.m_Z;
  m_W = v.m_W;
  return *this;
}




/**
 * Return a vnl_quaternion
 */
template<class T>
vnl_quaternion<T>
Versor<T>
::GetVnlQuaternion(void) const
{
  return vnl_quaternion<T>(m_X,m_Y,m_Z,m_W);
}



/**
 * Assignment and Composition Operator
 */
template<class T>
const Versor<T> &
Versor<T>
::operator*=( const Self & v)
{

  const double mx =  m_W*v.m_X - m_Z*v.m_Y + m_Y*v.m_Z + m_X*v.m_W;
  const double my =  m_Z*v.m_X + m_W*v.m_Y - m_X*v.m_Z + m_Y*v.m_W;
  const double mz = -m_Y*v.m_X + m_X*v.m_Y + m_W*v.m_Z + m_Z*v.m_W;
  const double mw = -m_X*v.m_X - m_Y*v.m_Y - m_Z*v.m_Z + m_W*v.m_W;

  m_X = mx;
  m_Y = my;
  m_Z = mz;
  m_W = mw;

  return *this;
}



/**
 * Composition Operator
 */
template<class T>
Versor<T> 
Versor<T>
::operator*( const Self & v) const
{
 
  Self result;

  result.m_X =  m_W*v.m_X - m_Z*v.m_Y + m_Y*v.m_Z + m_X*v.m_W;
  result.m_Y =  m_Z*v.m_X + m_W*v.m_Y - m_X*v.m_Z + m_Y*v.m_W;
  result.m_Z = -m_Y*v.m_X + m_X*v.m_Y + m_W*v.m_Z + m_Z*v.m_W;
  result.m_W = -m_X*v.m_X - m_Y*v.m_Y - m_Z*v.m_Z + m_W*v.m_W;

  return result;
}



/**
 * Get Conjugate
 */
template<class T>
Versor<T> 
Versor<T>
::GetConjugate( void ) const
{
  Self result;
  
  result.m_X = -m_X;
  result.m_Y = -m_Y;
  result.m_Z = -m_Z;
  result.m_W =  m_W;

  return result;
}
 



/**
 * Get Reciprocal
 */
template<class T>
Versor<T> 
Versor<T>
::GetReciprocal( void ) const
{
  Self result;
  
  result.m_X = -m_X;
  result.m_Y = -m_Y;
  result.m_Z = -m_Z;
  result.m_W =  m_W;

  return result;
}
 



/**
 * Get Axis
 */
template<class T>
Versor<T>::VectorType 
Versor<T>
::GetAxis( void ) const
{
  VectorType axis;
  
  const ValueType vectorNorm = 
                      sqrt( m_X*m_X + m_Y*m_Y + m_Z*m_Z );

  axis[0] = m_X / vectorNorm;
  axis[1] = m_Y / vectorNorm;
  axis[2] = m_Z / vectorNorm;

  return axis;
}
 


/**
 * Get Right part
 */
template<class T>
Versor<T>::VectorType 
Versor<T>
::GetRight( void ) const
{
  VectorType axis;
  
  axis[0] = m_X;
  axis[1] = m_Y;
  axis[2] = m_Z;

  return axis;
}
 



/**
 * Get Scalar part
 */
template<class T>
Versor<T>::ValueType 
Versor<T>
::GetScalar( void ) const
{
  return m_W;
}
 


/**
 * Get Angle (in radians)
 */
template<class T>
Versor<T>::ValueType
Versor<T>
::GetAngle( void ) const
{

  const ValueType vectorNorm = 
                      sqrt( m_X*m_X + m_Y*m_Y + m_Z*m_Z );

  const ValueType angle = 2.0 * atan2( vectorNorm, m_W );
  
  return angle;

}
 


/**
 * Set Axis and Angle (in radians)
 */
template<class T>
void
Versor<T>
::Set( const VectorType & axis, ValueType angle )
{

  const ValueType vectorNorm = axis.GetNorm();

  const ValueType cosangle2 = cos( angle / 2.0 );
  const ValueType sinangle2 = sin( angle / 2.0 );
  
  const ValueType factor = sinangle2 / vectorNorm;
  
  m_X = axis[0] * factor;
  m_Y = axis[1] * factor;
  m_Z = axis[2] * factor;
  
  m_W = cosangle2;

}
 



} // end namespace itk


#endif
