/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersor.h
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
#ifndef __itkVersor_h
#define __itkVersor_h


#include "itkVector.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_quaternion.h"
#include "vnl/vnl_vector_fixed.h"


namespace itk
{

/** \class Versor
 * \brief A templated class holding Unit Quaternion.
 * 
 * Versor is a templated class that holds a unit quaternion.
 * The difference between Versors and Quaternions is that Quaternions
 * can represent rotations and scale changes while Versors are limited
 * to rotations.
 *
 * This class only implements the operations that maintain Versors as
 * a group, that is, any operations between versors result in another
 * versor. For this reason, addition is not defined in this class, even
 * though it is a valid operation between quaternions.
 *
 * \ingroup Geometry
 * \ingroup DataRepresentation
 * 
 * \sa Vector
 * \sa Point
 * \sa CovariantVector
 * \sa Matrix
 *
 */

template<class T> 
class Versor 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Versor  Self;
   

  /**
   * ValueType can be used to declare a variable that is the same type
   * as a data element held in a Versor.  
   */
  typedef T ValueType;


  /**
   * Vector type used to represent the axis.
   */
  typedef  Vector<T,3>   VectorType;


  /**
   * Point type 
   */
  typedef  Point<T,3>   PointType;

 
  /**
   * CovariantVector type 
   */
  typedef  CovariantVector<T,3>   CovariantVectorType;

 
  /**
   * Vnl Vector type 
   */
  typedef  vnl_vector_fixed<T,3>   VnlVectorType;

       

  /**
   * Get a vnl_quaternion with a copy of the internal memory block
   */
  vnl_quaternion<T> GetVnlQuaternion( void ) const;


  /**
   * Default constructor creates a null versor 
   * (representing 0 degrees  rotation)
   */
  Versor();


  /**
   * Copy Constructor 
   */
  Versor(const Self & v);


  /**
   * Assignment operator =.  Copy the versor argument
   */
  const Self& operator=(const Self & v);

 
 
  /**
   * Composition operator *=.  Compose the current versor
   * with the operand and store the result in the current
   * versor.
   */
  const Self& operator*=(const Self & v);


  /**
   * Get Conjugate versor.  Returns the versor that produce
   * a rotation by the same angle but in opposite direction
   */
  Self GetConjugate(void) const;
  

  /**
   * Get Reciprocal versor.  Returns the versor that composed
   * with this one will result in a scalar operator equals to 1.
   * It is also equivalent to 1/this.
   */
  Self GetReciprocal(void) const;
  


  /**
   * Versor operator*.  Performs the composition of two versors.
   * this operation is NOT commutative.
   */
  Self operator*(const Self &vec) const;


  /**
   * Returns the Scalar part
   */
  ValueType GetScalar( void ) const;
  


  /**
   * Returns the X component
   */
  ValueType GetX( void ) const
    { return m_X; }
  

  /**
   * Returns the Y component
   */
  ValueType GetY( void ) const
    { return m_Y; }
  

  /**
   * Returns the Z component
   */
  ValueType GetZ( void ) const
    { return m_Z; }
  

  /**
   * Returns the W component
   */
  ValueType GetW( void ) const
    { return m_W; }
  

  /**
   * Returns the rotation angle in radians. 
   */
  ValueType GetAngle( void ) const;
  

  /**
   * Returns the axis of the rotation.
   * It is a unit vector parallel to the axis.
   */
   VectorType GetAxis( void ) const;
 
   
  /**
   * Returns the Right part
   * It is a vector part of the Versor. It is 
   * called Right because it is equivalent to
   * a right angle rotation
   */
   VectorType GetRight( void ) const;

   
  /**
   * Set the versor using an vector and angle
   * the unit vector parallel to the given vector 
   * will be used. The angle is expected in radians
   */
  void Set( const VectorType & axis, ValueType angle );
 

  /**
   * Transform a Vector
   */
   VectorType Transform( const VectorType & v ) const;


  /**
   * Transform a Covariant Vector
   */
   CovariantVectorType Transform( const CovariantVectorType & v ) const;


  /**
   * Transform a Point
   */
   PointType Transform( const PointType & v ) const;


  /**
   * Transform a vnl_vector
   */
   VnlVectorType Transform( const VnlVectorType & v ) const;



private:

   /** 
    * Component parallel to x axis
    */
   ValueType  m_X;
   
   /** 
    * Component parallel to y axis
    */
   ValueType  m_Y;
   
   /** 
    * Component parallel to z axis
    */
   ValueType  m_Z;

   /** 
    * Escalar component of the Versor
    */
   ValueType  m_W;

};

template< class T>  
ITK_EXPORT std::ostream& operator<<( std::ostream& os, 
                                     const Versor<T> & v)
{
  os << "[";
  os << v.GetX() << ", " << v.GetY () << ", " << v.GetZ() << ", " << v.GetW();
  os << "]" << std::endl;
  return os;
}

template< class T>
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                    Versor<T> & v); 


} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersor.txx"
#endif


#endif 
