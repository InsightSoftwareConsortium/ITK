/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarVector.h
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
#ifndef __itkScalarVector_h
#define __itkScalarVector_h

#include "itkMacro.h"
#include "vnl/vnl_vector_fixed.h"

#include <memory>

namespace itk
{

/** \class ScalarVector
 * \brief A templated class holding bot scalar and vector values and
 *         responding to the GetScalar() and GetVector() methods.
 * 
 * ScalarVector is a templated class that holds a scalar value plus an
 * array of values (a vector).  ScalarVector can be used as the data type
 * held at each pixel in an Image or at each vertex of an Mesh. There
 * are three template parameters: the type of scalar, the type of vector, and
 * the number of components of the vector. The types can be any data type
 * that behaves like a primitive (or atomic) data type (int, short, float,
 * complex).  itk filters that rely on scalar data assume the data type held
 * at each pixel or each vertex responds to GetScalar()/SetScalar()
 * methods. itk filters that rely on vector data assume the data type held at
 * each pixel or each vertex responds to GetVector()/SetVector() methods. If
 * not, a compile time error will occur.
 *
 * ScalarVector is not a dynamically extendible array like std::vector. It
 * is intended to be used like a mathematical vector.
 *
 * If you wish a simpler pixel types, you can use Scalar, which represents
 * a single data value at a pixel. YOu can also use Vector, which supports
 * (for a given pixel) an array of vector values.
 * 
 * \sa Image
 * \sa Mesh
 * \sa Scalar
 * \sa Vector 
 * \ingroup DataRepresentation
 * 
 */

template<class TScalar, class TVector, unsigned int TVectorDimension=3>
class ScalarVector {
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ScalarVector  Self;
  
  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the ScalarVector.  
   */
  typedef TScalar ValueType;

  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the ScalarVector.  
   */
  typedef TScalar ScalarValueType;

  /**
   * ValueType can be used to declare a variable that is the same type
   * as the data held in the scalar portion of the ScalarVector.  
   */
  typedef TVector VectorValueType;

  /**
   * VectorType can be used to declare a variable that is the same type
   * as the internal vector.  
   */
  typedef vnl_vector_fixed<TVector, TVectorDimension> VectorType;

  /**
   * Get the scalar value.
   * \sa SetScalar()
   */
  TScalar GetScalar() const 
    { return m_Scalar; }

  /**
   * Set the scalar value.
   * \sa GetScalar()
   */
  void SetScalar(const TScalar &val) 
    { m_Scalar = val; }

  /**
   * Get the dimension (size) of the vector.
   */
  static unsigned int GetVectorDimension() 
    { return TVectorDimension; }
  
  /**
   * Get the vector. This provides a read only reference to the vector.
   * \sa SetVector().
   */
  const VectorType &GetVector() const 
    { return m_Vector; }

  /**
   * Get the vector. This provides a read/write reference to the vector.
   * \sa SetVector
   */
  VectorType &GetVector()  
    { return m_Vector; }

  /**
   * Set the vector. 
   * \sa GetVector
   */
  void SetVector(const VectorType &vec)
    { m_Vector = vec; }

private:
  TScalar m_Scalar;
  VectorType m_Vector;
};

  
} // end namespace itk
  
#endif 
