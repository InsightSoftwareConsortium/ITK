/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransform.h
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
#ifndef __itkTransform_h
#define __itkTransform_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"

#include "itkObjectFactory.h"


namespace itk
{
  
/** \class Transform
 * \brief Generic concept of transformation methods
 *
 * This Abstract Class define the generic interface for a geometrical 
 * transformation from one space to another. The class provides methods
 * for mapping points from the input space to the output space, it also
 * maps vectors and covariant vectors. 
 *
 * Given that transformation are not necesarily invertible, this basic
 * class provide the methods for back transfromation. Derived classes that
 * implement non-invertible transformation should throw exceptions when
 * appropriate.
 *
 * \ingroup Transforms
 *
 */

template <class TScalarType,
          unsigned int NInputDimensions, 
          unsigned int NOutputDimensions,
          class TParameters,
          class TJacobianType >
class ITK_EXPORT  Transform  : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Transform  Self;

  /**
   * Dimension of the domain space
   */
  enum { InputSpaceDimension     = NInputDimensions,
         OutputSpaceDimension    = NOutputDimensions   };

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer< Self >   Pointer;
  typedef SmartPointer< const Self >  ConstPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( Transform, Object );


  /** 
   * New method for creating an object using a factory
   */
  itkNewMacro(Self);


  /** 
   * Type of the input parameters
   */
  typedef  TScalarType     ScalarType;


  /** 
   * Type of the input parameters
   */
  typedef  TParameters     ParametersType;


  /** 
   * Type of the Jacobian Matrix
   */
  typedef  TJacobianType JacobianType;


  /**
   * Standard vector type for this class
   */
  typedef Vector<TScalarType, InputSpaceDimension>  InputVectorType;
  typedef Vector<TScalarType, OutputSpaceDimension> OutputVectorType;


  /**
   * Standard covariant vector type for this class
   */
  typedef CovariantVector<TScalarType, InputSpaceDimension>  InputCovariantVectorType;
  typedef CovariantVector<TScalarType, OutputSpaceDimension> OutputCovariantVectorType;


  /**
   * Standard vnl_vector type for this class
   */
  typedef vnl_vector_fixed<TScalarType, InputSpaceDimension>  InputVnlVectorType;
  typedef vnl_vector_fixed<TScalarType, OutputSpaceDimension> OutputVnlVectorType;


  /**
   * Standard coordinate point type for this class
   */
  typedef Point<TScalarType, InputSpaceDimension> InputPointType;
  typedef Point<TScalarType, OutputSpaceDimension> OutputPointType;


  /**
   *  Method to transform a Point
   */
  virtual OutputPointType TransformPoint(const InputPointType  &point ) const
    { OutputPointType result; return result; }

  /**
   *  Method to transform a vector
   */
  virtual OutputVectorType    TransformVector(const InputVectorType &vector) const
    { OutputVectorType result; return result; }

  /**
   *  Method to transform a vnl_vector
   */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &vector) const
    { OutputVnlVectorType result; return result; }

  /**
   *  Method to transform a CovariantVector
   */
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &vector) const
    { OutputCovariantVectorType result; return result; }

  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
  virtual void SetParameters(const ParametersType &)=0;


  /**
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point.
   *
   * The Jacobian can be expressed as a set of partial derivatives of the
   * output point components with respect to the parameters that defined
   * the transform:
   *
   * \f[
   *

      J=\left[ \begin{array}{cccc}
      \frac{\partial x_{1}}{\partial p_{1}} & 
      \frac{\partial x_{2}}{\partial p_{1}} & 
      \cdots  & \frac{\partial x_{n}}{\partial p_{1}}\\
      \frac{\partial x_{1}}{\partial p_{2}} & 
      \frac{\partial x_{2}}{\partial p_{2}} & 
      \cdots  & \frac{\partial x_{n}}{\partial p_{2}}\\
      \vdots  & \vdots  & \ddots  & \vdots \\
      \frac{\partial x_{1}}{\partial p_{m}} & 
      \frac{\partial x_{2}}{\partial p_{m}} & 
      \cdots  & \frac{\partial x_{n}}{\partial p_{m}}
      \end{array}\right] 
      
   *
   * \f]
   *
   **/
  virtual const JacobianType & GetJacobian(const InputPointType  &point ) const;



protected:
  
  Transform();
  virtual ~Transform() {};
  Transform(const Self&);
  const Self & operator=(const Self&);


  /**
   * Jacobian matrix of the transformation. It is used to compute
   * derivatives by using the chain rule.
   */
  mutable JacobianType                m_Jacobian;     



private:

  /**
   *  List of parameters that unambiguosly define the transformation
   */  
  ParametersType                      m_Parameters;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransform.txx"
#endif

#endif



