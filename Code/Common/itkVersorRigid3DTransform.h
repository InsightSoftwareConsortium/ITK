/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransform.h
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

#ifndef __itkVersorRigid3DTransform_h
#define __itkVersorRigid3DTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"

namespace itk
{

/** \brief VersorRigid3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT VersorRigid3DTransform : 
            public Rigid3DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef VersorRigid3DTransform Self;
  typedef Rigid3DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( VersorRigid3DTransform, Rigid3DTransform );

  /** Dimension of parameters. */
  enum { SpaceDimension = 3, 
         ParametersDimension = 6 };

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;

  /** VnlQuaternion type. */
  typedef typename Superclass::VnlQuaternionType  VnlQuaternionType;

  /** Versor type. */
  typedef typename Superclass::VersorType  VersorType;
  typedef typename VersorType::VectorType  AxisType;
  typedef typename VersorType::ValueType   AngleType;
  
  /** Offset type. */
  typedef typename Superclass::OffsetType  OffsetType;

  /** Point type. */
  typedef typename Superclass::InputPointType   InputPointType;
  typedef typename Superclass::OutputPointType  OutputPointType;
  
  /** Vector type. */
  typedef typename Superclass::InputVectorType   InputVectorType;
  typedef typename Superclass::OutputVectorType  OutputVectorType;
  
  /** CovariantVector type. */
  typedef typename Superclass::InputCovariantVectorType   InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType  OutputCovariantVectorType;
  
  /** VnlVector type. */
  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType  OutputVnlVectorType;
  
  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 6 parameters. The first three represent the
   * versor and the last three represents the offset. */
  void SetParameters( const ParametersType & parameters );

  /** Set the rotational part of the transform. */
  void SetRotation( const VersorType & versor );
  void SetRotation( const AxisType & axis, AngleType angle );
  
  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  VersorRigid3DTransform();
  ~VersorRigid3DTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrix(void);

private:
  VersorRigid3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**  Versor containing the rotation. */
  VersorType    m_Versor;

}; //class VersorRigid3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersorRigid3DTransform.txx"
#endif

#endif /* __itkVersorRigid3DTransform_h */
