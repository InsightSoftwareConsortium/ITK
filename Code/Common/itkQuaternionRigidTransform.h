/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidTransform.h
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

#ifndef __itkQuaternionRigidTransform_h
#define __itkQuaternionRigidTransform_h

#include <iostream>
#include "itkTransform.h"
#include "vnl/vnl_quaternion.h"

namespace itk
{

/**
 *
 * QuaternionRigidTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 *
 * \ingroup Transforms
 *
 **/
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT QuaternionRigidTransform :
            public Transform< TScalarType,
                              3, 3,       // Dimensions of input and output spaces
                              Point< double, 7 >, // a versor plus a vector
                              Matrix<double, 3, 7 > >
{
public:

    /**
     * Standard Self Typedef
     */
    typedef QuaternionRigidTransform Self;



    /**
     * Standard "Superclass" typedef.
     */
    typedef Transform< TScalarType, 3, 3,
                       Point< double, 7 >,
                       Matrix< double, 3, 7 > >   Superclass;


    /**
     * Scalar Type
     */
    typedef typename Superclass::ScalarType  ScalarType;


    /**
     * Parameters Type
     */
    typedef typename Superclass::ParametersType  ParametersType;


    /**
     * Jacobian Type
     */
    typedef typename Superclass::JacobianType  JacobianType;

    /**
     * VnlQuaternion Type
     */
    typedef vnl_quaternion<TScalarType>           VnlQuaternionType;


    /**
     * Smart pointer typedef support
     */
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;


    /**
     * Run-time type information (and related methods).
     */
    itkTypeMacro( QuaternionRigidTransform, Transform );


    /**
     * New macro for creation of through a Smart Pointer
     */
    itkNewMacro( Self );

    /**
     * Dimension of parameters
     */
    enum { SpaceDimension = 3,
           ParametersDimension = 7 };

    /**
     * Dimension of the domain space
     */
    enum { InputSpaceDimension = Superclass::InputSpaceDimension,
           OutputSpaceDimension = Superclass::OutputSpaceDimension };


    /// Standard matrix type for this class
    typedef Matrix<ScalarType, InputSpaceDimension, InputSpaceDimension> MatrixType;

    /// Standard vector type for this class
    typedef Vector<TScalarType, InputSpaceDimension> OffsetType;

    /// Standard coordinate point type for this class
    typedef typename Superclass::InputPointType    InputPointType;
    typedef typename Superclass::OutputPointType   OutputPointType;

    /// Standard vnl_quaternion type
    typedef vnl_quaternion<TScalarType>           VnlQuaternionType;

    /**
     * Get offset of an QuaternionRigidTransform
     *
     * This method returns the value of the offset of the
     * QuaternionRigidTransform.
     **/
    const OffsetType & GetOffset(void) const
        { return m_Offset; }

    /**
     * Get rotation from an QuaternionRigidTransform
     *
     * This method returns the value of the rotation of the
     * QuaternionRigidTransform.
     **/
    const VnlQuaternionType & GetRotation(void) const
        { return m_Rotation; }


    /**
     * Get rotation MAtrix from an QuaternionRigidTransform
     *
     * This method returns the value of the rotation of the
     * QuaternionRigidTransform.
     **/
    const MatrixType & GetRotationMatrix(void) const
      { return m_DirectMatrix; }


    /**
     * Set offset of a QuaternionRigidTransform
     *
     * This method sets the offset of a QuaternionRigidTransform to a
     * value specified by the user.
     **/
    void SetOffset(const OffsetType &offset)
        { m_Offset = offset; return; }

    /**
     * Set Rotation of the Rigid transform
     *
     * This method sets the rotation of a QuaternionRigidTransform to a
     * value specified by the user.
     **/
    void SetRotation(const VnlQuaternionType &rotation);


    /**
     * Transform by rigid transformation
     *
     * This method applies the affine transform given by self to a
     * given point or vector, returning the transformed point or
     * vector.
     **/
    OutputPointType     TransformPoint(const InputPointType  &point ) const;


    /**
     * Set the transformation from a container of parameters
     * This is typically used by optimizers.
     *
     * There are 7 parameters. The first four represents the
     * quaternion and the last three represents the
     * offset.
     *
     **/
    void SetParameters( const ParametersType & parameters );


  /**
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point.
   */
    const JacobianType & GetJacobian(const InputPointType  &point ) const;


protected:
    /**
     * Construct an QuaternionRigidTransform object
     *
     **/
    QuaternionRigidTransform();

    /**
     * Copy a QuaternionRigidTransform object
     *
     * This method creates a new QuaternionRigidTransform object and
     * initializes it to be a copy of an existing QuaternionRigidTransform.
     **/
    QuaternionRigidTransform(const Self & other);

    /**
     * Destroy an QuaternionRigidTransform object
     **/
    ~QuaternionRigidTransform(){};

    /**
     * Print contents of an QuaternionRigidTransform
     **/
    void PrintSelf(std::ostream &os, Indent indent) const;

    /**
     * Assignment operator
     **/
    const Self & operator=( const Self & );

    // matrix representation of the rotation
    // Should be protected in order to be modified
    // by derived classes that instantiate an interface
    // to rotation computation
    MatrixType          m_DirectMatrix;


private:

    // Offset of the transformation
    OffsetType          m_Offset;

    // Rotation of the transformation
    VnlQuaternionType   m_Rotation;


}; //class QuaternionRigidTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuaternionRigidTransform.txx"
#endif

#endif /* __itkQuaternionRigidTransform_h */
