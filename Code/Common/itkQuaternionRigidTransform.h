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
#include "itkRigid3DTransform.h"

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
            public Rigid3DTransform< TScalarType > 
{
public:

    /**
     * Standard Self Typedef
     */
    typedef QuaternionRigidTransform Self;


    /// Dimension of parameters
    enum { SpaceDimension = 3, 
           ParametersDimension = 7 };

    /**
     * Standard "Superclass" typedef.
     */
    typedef Rigid3DTransform< TScalarType >   Superclass;


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
    typedef typename Superclass::VnlQuaternionType  VnlQuaternionType;

    /**
     * Offset Type
     */
    typedef typename Superclass::OffsetType  OffsetType;

    /**
     * InputPoint Type
     */
    typedef typename Superclass::InputPointType  InputPointType;

    /** 
     * Smart pointer typedef support 
     */
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;


    /** 
     * Run-time type information (and related methods).
     */
    itkTypeMacro( QuaternionRigidTransform, Rigid3DTransform );


    /** 
     * New macro for creation of through a Smart Pointer
     */
    itkNewMacro( Self );


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
     * Assignment operator
     **/
    const Self & operator=( const Self & );


}; //class QuaternionRigidTransform


/**
 * Print the offset of a QuaternionRigidTransform
 * This method prints the offset of the
 * QuaternionRigidTransform as n vector.
 **/
                                                              
 template<class ScalarType >
 inline
 std::ostream &
 operator<< (std::ostream &s, QuaternionRigidTransform<ScalarType > &rigid)
 {
   //s << rigid.m_Offset << std::endl;
   //s << rigid.m_Rotation << std::endl;
   return s;
 }


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuaternionRigidTransform.txx"
#endif

#endif /* __itkQuaternionRigidTransform_h */
