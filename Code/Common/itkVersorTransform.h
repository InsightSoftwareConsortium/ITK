/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVersorTransform_h
#define __itkVersorTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"

namespace itk
{

/**
 *
 * VersorTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 *
 * \ingroup Transforms
 *
 **/
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT VersorTransform : 
            public Rigid3DTransform< TScalarType > 
{
public:

    /**
     * Standard Self Typedef
     */
    typedef VersorTransform Self;


    /// Dimension of parameters
    itkStaticConstMacro(SpaceDimension, unsigned int, 3);
    itkStaticConstMacro(ParametersDimension, unsigned int, 3);

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
    typedef typename Superclass::JacobianType     JacobianType;

    /**
     * VnlQuaternion Type
     */
    typedef typename Superclass::VnlQuaternionType  VnlQuaternionType;

    /**
     * Versor Type
     */
    typedef typename Superclass::VersorType  VersorType;
    typedef typename VersorType::VectorType  AxisType;
    typedef typename VersorType::ValueType   AngleType;

    /**
     * Offset Type
     */
    typedef typename Superclass::OffsetType  OffsetType;

    /**
     * Point Type
     */
    typedef typename Superclass::InputPointType   InputPointType;
    typedef typename Superclass::OutputPointType  OutputPointType;

    /**
     * Vector Type
     */
    typedef typename Superclass::InputVectorType   InputVectorType;
    typedef typename Superclass::OutputVectorType  OutputVectorType;

    /**
     * CovariantVector Type
     */
    typedef typename Superclass::InputCovariantVectorType   InputCovariantVectorType;
    typedef typename Superclass::OutputCovariantVectorType  OutputCovariantVectorType;

    /**
     * VnlVector Type
     */
    typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
    typedef typename Superclass::OutputVnlVectorType  OutputVnlVectorType;




    /** 
     * Smart pointer typedef support 
     */
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;


    /** 
     * Run-time type information (and related methods).
     */
    itkTypeMacro( VersorTransform, Rigid3DTransform );


    /** 
     * New macro for creation of through a Smart Pointer
     */
    itkNewMacro( Self );


    /**
     * Set the transformation from a container of parameters
     * This is typically used by optimizers.
     *
     * There are 3 parameters. They represent the components
     * of the right part of the versor. This can be seen
     * as the components of the vector parallel to the rotation
     * axis and multiplied by sin( angle / 2 ).
     *
     **/
    void SetParameters( const ParametersType & parameters );


    /**
     * Set the rotational part of the transform
     **/
    void SetRotation( const VersorType & versor );
    void SetRotation( const AxisType & axis, AngleType angle );


    /**
     * Compute the Jacobian of the transformation
     *
     * This method computes the Jacobian matrix of the transformation.
     * given point or vector, returning the transformed point or
     * vector. The rank of the Jacobian will also indicate if the 
     * transform is invertible at this point.
     */
    const JacobianType & GetJacobian(const InputPointType  &point ) const;



 protected:
    /**
     * Print contents of a VersorTransform
     **/
    void PrintSelf(std::ostream &os, Indent indent) const;
    /**
     * Construct an VersorTransform object
     *
     **/
    VersorTransform();

    /**
     * Copy a VersorTransform object
     *
     * This method creates a new VersorTransform object and
     * initializes it to be a copy of an existing VersorTransform.
     **/
    VersorTransform(const Self & other);

    /**
     * Destroy an VersorTransform object
     **/
    ~VersorTransform(){};


    /**
     * Assignment operator
     **/
    const Self & operator=( const Self & );


    /**
     * Compute Matrix
     * Compute the components of the rotation matrix in the superclass
     **/
    void ComputeMatrix(void);

private:

    /**
     *  Versor containing the rotation
     */
    VersorType    m_Versor;

}; //class VersorTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersorTransform.txx"
#endif

#endif /* __itkVersorTransform_h */
