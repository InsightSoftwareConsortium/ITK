/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredRigid2DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCenteredRigid2DTransform_h
#define __itkCenteredRigid2DTransform_h

#include <iostream>
#include "itkRigid2DTransform.h"

namespace itk
{

/** \brief CenteredRigid2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rigid transformation in 2D space The transformation
 * is specified as two translation and one rotation.  The translation are
 * applied one before the rotation and the other after the rotation. It can be
 * seen as a composition of a pure translations and pure rotation transforms.
 * 
 * The need for introducing this transform is that a rotation is not specified
 * simply by an angle, as we use to think, but also by a center of rotation. We
 * use to assume that the center of rotation is the origin of the coordinate
 * system. This assumption, however does not usually hold in applications such
 * as image registration.
 *
 * In order to initialize this transform a user should provide the following
 *
 * - Coordinates of the center of rotation in the input space
 * - Angle of rotation (in radians) 
 * - Translation to be applied after the rotation.
 *
 * With these parameters the transform applies first a translation that will
 * move the center of rotation to be the new origin. Then a rotation is
 * performed around this origin, and finally the user-specified translation is
 * applied after the rotation.
 *
 * The serialization of parameters results in an array of five element ordered as
 * follows
 *
 * p[0] = angle
 * p[1] = x coordinate of the centre
 * p[2] = y coordinate of the centre
 * p[3] = x component of the translation
 * p[4] = y component of the translation
 * 
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT CenteredRigid2DTransform : 
            public Rigid2DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef CenteredRigid2DTransform Self;
  typedef Rigid2DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( CenteredRigid2DTransform, Rigid2DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 2);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 2);
  itkStaticConstMacro(ParametersDimension, unsigned int, 5);


  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;
  
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
   * There are 3 parameters. The first one represents the
   * rotation and the last two represents the offset. */
  void SetParameters( const ParametersType & parameters );

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 3 parameters. The first one represents the
   * rotation and the last two represents the offset. */
  const ParametersType & GetParameters( void ) const;

  /** Set the rotational part of the transform. */
  void SetAngle(TScalarType angle);
  void SetAngleInDegrees(TScalarType angle);
  itkGetConstReferenceMacro( Angle, TScalarType );
  
  /** Set and Get the center of rotation */
  void SetCenter( const InputPointType & center );
  itkGetConstReferenceMacro( Center, InputPointType );

  /** Set and Get the Translation to be applied after rotation */
  void SetTranslation( const OutputVectorType & translation );
  itkGetConstReferenceMacro( Translation, OutputVectorType );
  
  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

  /** Set the transformation to an Identity
   * This sets the matrix to identity and the Offset to null. */
  virtual void SetIdentity( void );


protected:
  CenteredRigid2DTransform();
  ~CenteredRigid2DTransform(){};

  CenteredRigid2DTransform(unsigned int outputSpaceDimension, unsigned int parametersDimension);

  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Compute the components of the rotation matrix and offset in the superclass. */
  virtual void ComputeMatrixAndOffset(void);

private:
  CenteredRigid2DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  TScalarType         m_Angle; 

  InputPointType      m_Center;

  OutputVectorType    m_Translation;

}; //class CenteredRigid2DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredRigid2DTransform.txx"
#endif

#endif /* __itkCenteredRigid2DTransform_h */
