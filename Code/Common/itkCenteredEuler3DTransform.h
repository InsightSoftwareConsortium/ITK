/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredEuler3DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCenteredEuler3DTransform_h
#define __itkCenteredEuler3DTransform_h

#include <iostream>
#include "itkEuler3DTransform.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_quaternion.h"
#include "itkMatrix.h"
#include "itkVersor.h"

namespace itk
{

/** \brief CenteredEuler3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation about a specific coordinate or
 * centre of rotation followed by a translation.
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT CenteredEuler3DTransform : 
        public Euler3DTransform< TScalarType >
{
public:
  /** Standard class typedefs. */
  typedef CenteredEuler3DTransform Self;
  typedef Euler3DTransform< TScalarType > Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( CenteredEuler3DTransform, Euler3DTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

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
   * There are six parameters. The first three represent the
   * rotation and the last three represent the translation. */
  void SetParameters( const ParametersType & parameters );

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are six parameters. The first three represent the
   * rotation and the last three represent the translation. */
  const ParametersType & GetParameters( void ) const;

  /** Set and Get the center of rotation */
  void SetCenter( const InputPointType & center );
  itkGetConstReferenceMacro( Center, InputPointType );

  /** Set and Get the Translation to be applied after rotation */
  void SetTranslation( const OutputVectorType & translation );
  itkGetConstReferenceMacro( Translation, OutputVectorType );

  /**
   * Back transform by an affine transformation
   *
   * This method finds the point or vector that maps to a given
   * point or vector under the affine transformation defined by
   * self.  If no such point exists, an exception is thrown.
   **/
  inline InputPointType      BackTransform(const OutputPointType  &point ) const;
  inline InputVectorType     BackTransform(const OutputVectorType &vector) const;
  inline InputVnlVectorType  BackTransform(const OutputVnlVectorType &vector) const;

  inline InputCovariantVectorType BackTransform(
                                     const OutputCovariantVectorType &vector) const;

  /**
   * Find inverse of an affine transformation
   *
   * This method creates and returns a new CenteredEuler3DTransform object
   * which is the inverse of self.  If self is not invertible,
   * false is returned.
   **/
  bool GetInverse(Self* inverse) const;

  /** Set the parameters to the IdentityTransform */
  virtual void SetIdentity(void);

protected:
  CenteredEuler3DTransform();
  ~CenteredEuler3DTransform();

  
  /**
   * Print contents of an CenteredEuler3DTransform
   **/
  void PrintSelf(std::ostream &os, Indent indent) const;


  /** Compute the components of the rotation matrix in the superclass. */
  virtual void ComputeMatrix(void);

private:
  CenteredEuler3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**
   * Transformation translation (applied after rotation).
   * Not to be confused with member m_Offset in base class
   * Rigid3DTransform. m_Offset is recomputed to take into account
   * translation to the centre of rotation pre- (negative) and post-
   * (positive) rotation, and the subsequent translation m_Translation.
   **/
  OutputVectorType        m_Translation;   

  // The center of rotation coordinate 
  InputPointType          m_Center;   

}; //class CenteredEuler3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredEuler3DTransform.txx"
#endif

#endif /* __itkCenteredEuler3DTransform_h */
