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
#include "itkMatrix.h"
#include "itkVersor.h"

namespace itk
{

/** \class CenteredEuler3DTransform
 * \brief CenteredEuler3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation about a specific coordinate or
 * centre of rotation followed by a translation.
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars 
class ITK_EXPORT CenteredEuler3DTransform : 
        public Euler3DTransform< TScalarType >
{
public:
  /** Standard class typedefs. */
  typedef CenteredEuler3DTransform        Self;
  typedef Euler3DTransform< TScalarType > Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( CenteredEuler3DTransform, Euler3DTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 9);

  typedef typename Superclass::ParametersType             ParametersType;
  typedef typename Superclass::JacobianType               JacobianType;
  typedef typename Superclass::ScalarType                 ScalarType;
  typedef typename Superclass::InputVectorType            InputVectorType;
  typedef typename Superclass::OutputVectorType           OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType  
                                                     InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType  
                                                     OutputCovariantVectorType;

  typedef typename Superclass::InputVnlVectorType         InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType        OutputVnlVectorType;
  typedef typename Superclass::InputPointType             InputPointType;
  typedef typename Superclass::OutputPointType            OutputPointType;
  typedef typename Superclass::MatrixType                 MatrixType;
  typedef typename Superclass::InverseMatrixType          InverseMatrixType;
  typedef typename Superclass::CenterType                 CenterType;
  typedef typename Superclass::TranslationType            TranslationType;
  typedef typename Superclass::OffsetType                 OffsetType;

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

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  CenteredEuler3DTransform();
  CenteredEuler3DTransform(unsigned int SpaceDimension,
                           unsigned int ParametersDimension);
  CenteredEuler3DTransform(const MatrixType & matrix,
                           const OutputVectorType & offset);
  ~CenteredEuler3DTransform();

  /**
   * Print contents of an CenteredEuler3DTransform
   **/
  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  CenteredEuler3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; //class CenteredEuler3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredEuler3DTransform.txx"
#endif

#endif /* __itkCenteredEuler3DTransform_h */
