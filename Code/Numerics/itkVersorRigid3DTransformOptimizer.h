/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransformOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVersorRigid3DTransformOptimizer_h
#define __itkVersorRigid3DTransformOptimizer_h

#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkVersor.h"

namespace itk
{
/** \class VersorRigid3DTransformOptimizer
 * \brief Implement a gradient descent optimizer for the VersorRigid3DTransform
 * parameter space.
 *
 * VersorRigid3DTransformOptimizer is a variant of the gradient descent
 * optimizer implmented in RegularStepGradientDescentOptimizer.
 *
 * Versors are not in a vector space, for that reason, the classical gradient
 * descent algorithm has to be modified in order to be applicable to Versors
 * (unit quaternions) that form the group SO(3).
 *
 * The Versor space has only three degrees of freedom, even though Versors are
 * represented using four values.
 *
 * This optimizer assumes that the CostFunction to be optimized has an
 * itk::Versor and an itk::Vector as parameters.
 *
 * \sa RegularStepGradientDescentOptimizer
 * \sa Versor
 * \sa VersorRigid3DTransform
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT VersorRigid3DTransformOptimizer:
  public RegularStepGradientDescentBaseOptimizer
{
public:
  /** Standard class typedefs. */
  typedef VersorRigid3DTransformOptimizer         Self;
  typedef RegularStepGradientDescentBaseOptimizer Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VersorRigid3DTransformOptimizer,
               RegularStepGradientDescentBaseOptimizer);

  /** This class is specialized for 3D  */
  itkStaticConstMacro(SpaceDimension, unsigned int, 6);

  /**  Versor Type  */
  typedef Versor< double >       VersorType;
  typedef VersorType::VectorType VectorType;

  /** Advance one step following the gradient direction. */
  virtual void StepAlongGradient(double factor,
                                 const DerivativeType & transformedGradient);

protected:
  VersorRigid3DTransformOptimizer() {}
  virtual ~VersorRigid3DTransformOptimizer() {}
private:
  VersorRigid3DTransformOptimizer(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented
};
} // end namespace itk

#endif
