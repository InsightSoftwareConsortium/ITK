/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransformOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVersorTransformOptimizer_h
#define __itkVersorTransformOptimizer_h

#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkVersor.h"

namespace itk
{
  
/** \class VersorTransformOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * VersorTransformOptimizer is a variant of the
 * gradient descent optimizer implmented in 
 * RegularStepGradientDescentOptimizer.
 *
 * Versors are not in a vector space, for that reason, 
 * the classical gradient descent algorithm has to be
 * modified in order to be applicable to Versors (unit
 * quaternions) that form the group SO(3).
 *
 * The Versor space has only three degrees of freedom,
 * even though Versors are represented using four values.
 *
 * This optimizer assumes that the CostFunction to be
 * optimized has an itk::Versor as parameter.
 * 
 * \sa RegularStepGradientDescentOptimizer
 * \sa Versor
 * \sa VersorTransform
 *
 * \ingroup Numerics Optimizers
 */  
class ITK_EXPORT VersorTransformOptimizer : 
    public RegularStepGradientDescentBaseOptimizer
{
public:
  /** Standard class typedefs. */
  typedef VersorTransformOptimizer                    Self;
  typedef RegularStepGradientDescentBaseOptimizer     Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( VersorTransformOptimizer, 
                RegularStepGradientDescentBaseOptimizer );

  /** This class is specialized for 3D  */
  enum { SpaceDimension = 3 };

  /**  Versor Type  */
  typedef Versor<double>                      VersorType;
  typedef VersorType::VectorType              VectorType;

  /** Advance one step following the gradient direction. */
  virtual void StepAlongGradient( double factor, 
                                  const DerivativeType & transformedGradient );

protected:
  VersorTransformOptimizer() {}
  virtual ~VersorTransformOptimizer() {}

private:
  VersorTransformOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk


#endif



