/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidTransformGradientDescentOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuaternionRigidTransformGradientDescentOptimizer_h
#define __itkQuaternionRigidTransformGradientDescentOptimizer_h

#include "itkGradientDescentOptimizer.h"

namespace itk
{
  
/** \class QuaternionRigidTransformGradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * QuaternionRigidTransformGradientDescentOptimizer is an extension to the
 * simple gradient descent optimizer implmented in GradientDescentOptimizer.
 * At each iteration the current position is updated according to
 *
 * p(n+1) = p(n) + learningRate * d f(p(n)) / d p(n)
 *
 * \f[ 
 *        p_{n+1} = p_n 
 *                + \mbox{learningRate} 
 *                \, \frac{\partial f(p_n) }{\partial p_n} 
 * \f]
 *
 * The learning rate is a fixed scalar defined via SetLearningRate().
 * The optimizer steps through a user defined number of iterations;
 * no convergence checking is done.
 * The first four components of p are assumed to be the four components
 * of the quaternion. After each update, the quaternion is normalized to 
 * have a magnitude of one. This ensures the the transform is purely rigid.
 * 
 * \sa GradientDescentOptimizer
 * \ingroup Numerics Optimizers
 */  
template <class TCostFunction>
class ITK_EXPORT QuaternionRigidTransformGradientDescentOptimizer : 
        public GradientDescentOptimizer< TCostFunction >
{
public:
  /** Standard class typedefs. */
  typedef QuaternionRigidTransformGradientDescentOptimizer  Self;
  typedef GradientDescentOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

 /** Run-time type information (and related methods). */
  itkTypeMacro( QuaternionRigidTransformGradientDescentOptimizer, 
      GradientDescentOptimizer );

  /** Dimension of the Search Space */
  enum { SpaceDimension = Superclass::SpaceDimension };

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename Superclass::ParametersType ParametersType;

  /** Advance one step following the gradient direction. */
  virtual void AdvanceOneStep( void );

protected:
  QuaternionRigidTransformGradientDescentOptimizer();
  virtual ~QuaternionRigidTransformGradientDescentOptimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


private:
  QuaternionRigidTransformGradientDescentOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuaternionRigidTransformGradientDescentOptimizer.txx"
#endif

#endif



