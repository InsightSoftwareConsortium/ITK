/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidTransformGradientDescentOptimizer.h
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
 * \ingroup Numerics
 */  
template <class TCostFunction>
class ITK_EXPORT QuaternionRigidTransformGradientDescentOptimizer : 
        public GradientDescentOptimizer< TCostFunction >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef QuaternionRigidTransformGradientDescentOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef GradientDescentOptimizer<TCostFunction> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Dimension of the Search Space
   */
  enum { SpaceDimension = Superclass::SpaceDimension };
 

  /**
   *  Parameters type.
   *  It defines a position in the optimization search space
   */
  typedef typename Superclass::ParametersType ParametersType;

 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( QuaternionRigidTransformGradientDescentOptimizer, 
      GradientDescentOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);


  /**
   * Advance one step following the gradient direction
   */
  virtual void AdvanceOneStep( void );


protected:

  QuaternionRigidTransformGradientDescentOptimizer();
  virtual ~QuaternionRigidTransformGradientDescentOptimizer() {};
  QuaternionRigidTransformGradientDescentOptimizer(const Self&) {}
  void operator=(const Self&) {}


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuaternionRigidTransformGradientDescentOptimizer.txx"
#endif

#endif



