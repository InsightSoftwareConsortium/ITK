/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransformOptimizer.h
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
#ifndef __itkVersorTransformOptimizer_h
#define __itkVersorTransformOptimizer_h

#include "itkRegularStepGradientDescentOptimizer.h"

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
template <class TCostFunction>
class ITK_EXPORT VersorTransformOptimizer : 
        public RegularStepGradientDescentBaseOptimizer< TCostFunction >
{
public:
  /** Standard class typedefs. */
  typedef VersorTransformOptimizer  Self;
  typedef RegularStepGradientDescentBaseOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( VersorTransformOptimizer, 
                RegularStepGradientDescentBaseOptimizer );

  /** Dimension of the Search Space */
  enum { SpaceDimension = Superclass::SpaceDimension };

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename Superclass::ParametersType ParametersType;
  typedef typename Superclass::DerivativeType DerivativeType;

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

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersorTransformOptimizer.txx"
#endif

#endif



