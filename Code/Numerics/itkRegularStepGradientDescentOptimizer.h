/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizer.h
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
#ifndef __itkRegularStepGradientDescentOptimizer_h
#define __itkRegularStepGradientDescentOptimizer_h

#include "itkRegularStepGradientDescentBaseOptimizer.h"

namespace itk
{
  
/** \class RegularStepGradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * \ingroup Numerics  Optimizers
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT RegularStepGradientDescentOptimizer : 
        public RegularStepGradientDescentBaseOptimizer< TCostFunction >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegularStepGradientDescentOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef RegularStepGradientDescentBaseOptimizer<TCostFunction> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   * Cost Function  typedef.
   */
  typedef          TCostFunction                CostFunctionType;
  typedef typename CostFunctionType::Pointer    CostFunctionPointer;

  /**
   * Dimension of the Search Space
   */
  enum { SpaceDimension = Superclass::SpaceDimension };
 

  /**
   *  Parameters type.
   *  it defines a position in the optimization search space
   */
  typedef typename Superclass::ParametersType ParametersType;


  /**
   *  Measure type.
   *  it defines a type used to return the cost function value 
   */
  typedef typename Superclass::MeasureType MeasureType;


  /**
   *  Derivative type.
   *  it defines a type used to return the cost function derivative 
   */
  typedef typename Superclass::DerivativeType DerivativeType;



 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( RegularStepGradientDescentOptimizer, 
                RegularStepGradientDescentBaseOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  


protected:

  RegularStepGradientDescentOptimizer() {};
  virtual ~RegularStepGradientDescentOptimizer() {};

  /**
   * Advance one step along the corrected gradient taking into
   * account the steplength represented by factor.
   * This method is invoked by AdvanceOneStep. It is expected
   * to be overrided by optimization methods in non-vector spaces
   * \sa AdvanceOneStep
   */
  virtual void StepAlongGradient( 
                  double factor, 
                  const DerivativeType & transformedGradient );

private:

  RegularStepGradientDescentOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegularStepGradientDescentOptimizer.txx"
#endif

#endif



