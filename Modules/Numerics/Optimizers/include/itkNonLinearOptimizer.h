/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkNonLinearOptimizer_h
#define __itkNonLinearOptimizer_h

#include "itkOptimizer.h"

namespace itk
{
/** \class NonLinearOptimizer
 * \brief Wrap of the vnl_nonlinear_minimizer to be adapted
 *
 * This class is provided to support the structure of an
 * Optimizers Hierarchy. It is not intended to be instantiated.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class NonLinearOptimizer:public Optimizer

{
public:
  /** Standard class typedefs. */
  typedef NonLinearOptimizer         Self;
  typedef Optimizer                  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonLinearOptimizer, Optimizer);

  /**  Types inherited from the superclass */
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::ScalesType     ScalesType;

protected:
  NonLinearOptimizer() {}
  virtual ~NonLinearOptimizer() {}

private:
  NonLinearOptimizer(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};
} // end namespace itk

#endif
