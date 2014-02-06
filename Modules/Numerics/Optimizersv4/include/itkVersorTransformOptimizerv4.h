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
#ifndef __itkVersorTransformOptimizerv4_h
#define __itkVersorTransformOptimizerv4_h

#include "itkGradientDescentOptimizerv4.h"

namespace itk
{
/** \class VersorTransformOptimizerv4
 * \brief Implement a gradient descent optimizer
 *
 * VersorTransformOptimizerv4 is a variant of the
 * gradient descent optimizer implmented in
 * GradientDescentOptimizerv4.
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
 * \sa GradientDescentOptimizerv4
 * \sa Versor
 * \sa VersorTransform
 *
 * \ingroup ITKOptimizersv4
 */
template<typename TInternalComputationValueType>
class VersorTransformOptimizerv4Template:
  public GradientDescentOptimizerv4Template<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef VersorTransformOptimizerv4Template      Self;
  typedef GradientDescentOptimizerv4Template
                  <TInternalComputationValueType> Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VersorTransformOptimizerv4Template, Superclass);

  /**  Versor Type  */
  typedef Versor< double >       VersorType;
  typedef VersorType::VectorType VectorType;

protected:
  /** Default constructor */
  VersorTransformOptimizerv4Template() {}

  /** Destructor */
  virtual ~VersorTransformOptimizerv4Template() {}

  /** Advance one Step following the gradient direction.
   * Includes transform update. */
  virtual void AdvanceOneStep(void);

  virtual void PrintSelf( std::ostream & os, Indent indent ) const;

private:
  VersorTransformOptimizerv4Template(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};

/** This helps to meet backward compatibility */
typedef VersorTransformOptimizerv4Template<double> VersorTransformOptimizerv4;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersorTransformOptimizerv4.hxx"
#endif

#endif
