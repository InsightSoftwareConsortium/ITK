/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMultipleValuedVnlCostFunctionAdaptor_h
#define itkMultipleValuedVnlCostFunctionAdaptor_h

#include "itkMultipleValuedCostFunction.h"
#include "vnl/vnl_least_squares_function.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class MultipleValuedVnlCostFunctionAdaptor
 * \brief This class is an Adaptor that allows to pass
 * itk::MultipleValuedCostFunctions to vnl_optimizers expecting
 * a vnl_cost_function.
 *
 * This class returns a single valued.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT MultipleValuedVnlCostFunctionAdaptor : public vnl_least_squares_function
{
public:
  /** InternalParametersType type alias. */
  using InternalParametersType = vnl_vector<double>;

  /** InternalMeasureType type alias. */
  using InternalMeasureType = vnl_vector<double>;

  /** InternalGradientType type alias. */
  using InternalDerivativeType = vnl_matrix<double>;

  /** MeasureType of the MultipleValuedCostFunction */
  using MeasureType = MultipleValuedCostFunction::MeasureType;

  /** Parameters of the MultipleValuedCostFunction */
  using ParametersType = MultipleValuedCostFunction::ParametersType;

  /** Derivatives of the MultipleValuedCostFunction */
  using DerivativeType = MultipleValuedCostFunction::DerivativeType;

  /** Scales type alias */
  using ScalesType = Array<double>;

  /** Constructor with size */
  MultipleValuedVnlCostFunctionAdaptor(unsigned int spaceDimension, unsigned int numberOfValues);

  /** Set the CostFunction deriving from MultipleValuedCostFunction */
  void
  SetCostFunction(MultipleValuedCostFunction * costFunction)
  {
    m_CostFunction = costFunction;
  }

  /** Get the CostFunction deriving from MultipleValuedCostFunction */
  const MultipleValuedCostFunction *
  GetCostFunction() const
  {
    return m_CostFunction;
  }

  /**  Delegate computation of the value to the CostFunction. */
  void
  f(const InternalParametersType & inparameters, InternalMeasureType & measures) override;

  /**  Delegate computation of the gradient to the costFunction.  */
  void
  gradf(const InternalParametersType & inparameters, InternalDerivativeType & gradient) override;

  /**  Delegate computation of value and gradient to the costFunction.     */
  virtual void
  compute(const InternalParametersType & x, InternalMeasureType * ff, InternalDerivativeType * g);

  /**  Convert external derivative measures  into internal type */
  void
  ConvertExternalToInternalGradient(const DerivativeType & input, InternalDerivativeType & output);

  /**  Convert external measures  into internal type */
  void
  ConvertExternalToInternalMeasures(const MeasureType & input, InternalMeasureType & output);

  /**  Define if the Cost function should provide a customized
       Gradient computation or the gradient can be computed internally
       using a default approach  */
  void
  SetUseGradient(bool);

  void
  UseGradientOn()
  {
    this->SetUseGradient(true);
  }
  void
  UseGradientOff()
  {
    this->SetUseGradient(false);
  }
  bool
  GetUseGradient() const;

  /** Set current parameters scaling. */
  void
  SetScales(const ScalesType & scales);

  /** This AddObserver method allows to simulate that this class derives from
   * an itkObject for the purpose of reporting iteration events. The goal of
   * this method is to allow ITK-vnl optimizer adaptors to get iteration events
   * despite the fact that VNL does not provide callbacks. */
  unsigned long
  AddObserver(const EventObject & event, Command *) const;

  /** Return the value of the last evaluation to the value of the cost function.
   *  Note that this method DOES NOT triggers a computation of the function or
   *  the derivatives, it only returns previous values. Therefore the values here
   *  are only valid after you invoke the f() or gradf() methods. */
  const MeasureType &
  GetCachedValue() const;

  const DerivativeType &
  GetCachedDerivative() const;

  const ParametersType &
  GetCachedCurrentParameters() const;

protected:
  /** This method is intended to be called by the derived classes in order to
   * notify of an iteration event to any Command/Observers */
  void
  ReportIteration(const EventObject & event) const;

private:
  /** Get current parameters scaling. */
  itkGetConstReferenceMacro(InverseScales, ScalesType);

  MultipleValuedCostFunction::Pointer m_CostFunction;

  bool            m_ScalesInitialized;
  ScalesType      m_InverseScales;
  Object::Pointer m_Reporter;

  mutable MeasureType    m_CachedValue;
  mutable DerivativeType m_CachedDerivative;
  mutable ParametersType m_CachedCurrentParameters;
}; // end of Class CostFunction
} // end namespace itk

#endif
