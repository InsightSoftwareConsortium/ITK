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
#ifndef itkSingleValuedVnlCostFunctionAdaptor_h
#define itkSingleValuedVnlCostFunctionAdaptor_h

#include "itkSingleValuedCostFunction.h"
#include "vnl/vnl_cost_function.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class SingleValuedVnlCostFunctionAdaptor
 * \brief This class is an Adaptor that allows to pass
 * itk::SingleValuedCostFunctions to vnl_optimizers expecting
 * a vnl_cost_function.
 *
 * This class returns a single valued.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT SingleValuedVnlCostFunctionAdaptor : public vnl_cost_function
{
public:
  /** InternalParametersType type alias. */
  using InternalParametersType = vnl_vector<double>;

  /** InternalMeasureType type alias. */
  using InternalMeasureType = double;

  /** InternalGradientType type alias. */
  using InternalDerivativeType = vnl_vector<double>;

  /** Parameters of the SingleValuedCostFunction */
  using ParametersType = SingleValuedCostFunction::ParametersType;

  /** Derivatives of the SingleValuedCostFunction */
  using DerivativeType = SingleValuedCostFunction::DerivativeType;

  /** Type of the SingleValuedCostFunction value */
  using MeasureType = SingleValuedCostFunction::MeasureType;

  /** Scales type alias */
  using ScalesType = Array<double>;

  /** Constructor with size */
  SingleValuedVnlCostFunctionAdaptor(unsigned int spaceDimension);

  /** Set the CostFunction deriving from SingleValuedCostFunction */
  void
  SetCostFunction(SingleValuedCostFunction * costFunction)
  {
    m_CostFunction = costFunction;
  }

  /** Get the CostFunction deriving from SingleValuedCostFunction */
  const SingleValuedCostFunction *
  GetCostFunction() const
  {
    return m_CostFunction;
  }

  /**  Delegate computation of the value to the CostFunction. */
  InternalMeasureType
  f(const InternalParametersType & inparameters) override;

  /**  Delegate computation of the gradient to the costFunction.  */
  void
  gradf(const InternalParametersType & inparameters, InternalDerivativeType & gradient) override;

  /**  Delegate computation of value and gradient to the costFunction.     */
  void
  compute(const InternalParametersType & x, InternalMeasureType * fun, InternalDerivativeType * g) override;

  /**  Convert external derivative measures into internal type   */
  void
  ConvertExternalToInternalGradient(const DerivativeType & input, InternalDerivativeType & output) const;

  /** Set current parameters scaling. */
  void
  SetScales(const ScalesType & scales);

  /** Set/Get Negate cost function. The purpose of this boolean flag is to make
   * possible to take certain VNL optimizers that are only minimizers, and use
   * them for maximizing functions. When the boolean flag is set to true, the
   * values returned by GetValue in the internal ITK cost function will be
   * multiplied by -1 before returning it in the f() function. Similar
   * operations will be done for the gradf() and compute() methods. When the
   * boolean flag is set to false, then the values returned by the ITK cost
   * function will be passed unchanged to the VNL optimizers. */
  void
  SetNegateCostFunction(bool flag);

  bool
  GetNegateCostFunction() const;

  void
  NegateCostFunctionOn()
  {
    m_NegateCostFunction = true;
  }
  void
  NegateCostFunctionOff()
  {
    m_NegateCostFunction = false;
  }

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

  SingleValuedCostFunction::Pointer m_CostFunction;
  bool                              m_ScalesInitialized;
  ScalesType                        m_InverseScales;
  bool                              m_NegateCostFunction;
  Object::Pointer                   m_Reporter;

  mutable MeasureType    m_CachedValue;
  mutable DerivativeType m_CachedDerivative;
  mutable ParametersType m_CachedCurrentParameters;
}; // end of Class CostFunction
} // end namespace itk

#endif
