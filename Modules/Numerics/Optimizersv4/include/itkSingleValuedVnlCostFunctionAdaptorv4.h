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
#ifndef itkSingleValuedVnlCostFunctionAdaptorv4_h
#define itkSingleValuedVnlCostFunctionAdaptorv4_h

#include "itkOptimizerParameters.h"
#include "itkObjectToObjectMetricBase.h"
#include "vnl/vnl_cost_function.h"

namespace itk
{
/**
 *\class SingleValuedVnlCostFunctionAdaptorv4
 * \brief This class is an Adaptor that allows to pass
 * itk::ObjectToObjectMetricBase objects to vnl_optimizers expecting
 * a vnl_cost_function. For use in the ITKv4 registration framework.
 *
 * This class returns a single value.
 *
 * \ingroup ITKOptimizersv4
 */
class SingleValuedVnlCostFunctionAdaptorv4 : public vnl_cost_function
{
public:
  /** InternalMeasureType type alias. */
  using InternalMeasureType = double;

  /** InternalParametersType type alias. */
  using InternalParametersType = vnl_vector<InternalMeasureType>;

  /** InternalGradientType type alias. */
  using InternalDerivativeType = vnl_vector<InternalMeasureType>;

  /** Parameters of the SingleValuedCostFunction */
  using ParametersType = ObjectToObjectMetricBase::ParametersType;

  /** Derivatives of the SingleValuedCostFunction */
  using DerivativeType = ObjectToObjectMetricBase::DerivativeType;

  /** Type of the SingleValuedCostFunction value */
  using MeasureType = ObjectToObjectMetricBase::MeasureType;

  /** Scales type alias */
  using ScalesType = OptimizerParameters<InternalMeasureType>;

  /** Constructor with size */
  SingleValuedVnlCostFunctionAdaptorv4(unsigned int spaceDimension);

  /** Set the CostFunction deriving from SingleValuedCostFunction */
  void
  SetCostFunction(ObjectToObjectMetricBase * costFunction)
  {
    m_ObjectMetric = costFunction;
  }

  /** Get the CostFunction deriving from SingleValuedCostFunction */
  const ObjectToObjectMetricBase *
  GetCostFunction() const
  {
    return m_ObjectMetric;
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
  itkGetConstReferenceMacro(CachedValue, MeasureType);

  itkGetConstReferenceMacro(CachedDerivative, DerivativeType);

  /** Return the parameters directly from the assigned metric. */
  const ParametersType &
  GetCachedCurrentParameters() const;

protected:
  /** This method is intended to be called by the derived classes in order to
   * notify of an iteration event to any Command/Observers */
  void
  ReportIteration(const EventObject & event) const;

private:
  ObjectToObjectMetricBase::Pointer m_ObjectMetric;
  bool                              m_ScalesInitialized;
  ScalesType                        m_Scales;
  Object::Pointer                   m_Reporter;

  mutable MeasureType    m_CachedValue;
  mutable DerivativeType m_CachedDerivative;

}; // end of Class CostFunction

} // end namespace itk

#endif
