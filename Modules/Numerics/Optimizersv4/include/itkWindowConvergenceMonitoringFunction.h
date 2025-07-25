/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkWindowConvergenceMonitoringFunction_h
#define itkWindowConvergenceMonitoringFunction_h

#include "itkConvergenceMonitoringFunction.h"

namespace itk::Function
{
/**
 * \class WindowConvergenceMonitoringFunction
 * \brief Class which monitors convergence during the course of optimization.
 *
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKOptimizersv4
 */

template <typename TScalar = double>
class ITK_TEMPLATE_EXPORT WindowConvergenceMonitoringFunction : public ConvergenceMonitoringFunction<TScalar, TScalar>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(WindowConvergenceMonitoringFunction);

  using Self = WindowConvergenceMonitoringFunction;
  using Superclass = ConvergenceMonitoringFunction<TScalar, TScalar>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(WindowConvergenceMonitoringFunction);

  using ScalarType = TScalar;
  using RealType = typename NumericTraits<ScalarType>::RealType;

  using typename Superclass::EnergyValueType;
  using typename Superclass::EnergyValueContainerType;
  using typename Superclass::EnergyValueContainerSizeType;
  using EnergyValueIterator = typename EnergyValueContainerType::iterator;
  using EnergyValueConstIterator = typename EnergyValueContainerType::const_iterator;

  /** Add energy value */
  void
  AddEnergyValue(const EnergyValueType) override;

  /* Clear energy values and set total energy to 0 */
  void
  ClearEnergyValues() override;

  /** Set/Get window size over which the convergence value is calculated */
  /** @ITKStartGrouping */
  itkSetMacro(WindowSize, EnergyValueContainerSizeType);
  itkGetConstMacro(WindowSize, EnergyValueContainerSizeType);
  /** @ITKEndGrouping */
  /** Calculate convergence value by fitting to a window of the energy profile */
  RealType
  GetConvergenceValue() const override;

protected:
  WindowConvergenceMonitoringFunction();

  ~WindowConvergenceMonitoringFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  EnergyValueContainerSizeType m_WindowSize{};

  RealType m_TotalEnergy{};
};
} // namespace itk::Function

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWindowConvergenceMonitoringFunction.hxx"
#endif

#endif
