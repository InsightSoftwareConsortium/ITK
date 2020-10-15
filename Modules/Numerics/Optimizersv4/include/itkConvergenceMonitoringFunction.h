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
#ifndef itkConvergenceMonitoringFunction_h
#define itkConvergenceMonitoringFunction_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#include "itkNumericTraits.h"

#include <deque>

namespace itk
{
namespace Function
{
/**
 * \class ConvergenceMonitoringFunction
 * \brief Abstract base class which monitors convergence during the course of optimization.
 *
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TScalar, typename TEnergyValue>
class ConvergenceMonitoringFunction : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConvergenceMonitoringFunction);

  using Self = ConvergenceMonitoringFunction;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConvergenceMonitoringFunction, Object);

  using ScalarType = TScalar;
  using RealType = typename NumericTraits<ScalarType>::RealType;

  using EnergyValueType = TEnergyValue;
  using EnergyValueContainerType = std::deque<EnergyValueType>;
  using EnergyValueContainerSizeType = typename EnergyValueContainerType::size_type;
  using EnergyValueIterator = typename EnergyValueContainerType::iterator;
  using EnergyValueConstIterator = typename EnergyValueContainerType::const_iterator;

  /* Add energy value to the end of the profile. */
  virtual void
  AddEnergyValue(const EnergyValueType value)
  {
    itkDebugMacro("Adding energy value " << value);
    this->m_EnergyValues.push_back(value);
    this->Modified();
  }

  /* Get the current number of energy values. */
  EnergyValueContainerSizeType
  GetNumberOfEnergyValues() const
  {
    return this->m_EnergyValues.size();
  }

  /** Clear all the energy values. */
  virtual void
  ClearEnergyValues()
  {
    if (this->GetNumberOfEnergyValues() > 0)
    {
      itkDebugMacro("Clearing energy values.");
      this->m_EnergyValues.clear();
      this->Modified();
    }
  }

  /** Derived classes are responsible for defining the convergence value calculation */
  virtual RealType
  GetConvergenceValue() const = 0;

protected:
  ConvergenceMonitoringFunction() { this->m_EnergyValues.clear(); }

  ~ConvergenceMonitoringFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);

    os << std::endl << "Energy values: " << std::flush;

    auto it = this->m_EnergyValues.begin();
    while (it != this->m_EnergyValues.end())
    {
      os << "(" << it - this->m_EnergyValues.begin() << "): " << *it << " ";
      ++it;
    }
    os << std::endl;
  }

  EnergyValueContainerType m_EnergyValues;
};
} // end namespace Function
} // end namespace itk

#endif
