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
#ifndef itkParticleSwarmOptimizerTestFunctions_h
#define itkParticleSwarmOptimizerTestFunctions_h

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "itkCommand.h"
#include "itkParticleSwarmOptimizerBase.h"

namespace itk
{
/**
 * \class ParticleSwarmTestF1
 *
 * Function we want to optimize, comprised of two parabolas with C0 continuity
 * at 0:
 * f(x) = if(x<0) x^2+4x; else 2x^2-8x
 *
 * Minima are at -2 and 2 with function values of -4 and -8 respectively.
 */
class ParticleSwarmTestF1 : public SingleValuedCostFunction
{
public:
  using Self = ParticleSwarmTestF1;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(ParticleSwarmTestF1, SingleValuedCostFunction);

  using ParametersType = Superclass::ParametersType;
  using MeasureType = Superclass::MeasureType;

  ParticleSwarmTestF1() = default;

  double
  GetValue(const ParametersType & parameters) const override
  {
    double val;

    if (parameters[0] < 0)
    {
      val = parameters[0] * parameters[0] + 4 * parameters[0];
    }
    else
    {
      val = 2 * parameters[0] * parameters[0] - 8 * parameters[0];
    }
    return val;
  }

  void
  GetDerivative(const ParametersType & itkNotUsed(parameters), DerivativeType & itkNotUsed(derivative)) const override
  {
    throw ExceptionObject(__FILE__, __LINE__, "no derivative available");
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return 1;
  }
};

/**
 *  Function we want to optimize, quadratic:
 *
 *  1/2 x^T A x - b^T x
 *
 * where A = [ 3 2 ]  and b = [ 2 ]
 *           [ 2 6 ]          [-8 ]
 *
 * solution is [ 2 ] with a function value of 10.0
 *             [-2 ]
 *
 */
class ParticleSwarmTestF2 : public SingleValuedCostFunction
{
public:
  using Self = ParticleSwarmTestF2;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(ParticleSwarmTestF2, SingleValuedCostFunction);

  using ParametersType = Superclass::ParametersType;
  using DerivativeType = Superclass::DerivativeType;
  using MeasureType = Superclass::MeasureType;

  using VectorType = vnl_vector<double>;
  using MatrixType = vnl_matrix<double>;

  ParticleSwarmTestF2()
    : m_A(2, 2)
    , m_Intercept(2)
  {
    m_A[0][0] = 3;
    m_A[0][1] = 2;
    m_A[1][0] = 2;
    m_A[1][1] = 6;

    m_Intercept[0] = 2;
    m_Intercept[1] = -8;
  }

  double
  GetValue(const ParametersType & parameters) const override
  {
    return 0.5 * (m_A(0, 0) * parameters[0] * parameters[0] + m_A(0, 1) * parameters[0] * parameters[1] +
                  m_A(1, 0) * parameters[0] * parameters[1] + m_A(1, 1) * parameters[1] * parameters[1]) -
           m_Intercept[0] * parameters[0] - m_Intercept[1] * parameters[1];
  }

  void
  GetDerivative(const ParametersType & itkNotUsed(parameters), DerivativeType & itkNotUsed(derivative)) const override
  {
    throw ExceptionObject(__FILE__, __LINE__, "no derivative available");
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return 2;
  }

private:
  MatrixType m_A;
  VectorType m_Intercept;
};

/**
 * \class ParticleSwarmTestF3
 * The Rosenbrock function f(x,y) = (1-x)^2 + 100(y-x^2)^2
 * minimum is at (1,1) with f(x,y) = 0.
 */
class ParticleSwarmTestF3 : public SingleValuedCostFunction
{
public:
  using Self = ParticleSwarmTestF3;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  itkNewMacro(Self);
  itkTypeMacro(ParticleSwarmTestF3, SingleValuedCostFunction);

  using ParametersType = Superclass::ParametersType;
  using MeasureType = Superclass::MeasureType;

  ParticleSwarmTestF3() = default;

  double
  GetValue(const ParametersType & parameters) const override
  {
    return (1 - parameters[0]) * (1 - parameters[0]) +
           100 * (parameters[1] - parameters[0] * parameters[0]) * (parameters[1] - parameters[0] * parameters[0]);
  }

  void
  GetDerivative(const ParametersType & itkNotUsed(parameters), DerivativeType & itkNotUsed(derivative)) const override
  {
    throw ExceptionObject(__FILE__, __LINE__, "no derivative available");
  }

  unsigned int
  GetNumberOfParameters() const override
  {
    return 2;
  }
};

class CommandIterationUpdateParticleSwarm : public Command
{
public:
  using Self = CommandIterationUpdateParticleSwarm;
  using Superclass = Command;
  using Pointer = SmartPointer<Self>;
  itkNewMacro(Self);

  void
  Reset()
  {
    m_IterationNumber = 0;
  }

  itkSetMacro(PrintOptimizer, bool);

  void
  Execute(Object * caller, const EventObject & event) override
  {
    Execute((const Object *)caller, event);
  }

  void
  Execute(const Object * object, const EventObject & event) override
  {
    const auto * optimizer = static_cast<const ParticleSwarmOptimizerBase *>(object);

    if (dynamic_cast<const IterationEvent *>(&event) != nullptr || dynamic_cast<const StartEvent *>(&event) != nullptr)
    {
      std::cout << m_IterationNumber++ << ":  ";
      std::cout << "x: " << optimizer->GetCurrentPosition() << "  ";
      std::cout << "f(x): " << optimizer->GetValue() << std::endl;
      if (m_PrintOptimizer)
      {
        ParticleSwarmOptimizerBase::Pointer optimizerPtr = const_cast<ParticleSwarmOptimizerBase *>(optimizer);
        std::cout << optimizerPtr;
      }
    }
  }

protected:
  CommandIterationUpdateParticleSwarm()
  {
    m_IterationNumber = 0;
    m_PrintOptimizer = false;
  }

private:
  unsigned long m_IterationNumber;
  bool          m_PrintOptimizer;
};

} // namespace itk
#endif // itkParticleSwarmOptimizerTestFunctions_h
