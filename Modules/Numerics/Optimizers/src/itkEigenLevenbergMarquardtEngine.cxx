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

#include "itkEigenLevenbergMarquardtEngine.h"

#include "itk_eigen.h"
#include "itkeigen/Eigen/Core"
#include "itkeigen/unsupported/Eigen/NonLinearOptimization"
#include "itkeigen/unsupported/Eigen/NumericalDiff"

#include <vector>

namespace itk::detail
{
namespace
{
/** Adapts the raw-pointer residual/Jacobian callbacks to an Eigen functor. */
struct CallbackFunctor
{
  using Scalar = double;
  using InputType = Eigen::VectorXd;
  using ValueType = Eigen::VectorXd;
  using JacobianType = Eigen::MatrixXd;
  enum
  {
    InputsAtCompileTime = Eigen::Dynamic,
    ValuesAtCompileTime = Eigen::Dynamic
  };

  int                                                   m_NumberOfParameters;
  int                                                   m_NumberOfResiduals;
  const std::function<void(const double *, double *)> & m_Residual;
  const std::function<void(const double *, double *)> & m_Jacobian;

  int
  inputs() const
  {
    return m_NumberOfParameters;
  }
  int
  values() const
  {
    return m_NumberOfResiduals;
  }

  int
  operator()(const Eigen::VectorXd & x, Eigen::VectorXd & fvec) const
  {
    m_Residual(x.data(), fvec.data());
    return 0;
  }

  // Eigen's Jacobian is column-major; the callback fills row-major (m-by-n).
  int
  df(const Eigen::VectorXd & x, Eigen::MatrixXd & fjac) const
  {
    std::vector<double> rowMajor(static_cast<size_t>(m_NumberOfResiduals) * m_NumberOfParameters);
    m_Jacobian(x.data(), rowMajor.data());
    for (int i = 0; i < m_NumberOfResiduals; ++i)
    {
      for (int j = 0; j < m_NumberOfParameters; ++j)
      {
        fjac(i, j) = rowMajor[static_cast<size_t>(i) * m_NumberOfParameters + j];
      }
    }
    return 0;
  }
};

bool
StatusIsConverged(int status)
{
  // LevenbergMarquardtSpace::Status: 1..4 are the success codes
  // (RelativeReductionTooSmall, RelativeErrorTooSmall,
  //  RelativeErrorAndReductionTooSmall, CosinusTooSmall).
  return status >= 1 && status <= 4;
}
} // namespace

EigenLevenbergMarquardtResult
EigenLevenbergMarquardtSolve(unsigned int                                          numberOfParameters,
                             unsigned int                                          numberOfResiduals,
                             const std::function<void(const double *, double *)> & residual,
                             const std::function<void(const double *, double *)> & jacobian,
                             const vnl_vector<double> &                            initialPosition,
                             const EigenLevenbergMarquardtOptions &                options)
{
  Eigen::VectorXd x(numberOfParameters);
  for (unsigned int i = 0; i < numberOfParameters; ++i)
  {
    x[static_cast<Eigen::Index>(i)] = initialPosition[i];
  }

  CallbackFunctor functor{
    static_cast<int>(numberOfParameters), static_cast<int>(numberOfResiduals), residual, jacobian
  };

  int          status = 0;
  unsigned int numberOfEvaluations = 0;
  double       residualNorm = 0.0;

  if (jacobian)
  {
    // Analytic Jacobian -> lmder.
    Eigen::LevenbergMarquardt<CallbackFunctor> lm(functor);
    lm.parameters.maxfev = static_cast<int>(options.maxFunctionEvaluations);
    lm.parameters.xtol = options.xTolerance;
    lm.parameters.ftol = options.functionTolerance;
    lm.parameters.gtol = options.gradientTolerance;
    status = lm.minimize(x);
    numberOfEvaluations = static_cast<unsigned int>(lm.nfev);
    residualNorm = lm.fvec.norm();
  }
  else
  {
    // Forward-difference Jacobian -> lmdif.
    Eigen::NumericalDiff<CallbackFunctor>                            numericalDiff(functor, options.epsilonFunction);
    Eigen::LevenbergMarquardt<Eigen::NumericalDiff<CallbackFunctor>> lm(numericalDiff);
    lm.parameters.maxfev = static_cast<int>(options.maxFunctionEvaluations);
    lm.parameters.xtol = options.xTolerance;
    lm.parameters.ftol = options.functionTolerance;
    lm.parameters.gtol = options.gradientTolerance;
    status = lm.minimize(x);
    numberOfEvaluations = static_cast<unsigned int>(lm.nfev);
    residualNorm = lm.fvec.norm();
  }

  EigenLevenbergMarquardtResult result;
  result.solution = vnl_vector<double>(numberOfParameters);
  for (unsigned int i = 0; i < numberOfParameters; ++i)
  {
    result.solution[i] = x[static_cast<Eigen::Index>(i)];
  }
  result.status = status;
  result.converged = StatusIsConverged(status);
  result.numberOfEvaluations = numberOfEvaluations;
  result.residualNorm = residualNorm;
  return result;
}
} // namespace itk::detail
