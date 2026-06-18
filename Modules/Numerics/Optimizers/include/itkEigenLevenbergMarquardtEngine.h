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
#ifndef itkEigenLevenbergMarquardtEngine_h
#define itkEigenLevenbergMarquardtEngine_h

#include "ITKOptimizersExport.h"
#include "vnl/vnl_vector.h"
#include <functional>

namespace itk::detail
{
/** \brief Tolerances and limits for the Eigen Levenberg-Marquardt engine.
 *
 * Fields mirror the MINPACK lmdif/lmder parameters that vnl_levenberg_marquardt
 * also exposes, so the two backends accept identical settings.
 */
struct EigenLevenbergMarquardtOptions
{
  unsigned long maxFunctionEvaluations{ 2000 };
  double        xTolerance{ 1e-8 };
  double        gradientTolerance{ 1e-5 };
  double        functionTolerance{ 1e-8 };
  double        epsilonFunction{ 1e-11 };
};

/** \brief Result of an Eigen Levenberg-Marquardt minimization. */
struct EigenLevenbergMarquardtResult
{
  vnl_vector<double> solution;
  int                status{ 0 };
  bool               converged{ false };
  unsigned int       numberOfEvaluations{ 0 };
  double             residualNorm{ 0.0 };
};

/** \brief Solve a nonlinear least-squares problem with Eigen's MINPACK-port
 * Levenberg-Marquardt (unsupported/Eigen/NonLinearOptimization).
 *
 * This is the algorithm-identical counterpart to vnl_levenberg_marquardt
 * (both are ports of MINPACK lmdif/lmder). Eigen is confined to the
 * implementation; no Eigen type appears in this interface (see issue #6230).
 *
 * \param numberOfParameters  n, the size of x.
 * \param numberOfResiduals   m, the size of the residual vector.
 * \param residual            Fills m residuals from n parameters (raw pointers).
 * \param jacobian            Fills the m-by-n Jacobian, row-major, from n
 *                            parameters. If empty, forward differences are used.
 * \param initialPosition     Starting x (size n).
 * \param options             Tolerances / evaluation limit.
 */
ITKOptimizers_EXPORT EigenLevenbergMarquardtResult
EigenLevenbergMarquardtSolve(unsigned int                                          numberOfParameters,
                             unsigned int                                          numberOfResiduals,
                             const std::function<void(const double *, double *)> & residual,
                             const std::function<void(const double *, double *)> & jacobian,
                             const vnl_vector<double> &                            initialPosition,
                             const EigenLevenbergMarquardtOptions &                options);
} // namespace itk::detail

#endif
