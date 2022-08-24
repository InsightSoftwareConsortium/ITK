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

#ifndef itkSphereParametersEstimator_hxx
#define itkSphereParametersEstimator_hxx

#include "itkSphereParametersEstimator.h"
#include <vnl/algo/vnl_levenberg_marquardt.h>
#include <vnl/algo/vnl_matrix_inverse.h>

namespace itk
{

template <unsigned int Dimension>
SphereParametersEstimator<Dimension>::SphereParametersEstimator()
{
  this->delta = NumericTraits<double>::min();
  this->lsType = GEOMETRIC;
  this->minForEstimate = Dimension + 1;
}


template <unsigned int Dimension>
SphereParametersEstimator<Dimension>::~SphereParametersEstimator()
{}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::SetLeastSquaresType(LeastSquaresType lsType)
{
  if (lsType != ALGEBRAIC && lsType != GEOMETRIC)
    throw ExceptionObject(__FILE__, __LINE__, "Invalid value for least squares type.");
  this->lsType = lsType;
}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::SetDelta(double delta)
{
  if (delta < 0)
    throw ExceptionObject(__FILE__, __LINE__, "Invalid setting for delta, must be >0.");
  this->delta = delta;
}


template <unsigned int Dimension>
double
SphereParametersEstimator<Dimension>::GetDelta()
{
  return this->delta;
}

/*
 * Given k+1 points we compute the (hyper)sphere that goes through them.
 * We know that (p_1-center)^2 = (p_2-center)^2 =...= (p_{k+1}-center)^2 = r^2
 * This gives us k linearly independent equations in k unknowns (center_1,center_2,...,center_k):
 * (p_1-center)^2 = (p_2-center)^2
 * (p_1-center)^2 = (p_3-center)^2
 *                :
 * (p_1-center)^2 = (p_{k+1}-center)^2,
 * after simplification we get:
 *   [(p_11 - p_21)       (p_12 - p_22)     ...  (p_1k - p_2k)    ] [center_1]         [(p_11^2 - p_21^2)     + (p_12^2
 * - p_22^2)     + ... + (p_1k^2 - p_2k^2)    ]
 *   [(p_11 - p_31)       (p_12 - p_32)     ...  (p_1k - p_3k)    ] [center_2]  = 0.5* [(p_11^2 - p_31^2)     + (p_12^2
 * - p_32^2)     + ... + (p_1k^2 - p_3k^2)    ] [      :                   :            :         :          ] [   : ]
 * [        :                         :                 :           :            ]
 *   [(p_11 - p_{k+1}1)   (p_12 - p_{k+1}2) ...  (p_1k - p_{k+1}k)] [center_k]         [(p_11^2 - p_{k+1}1^2) +  (p_12^2
 * - p_{k+1}2^2)  + ... + (p_1k^2 - p_{k+1}k^2)]
 *
 *
 * Solving this set of linear equations yields the (hyper)sphere center, after which the radius is computed as
 * sqrt((center-p_1)^T(center-p_1)).
 *
 * For Dimension = 2, a circle, and Dimension = 3, a sphere we solve the linear equations (compute the matrix inverse)
 * using Cramer's rule: A^-1 = C^T/det(A) where C is the
 * cofactor matrix, C_ij = -1^(i+j)detM_ij.
 *
 * In 2D we get:
 *     [a  b]                            [d  -b]
 * A =         we get A^-1 = 1/(ad - bc) [     ]
 *     [c  d]                            [-c  a]
 *
 * In 3D we get:
 *     [a  b  c]                                                        [ei-fh  ch-bi  bf-ce]
 * A = [d  e  f]     we get A^-1 = 1/(a(ei-fh) - b(di-fg) + c(dh-eg))   [fg-di  ai-cg  cd-af]
 *     [g  h  i]                                                        [dh-eg  bg-ah  ae-bd]
 */
template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::Estimate(std::vector<Point<double, Dimension> *> & data,
                                               std::vector<double> &                     parameters)
{
  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;

  switch (Dimension)
  {
    case CIRCLE:
      Estimate2D(data, parameters);
      break;
    case SPHERE:
      Estimate3D(data, parameters);
      break;
    default: // hyper-sphere
      EstimateND(data, parameters);
      break;
  }
}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::Estimate(std::vector<Point<double, Dimension>> & data,
                                               std::vector<double> &                   parameters)
{
  std::vector<Point<double, Dimension> *> usedData;
  int                                     dataSize = data.size();
  for (int i = 0; i < dataSize; i++)
    usedData.push_back(&(data[i]));
  Estimate(usedData, parameters);
}


/*
 * See the Estimate() method for explanation.
 */
template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::Estimate2D(std::vector<Point<double, Dimension> *> & data,
                                                 std::vector<double> &                     parameters)
{
  const double EPS = 2 * NumericTraits<double>::epsilon();
  double       A00, A01, A10, A11;
  double       b0, b1;
  double       detA;

  Point<double, Dimension> & p0 = (*data[0]);
  Point<double, Dimension> & p1 = (*data[1]);
  Point<double, Dimension> & p2 = (*data[2]);

  A00 = p0[0] - p1[0];
  A01 = p0[1] - p1[1];
  A10 = p0[0] - p2[0];
  A11 = p0[1] - p2[1];
  detA = (A00 * A11 - A01 * A10);
  // the three points are colinear (A is singular, det(A)<SPHERE_EPS)
  if (fabs(detA) < EPS)
    return;
  detA *= 2.0;

  b0 = A00 * (p0[0] + p1[0]) + A01 * (p0[1] + p1[1]);
  b1 = A10 * (p0[0] + p2[0]) + A11 * (p0[1] + p2[1]);

  // centerX
  parameters.push_back((A11 * b0 - A01 * b1) / detA);
  // centerY
  parameters.push_back((A00 * b1 - A10 * b0) / detA);
  // r
  parameters.push_back(sqrt(((*data[0])[0] - parameters[0]) * ((*data[0])[0] - parameters[0]) +
                            ((*data[0])[1] - parameters[1]) * ((*data[0])[1] - parameters[1])));
}


/*
 * See the Estimate() method for explanation.
 */
template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::Estimate3D(std::vector<Point<double, Dimension> *> & data,
                                                 std::vector<double> &                     parameters)
{
  const double EPS = 2 * NumericTraits<double>::epsilon();
  double       A00, A01, A02, A10, A11, A12, A20, A21, A22;
  double       CT00, CT01, CT02, CT10, CT11, CT12, CT20, CT21, CT22;
  double       b0, b1, b2;
  double       detA;

  Point<double, Dimension> & p0 = (*data[0]);
  Point<double, Dimension> & p1 = (*data[1]);
  Point<double, Dimension> & p2 = (*data[2]);
  Point<double, Dimension> & p3 = (*data[3]);

  A00 = p0[0] - p1[0];
  A01 = p0[1] - p1[1];
  A02 = p0[2] - p1[2];
  A10 = p0[0] - p2[0];
  A11 = p0[1] - p2[1];
  A12 = p0[2] - p2[2];
  A20 = p0[0] - p3[0];
  A21 = p0[1] - p3[1];
  A22 = p0[2] - p3[2];

  CT00 = A11 * A22 - A12 * A21;
  CT10 = A12 * A20 - A10 * A22;
  CT20 = A10 * A21 - A11 * A20;

  detA = A00 * CT00 + A01 * CT10 + A02 * CT20;
  if (fabs(detA) < EPS) // the four points are coplanar
    return;
  detA *= 2;

  CT01 = A02 * A21 - A01 * A22;
  CT11 = A00 * A22 - A02 * A20;
  CT21 = A01 * A20 - A00 * A21;

  CT02 = A01 * A12 - A02 * A11;
  CT12 = A02 * A10 - A00 * A12;
  CT22 = A00 * A11 - A01 * A10;

  b0 = A00 * (p0[0] + p1[0]) + A01 * (p0[1] + p1[1]) + A02 * (p0[2] + p1[2]);
  b1 = A10 * (p0[0] + p2[0]) + A11 * (p0[1] + p2[1]) + A12 * (p0[2] + p2[2]);
  b2 = A20 * (p0[0] + p3[0]) + A21 * (p0[1] + p3[1]) + A22 * (p0[2] + p3[2]);

  // centerX
  parameters.push_back((CT00 * b0 + CT01 * b1 + CT02 * b2) / detA);
  // centerY
  parameters.push_back((CT10 * b0 + CT11 * b1 + CT12 * b2) / detA);
  // centerZ
  parameters.push_back((CT20 * b0 + CT21 * b1 + CT22 * b2) / detA);
  // r
  parameters.push_back(sqrt((((*data[0])[0] - parameters[0]) * ((*data[0])[0] - parameters[0])) +
                            (((*data[0])[1] - parameters[1]) * ((*data[0])[1] - parameters[1])) +
                            (((*data[0])[2] - parameters[2]) * ((*data[0])[2] - parameters[2]))));
}


/*
 * See the Estimate() method for explanation.
 */
template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::EstimateND(std::vector<Point<double, Dimension> *> & data,
                                                 std::vector<double> &                     parameters)
{
  const double       EPS = 2 * NumericTraits<double>::epsilon();
  double             rSquared;
  unsigned int       i, j, index;
  vnl_matrix<double> A(Dimension, Dimension);
  vnl_vector<double> b(Dimension, 0);
  vnl_vector<double> x(Dimension);

  // create the matrix A
  Point<double, Dimension> & p0 = (*data[0]);
  for (i = 0, index = 1; i < Dimension; i++, index++)
  {
    for (j = 0; j < Dimension; j++)
    {
      Point<double, Dimension> & p = (*data[index]);
      A(i, j) = p0[j] - p[j];
      b[i] += A(i, j) * (p0[j] + p[j]);
    }
  }

  vnl_matrix_inverse<double> Ainv(A);
  // explicitly zero out small singular values
  // this is ugly as it exposes that the inverse is computed via SVD
  Ainv.zero_out_absolute(EPS);

  // points are coplanar
  if (Ainv.rank() < Dimension)
    return;
  // solve the equation system
  x = Ainv * b * 0.5;

  rSquared = 0.0;
  for (i = 0; i < Dimension; i++)
  {
    parameters.push_back(x[i]);
    rSquared += (p0[i] - x[i]) * (p0[i] - x[i]);
  }
  parameters.push_back(sqrt(rSquared));
}


/*
 * Check that |sqrt((p-center)^T(p-center)) - r| < delta
 */
template <unsigned int Dimension>
bool
SphereParametersEstimator<Dimension>::Agree(std::vector<double> & parameters, Point<double, Dimension> & data)
{
  double delta = 0;
  for (unsigned int i = 0; i < Dimension; i++)
    delta += ((data[i] - parameters[i]) * (data[i] - parameters[i]));
  delta = fabs(sqrt(delta)) - parameters[Dimension];

  return delta < this->delta;
}

template <unsigned int Dimension>
std::vector<bool>
SphereParametersEstimator<Dimension>::AgreeMultiple(std::vector<double> &                   parameters,
                                                    std::vector<Point<double, Dimension>> & data)
{
  std::vector<bool> output;
  for (unsigned int i = 0; i < data.size(); ++i)
  {
    output.push_back(this->Agree(parameters, data[i]));
  }

  return output;
}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::AlgebraicLeastSquaresEstimate(std::vector<Point<double, Dimension> *> & data,
                                                                    std::vector<double> & parameters)
{
  const double EPS = 2 * NumericTraits<double>::epsilon();
  parameters.clear();
  if (data.size() < this->minForEstimate) // not enough points for estimate
    return;

  unsigned int       numPoints = data.size();
  vnl_matrix<double> A(numPoints, Dimension + 1);
  vnl_vector<double> x(Dimension + 1);
  vnl_vector<double> b(numPoints, 0);
  unsigned int       i, j;

  for (i = 0; i < numPoints; i++)
  {
    for (j = 0; j < Dimension; j++)
    {
      A[i][j] = -2 * (*data[i])[j];
      b[i] += -((*data[i])[j] * (*data[i])[j]);
    }
    A[i][Dimension] = 1;
  }

  vnl_matrix_inverse<double> Ainv(A);
  // explicitly zero out small singular values
  // this is ugly as it exposes that the inverse is computed via SVD
  Ainv.zero_out_absolute(EPS);
  if (Ainv.rank() < this->minForEstimate) // all the points are collinear
    return;
  x = Ainv * b;


  double rSquared = -x[Dimension];
  for (i = 0; i < Dimension; i++)
  {
    parameters.push_back(x[i]);
    rSquared += x[i] * x[i];
  }
  // equation system admits solutions that aren't actual circles
  if (rSquared > 0)
    parameters.push_back(sqrt(rSquared));
  else
    parameters.clear();
}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::LeastSquaresEstimate(std::vector<Point<double, Dimension> *> & data,
                                                           std::vector<double> &                     parameters)
{
  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;

  std::vector<double> initialParameters;
  switch (this->lsType)
  {
    case ALGEBRAIC:
      AlgebraicLeastSquaresEstimate(data, parameters);
      break;
    case GEOMETRIC:
      // algebraic least squares for initial estimate
      AlgebraicLeastSquaresEstimate(data, initialParameters);
      if (initialParameters.size() == 0) // the points are coplanar
        return;
      GeometricLeastSquaresEstimate(data, initialParameters, parameters);
      break;
  }
}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::LeastSquaresEstimate(std::vector<Point<double, Dimension>> & data,
                                                           std::vector<double> &                   parameters)
{
  std::vector<Point<double, Dimension> *> usedData;
  int                                     dataSize = data.size();
  for (int i = 0; i < dataSize; i++)
    usedData.push_back(&(data[i]));
  LeastSquaresEstimate(usedData, parameters);
}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::GeometricLeastSquaresEstimate(std::vector<Point<double, Dimension> *> & data,
                                                                    std::vector<double> & initialParameters,
                                                                    std::vector<double> & finalParameters)
{
  vnl_vector<double> parameters(Dimension + 1);
  unsigned int       i;
  for (i = 0; i <= Dimension; i++)
    parameters[i] = initialParameters[i];

  SphereParametersEstimator::SumSquaresSpherePointsDistanceFunction optimizedFunction(&data);
  vnl_levenberg_marquardt                                           lmOptimization(optimizedFunction);

  double gradTolerance = 10e-16;
  double parametersChangeTolerance = 10e-16;
  int    maxIterations = 500;

  lmOptimization.set_x_tolerance(parametersChangeTolerance);
  lmOptimization.set_g_tolerance(gradTolerance);
  lmOptimization.set_max_function_evals(maxIterations);

  lmOptimization.minimize(parameters);

  finalParameters.clear();
  for (i = 0; i <= Dimension; i++)
    finalParameters.push_back(parameters[i]);
}


template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::GetDistanceStatistics(std::vector<double> &                   parameters,
                                                            std::vector<Point<double, Dimension>> & data,
                                                            std::vector<double> &                   distances,
                                                            double &                                min,
                                                            double &                                max,
                                                            double &                                mean)
{
  if (parameters.size() < Dimension + 1)
    throw ExceptionObject(__FILE__, __LINE__, "Number of (hyper) sphere parameters does not match Dimension.");
  unsigned int i, j, n;
  double       dist;

  // use first point for initialization
  dist = 0.0;
  for (j = 0; j < Dimension; j++)
    dist += ((data[0])[j] - parameters[j]) * ((data[0])[j] - parameters[j]);
  dist = fabs(sqrt(dist) - parameters[Dimension]);
  distances.push_back(dist);
  min = max = mean = dist;

  n = data.size();
  // go over the rest of the points
  for (i = 1; i < n; i++)
  {
    Point<double, Dimension> & pnt = data[i];
    dist = 0.0;
    for (j = 0; j < Dimension; j++)
      dist += (pnt[j] - parameters[j]) * (pnt[j] - parameters[j]);
    dist = fabs(sqrt(dist) - parameters[Dimension]);
    distances.push_back(dist);
    mean += dist;
    if (dist > max)
      max = dist;
    else if (dist < min)
      min = dist;
  }
  mean /= n;
}


template <unsigned int Dimension>
SphereParametersEstimator<Dimension>::SumSquaresSpherePointsDistanceFunction::SumSquaresSpherePointsDistanceFunction(
  std::vector<Point<double, Dimension> *> * data)
  : vnl_least_squares_function(Dimension + 1, data->size(), vnl_least_squares_function::use_gradient)
{
  this->data = data;
}


// x = [c_0,c_1,...c_k,r]
template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::SumSquaresSpherePointsDistanceFunction::f(const vnl_vector<double> & x,
                                                                                vnl_vector<double> &       fx)
{
  unsigned int i, j, numPoints = this->data->size();
  double       sqrVal;

  for (i = 0; i < numPoints; i++)
  {
    // compute sqrVal = (p - c)^T (p - c)
    sqrVal = 0.0;
    Point<double, Dimension> & pnt = *((*this->data)[i]);
    for (j = 0; j < Dimension; j++)
      sqrVal += (pnt[j] - x[j]) * (pnt[j] - x[j]);
    fx[i] = sqrt(sqrVal) - x[Dimension];
  }
}


// x = [c_0,c_1,...c_k,r]

template <unsigned int Dimension>
void
SphereParametersEstimator<Dimension>::SumSquaresSpherePointsDistanceFunction::gradf(const vnl_vector<double> & x,
                                                                                    vnl_matrix<double> &       jacobian)
{
  unsigned int i, j, numPoints = this->data->size();
  double       sqrtVal;

  for (i = 0; i < numPoints; i++)
  {
    // compute sqrtVal = sqrt((p - c)^T (p - c))
    sqrtVal = 0.0;
    Point<double, Dimension> & p = (*(*this->data)[i]);
    for (j = 0; j < Dimension; j++)
      sqrtVal += (p[j] - x[j]) * (p[j] - x[j]);
    sqrtVal = sqrt(sqrtVal);
    // the jacobian entries (c_i - p_i)/sqrtVal
    for (j = 0; j < Dimension; j++)
      jacobian[i][j] = (x[j] - p[j]) / sqrtVal;
    jacobian[i][Dimension] = -1;
  }
}

} // end namespace itk

#endif //_SPHERE_PARAM_ESTIMATOR_HXX_
