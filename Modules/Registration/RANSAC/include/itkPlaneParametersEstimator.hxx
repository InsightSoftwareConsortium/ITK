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

#ifndef itkPlaneParametersEstimator_hxx
#define itkPlaneParametersEstimator_hxx

#include "itkPlaneParametersEstimator.h"
#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>

namespace itk
{

template <unsigned int Dimension>
PlaneParametersEstimator<Dimension>::PlaneParametersEstimator()
{
  this->deltaSquared = NumericTraits<double>::min();
  this->minForEstimate = Dimension;
}


template <unsigned int Dimension>
PlaneParametersEstimator<Dimension>::~PlaneParametersEstimator()
{}


template <unsigned int Dimension>
void
PlaneParametersEstimator<Dimension>::SetDelta(double delta)
{
  this->deltaSquared = delta * delta;
}


template <unsigned int Dimension>
double
PlaneParametersEstimator<Dimension>::GetDelta()
{
  return sqrt(this->deltaSquared);
}


/*
 * Estimate the plane parameters  [n_0,...,n_k,a_0,...,a_k], note that point
 * Dimension is k+1.
 * The plane is given as dot(n,p-a) = dot(n,p) - dot(n,a) = 0
 * The second dot product is a constant, d, so each point gives us one
 * equation in the equation system (Ax=0):
 *                        [n_0]
 *                          .
 *       [p_0,...,p_k,-1]   .   =  0
 *                          .
 *                        [n_k]
 *                        [ d ]
 *
 * If all k+1 points are linearly independent then the matrix A has a one
 * Dimensional null space [A is a (k+1)X(k+2) matrix] which is the answer we
 * seek.
 *
 */
template <unsigned int Dimension>
void
PlaneParametersEstimator<Dimension>::Estimate(std::vector<Point<double, Dimension> *> & data,
                                              std::vector<double> &                     parameters)
{
  unsigned int i, j;
  double       norm;
  const double EPS = 2 * NumericTraits<double>::epsilon();

  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;

  if (Dimension == 3)
  { // compute plane normal directly
    double             nx, ny, nz;
    vnl_vector<double> v1(3), v2(3);

    v1[0] = (*data[1])[0] - (*data[0])[0];
    v1[1] = (*data[1])[1] - (*data[0])[1];
    v1[2] = (*data[1])[2] - (*data[0])[2];
    v2[0] = (*data[2])[0] - (*data[0])[0];
    v2[1] = (*data[2])[1] - (*data[0])[1];
    v2[2] = (*data[2])[2] - (*data[0])[2];

    nx = v1[1] * v2[2] - v1[2] * v2[1];
    ny = v1[2] * v2[0] - v1[0] * v2[2];
    nz = v1[0] * v2[1] - v1[1] * v2[0];
    norm = sqrt(nx * nx + ny * ny + nz * nz);

    if (norm < EPS) // points are collinear
      return;
    parameters.push_back(nx / norm);
    parameters.push_back(ny / norm);
    parameters.push_back(nz / norm);
  }
  else
  { // get the plane normal as the null space of the matrix described above
    vnl_matrix<double> A(this->minForEstimate, this->minForEstimate + 1);

    for (i = 0; i < this->minForEstimate; i++)
    {
      Point<double, Dimension> & pnt = *(data[i]);
      for (j = 0; j < this->minForEstimate; j++)
        A(i, j) = pnt[j];
      A(i, j) = -1;
    }

    vnl_svd<double> svdA(A);
    // explicitly zero out small singular values
    svdA.zero_out_absolute(EPS);
    // the points are linearly dependent, need at least k linearly
    // independent points (gives us rank(A)=k)
    if (svdA.rank() < this->minForEstimate)
      return;

    // the one Dimensional null space of A is the solution we seek
    vnl_vector<double> x(this->minForEstimate + 1);
    x = svdA.nullvector();
    // get the (hyper)plane normal, we need to set it so ||n||=1, this
    // means we need to scale our solution to be
    // 1/||n_computed||*[n_computed,d] which is also a solution to the
    // equation system.
    norm = 0;
    for (i = 0; i < this->minForEstimate; i++)
    {
      norm += x[i] * x[i];
      parameters.push_back(x[i]);
    }
    norm = 1.0 / sqrt(norm);
    for (i = 0; i < this->minForEstimate; i++)
      parameters[i] *= norm;
  }
  // first point is arbitrarily chosen to be the
  //"point on plane"
  for (i = 0; i < Dimension; i++)
    parameters.push_back((*data[0])[i]);
}


template <unsigned int Dimension>
void
PlaneParametersEstimator<Dimension>::Estimate(std::vector<Point<double, Dimension>> & data,
                                              std::vector<double> &                   parameters)
{
  std::vector<Point<double, Dimension> *> usedData;
  int                                     dataSize = data.size();
  for (int i = 0; i < dataSize; i++)
    usedData.push_back(&(data[i]));
  Estimate(usedData, parameters);
}


/*
 * Estimate the plane parameters  [n_0,...,n_k,a_0,...,a_k].
 */
template <unsigned int Dimension>
void
PlaneParametersEstimator<Dimension>::LeastSquaresEstimate(std::vector<Point<double, Dimension> *> & data,
                                                          std::vector<double> &                     parameters)
{
  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;

  unsigned int       i, j, k, pointNum = data.size();
  vnl_matrix<double> meanMat(Dimension, Dimension), covariance(Dimension, Dimension, 0);
  vnl_vector<double> mean(Dimension, 0);

  // create covariance matrix
  double sqrtN = sqrt((double)pointNum);
  for (i = 0; i < pointNum; i++)
  {
    for (j = 0; j < Dimension; j++)
    {
      mean[j] += (*data[i])[j];
    }
  }

  mean /= sqrtN;
  for (i = 0; i < Dimension; i++)
  {
    for (j = i; j < Dimension; j++)
    {
      meanMat(i, j) = meanMat(j, i) = mean[i] * mean[j];
    }
  }

  // upper half
  for (i = 0; i < pointNum; i++)
  {
    for (j = 0; j < Dimension; j++)
    {
      for (k = j; k < Dimension; k++)
      {
        covariance(j, k) += (*data[i])[j] * (*data[i])[k];
      }
    }
  }

  // copy to lower half
  for (j = 0; j < Dimension; j++)
  {
    for (k = j + 1; k < Dimension; k++)
    {
      covariance(k, j) = covariance(j, k);
    }
  }

  // subtract mean matrix
  covariance -= meanMat;

  // compute eigen-vectors/values of covariance
  vnl_symmetric_eigensystem<double> eigenSystem(covariance);

  // the (hyper)plane normal is the eigen-vector corresponding to
  // the smallest eigen-value, I assume ||eigenSystem.V(i,0)|| = 1
  for (i = 0; i < Dimension; i++)
  {
    parameters.push_back(eigenSystem.V(i, 0));
  }

  for (i = 0; i < Dimension; i++)
  {
    parameters.push_back(mean[i] / sqrtN);
  }
}


template <unsigned int Dimension>
void
PlaneParametersEstimator<Dimension>::LeastSquaresEstimate(std::vector<Point<double, Dimension>> & data,
                                                          std::vector<double> &                   parameters)
{
  std::vector<Point<double, Dimension> *> usedData;
  int                                     dataSize = data.size();
  for (int i = 0; i < dataSize; i++)
    usedData.push_back(&(data[i]));
  LeastSquaresEstimate(usedData, parameters);
}


/*
 * Given the the plane parameters  [n_0,...,n_k,a_0,...,a_k] check if
 * dot([n_0,...,n_k], [data[0]-a_0,...,data[k]-a_k]) < delta
 */
template <unsigned int Dimension>
bool
PlaneParametersEstimator<Dimension>::Agree(std::vector<double> & parameters, Point<double, Dimension> & data)
{
  double signedDistance = 0;
  for (unsigned int i = 0; i < Dimension; i++)
    signedDistance += parameters[i] * (data[i] - parameters[Dimension + i]);
  return ((signedDistance * signedDistance) < this->deltaSquared);
}

} // end namespace itk

#endif
