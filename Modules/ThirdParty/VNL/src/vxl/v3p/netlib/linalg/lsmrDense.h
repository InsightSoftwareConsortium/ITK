/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef LSQR_lsmrDense_h
#define LSQR_lsmrDense_h

#include "lsmrBase.h"


/** \class lsmrDense
 *
 * Specific implementation of the solver for a type of dense Matrix.
 *
 */
class lsmrDense : public lsmrBase
{
public:

  lsmrDense();
  ~lsmrDense() override;

  /**
   * computes y = y + A*x without altering x,
   * where A is a matrix of dimensions A[m][n].
   * The size of the vector x is n.
   * The size of the vector y is m.
   */
  void Aprod1(unsigned int m, unsigned int n, const double * x, double * y ) const override;

  /**
   * computes x = x + A'*y without altering y,
   * where A is a matrix of dimensions A[m][n].
   * The size of the vector x is n.
   * The size of the vector y is m.
   */
  void Aprod2(unsigned int m, unsigned int n, double * x, const double * y ) const override;

  /** Set the matrix A of the equation to be solved A*x = b. */
  void SetMatrix( double ** A );

private:

  double ** A;
};

#endif
