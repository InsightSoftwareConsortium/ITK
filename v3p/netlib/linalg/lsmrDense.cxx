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
#include "lsmrDense.h"

lsmrDense::lsmrDense()
{
  this->A = 0;
}


lsmrDense::~lsmrDense()
{
}


void
lsmrDense::SetMatrix( double ** inputA )
{
  this->A = inputA;
}


/**
 * computes y = y + A*x without altering x.
 */
void lsmrDense::
Aprod1(unsigned int m, unsigned int n, const double * x, double * y ) const
{
  for ( unsigned int row = 0; row < m; row++ )
    {
    const double * rowA = this->A[row];
    double sum = 0.0;

    for ( unsigned int col = 0; col < n; col++ )
      {
      sum += rowA[col] * x[col];
      }

    y[row] +=  sum;
    }
}


/**
 * computes x = x + A'*y without altering y.
 */
void lsmrDense::
Aprod2(unsigned int m, unsigned int n, double * x, const double * y ) const
{
  for ( unsigned int col = 0; col < n; col++ )
    {
    double sum = 0.0;

    for ( unsigned int row = 0; row < m; row++ )
      {
      sum += this->A[row][col] * y[row];
      }

    x[col] +=  sum;
    }
}
