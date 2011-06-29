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

#include "itkFEMLinearSystemWrapperVNL.h"
#include <iostream>

/* Testing for linear system wrappers */
int itkFEMLinearSystemWrapperVNLTest(int, char *[])
{

  /* loop vars for printing */
  unsigned int i;
  unsigned int j;

  /* declare wrapper */
  itk::fem::LinearSystemWrapperVNL it;

  /* system parameters */
  unsigned int N = 5;
  unsigned int nMatrices =  3;
  unsigned int nVectors =   2;
  unsigned int nSolutions = 2;

  /* Set up the system */
  it.SetSystemOrder(N);
  it.SetNumberOfMatrices(nMatrices);
  it.SetNumberOfVectors(nVectors);
  it.SetNumberOfSolutions(nSolutions);

  /* Set max non zeros in any matrix */
  /* it.SetMaximumNonZeroValuesInMatrix(12); */
  /* Initialize memory */
  for( i = 0; i < nMatrices; i++ )
    {
    it.InitializeMatrix(i);
    }
  for( i = 0; i < nVectors; i++ )
    {
    it.InitializeVector(i);
    }
  for( i = 0; i < nSolutions; i++ )
    {
    it.InitializeSolution(i);
    }

  /*     matrix 0
   * |11  0  0 14 15|
   * | 0 22  0  0  0|
   * | 0  0 33  0  0|
   * |14  0  0 44 45|
   * |15  0  0 45 55|
   */
  it.SetMatrixValue(0, 0, 11, 0); it.SetMatrixValue(0, 3, 14, 0); it.SetMatrixValue(0, 4, 15, 0);
  it.SetMatrixValue(1, 1, 22, 0);
  it.SetMatrixValue(2, 2, 33, 0);
  it.SetMatrixValue(3, 0, 14, 0); it.SetMatrixValue(3, 3, 44, 0); it.SetMatrixValue(3, 4, 45, 0);
  it.SetMatrixValue(4, 0, 15, 0); it.SetMatrixValue(4, 3, 45, 0); it.SetMatrixValue(4, 4, 55, 0);

  /* print matrix 0 */
  std::cout << "Matrix 0" << std::endl;
  for( i = 0; i < N; i++ )
    {
    for( j = 0; j < N; j++ )
      {
      std::cout << it.GetMatrixValue(i, j, 0) << " ";
      }
    std::cout << std::endl;
    }
  std::cout << std::endl;

  /*     matrix 1
   * |11  0  0 14 15|
   * | 0 22  0  0  0|
   * | 0  0 33  0  0|
   * |14  0  0 44 45|
   * |15  0  0 45 55|
   */
  it.SetMatrixValue(0, 0, 11, 1); it.SetMatrixValue(0, 3, 14, 1); it.SetMatrixValue(0, 4, 15, 1);
  it.SetMatrixValue(1, 1, 22, 1);
  it.SetMatrixValue(2, 2, 33, 1);
  it.SetMatrixValue(3, 0, 14, 1); it.SetMatrixValue(3, 3, 44, 1); it.SetMatrixValue(3, 4, 45, 1);
  it.SetMatrixValue(4, 0, 15, 1); it.SetMatrixValue(4, 3, 45, 1); it.SetMatrixValue(4, 4, 55, 1);

  /* print Matrix 1 */
  std::cout << "Matrix 1" << std::endl;
  for( i = 0; i < N; i++ )
    {
    for( j = 0; j < N; j++ )
      {
      std::cout << it.GetMatrixValue(i, j, 1) << " ";
      }
    std::cout << std::endl;
    }
  std::cout << std::endl;

  /* matrix 2 = matrix 0 * matrix 1 */
  it.MultiplyMatrixMatrix(2, 0, 1);

  /* print Matrix 2 */
  std::cout << "matrix 2 = matrix 0 and matrix 1" << std::endl;
  for( i = 0; i < N; i++ )
    {
    for( j = 0; j < N; j++ )
      {
      std::cout << it.GetMatrixValue(i, j, 2) << " ";
      }
    std::cout << std::endl;
    }
  std::cout << std::endl;

  /* Vector 0 = [1 2 3 4 5] */
  it.SetVectorValue(0, 1, 0);
  it.SetVectorValue(1, 2, 0);
  it.SetVectorValue(2, 3, 0);
  it.SetVectorValue(3, 4, 0);
  it.SetVectorValue(4, 5, 0);

  /* print Vector 0 */
  std::cout << "Vector 0" << std::endl;
  for( i = 0; i < N; i++ )
    {
    std::cout << it.GetVectorValue(i, 0) << " ";
    }
  std::cout << std::endl << std::endl;

  /* vector 1 = matrix 0 * vector 0 */
  std::cout << "Vector 1 =  Matrix 0 * Vector 0" << std::endl;
  it.MultiplyMatrixVector(1, 0, 0);
  for( i = 0; i < N; i++ )
    {
    std::cout << it.GetVectorValue(i, 1) << " ";
    }
  std::cout << std::endl << std::endl;

  /* swap vectors */
  std::cout << "swap Vector 0 and Vector 1" << std::endl;
  std::cout << "Vector 0" << std::endl;
  it.SwapVectors(0, 1);
  for( i = 0; i < N; i++ )
    {
    std::cout << it.GetVectorValue(i, 0) << " ";
    }
  std::cout << std::endl << "Vector 1" << std::endl;
  for( i = 0; i < 5; i++ )
    {
    std::cout << it.GetVectorValue(i, 1) << " ";
    }
  std::cout << std::endl << std::endl;

  /* swap matrices */
  std::cout << "swap Matrix 0 and Matrix 2" << std::endl;
  it.SwapMatrices(0, 2);
  std::cout << "Matrix 0" << std::endl;
  for( i = 0; i < N; i++ )
    {
    for( j = 0; j < N; j++ )
      {
      std::cout << it.GetMatrixValue(i, j, 0) << " ";
      }
    std::cout << std::endl;
    }
  std::cout << std::endl << "Matrix 2" << std::endl;
  for( i = 0; i < N; i++ )
    {
    for( j = 0; j < N; j++ )
      {
      std::cout << it.GetMatrixValue(i, j, 2) << " ";
      }
    std::cout << std::endl;
    }
  std::cout << std::endl;

  /* solve system */
  std::cout << "Solve for x in: Matrix 0 * x = Vector 0" << std::endl;
  it.Solve();
  std::cout << "Solution 0" << std::endl;
  for( i = 0; i < N; i++ )
    {
    std::cout << it.GetSolutionValue(i, 0) << " ";
    }
  std::cout << std::endl << std::endl;

  /* set solution */
  std::cout << "Solution 1" << std::endl;
  it.SetSolutionValue(0, 1, 1);
  it.SetSolutionValue(1, 2, 1);
  it.SetSolutionValue(2, 3, 1);
  it.SetSolutionValue(3, 4, 1);
  it.SetSolutionValue(4, 5, 1);
  for( i = 0; i < 5; i++ )
    {
    std::cout << it.GetSolutionValue(i, 1) << " ";
    }
  std::cout << std::endl << std::endl;

  /* swap solutions */
  std::cout << "swap Solution 0 and Solution 1" << std::endl;
  it.SwapSolutions(0, 1);
  std::cout << "Solution 0" << std::endl;
  for( i = 0; i < N; i++ )
    {
    std::cout << it.GetSolutionValue(i, 0) << " ";
    }
  std::cout << std::endl << "Solution 1" << std::endl;
  for( i = 0; i < N; i++ )
    {
    std::cout << it.GetSolutionValue(i, 1) << " ";
    }
  std::cout << std::endl << std::endl;

  /* copy solution to vector */
  std::cout << "copy Solution 1 to Vector 0" << std::endl;
  it.CopySolution2Vector(1, 0);
  std::cout << "Vector 0" << std::endl;
  for( i = 0; i < N; i++ )
    {
    std::cout << it.GetVectorValue(i, 0) << " ";
    }
  std::cout << std::endl << std::endl;

  /* scale matrix */
  std::cout << "scale Matrix 2 by 2.0" << std::endl;
  it.ScaleMatrix(2.0, 2);
  std::cout << "Matrix 2" << std::endl;
  for( i = 0; i < N; i++ )
    {
    for( j = 0; j < N; j++ )
      {
      std::cout << it.GetMatrixValue(i, j, 2) << " ";
      }
    std::cout << std::endl;
    }
  std::cout << std::endl;

  /* scale vector */
  std::cout << "scale Vector 0 by 3.0" << std::endl;
  it.ScaleVector(3.0, 0);
  std::cout << "Vector 0" << std::endl;
  for( i = 0; i < 5; i++ )
    {
    std::cout << it.GetVectorValue(i, 0) << " ";
    }
  std::cout << std::endl << std::endl;

  /* destroy matrix,vector,solution */
  it.DestroyMatrix(0);
  it.DestroyVector(1);
  it.DestroySolution(0);

  return EXIT_SUCCESS;
}
