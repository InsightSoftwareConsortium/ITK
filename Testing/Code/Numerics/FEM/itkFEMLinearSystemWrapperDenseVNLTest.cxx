/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLinearSystemWrapperDenseVNLTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif


#include "itkFEMLinearSystemWrapperDenseVNL.h"
#include <iostream>

using namespace itk::fem;
using namespace std;

/* Testing for linear system wrappers */
int itkFEMLinearSystemWrapperDenseVNLTest( int, char ** ) 
{

  /* loop vars for printing */
  unsigned int i;
  unsigned int j;


  /* declare wrapper */
  LinearSystemWrapperDenseVNL it;


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
  for (i=0; i<nMatrices; i++) 
  {
    it.InitializeMatrix(i);
  }
  for (i=0; i<nVectors; i++)
  {
    it.InitializeVector(i);
  }
  for (i=0; i<nSolutions; i++)
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
  it.SetMatrixValue(0,0,11,0); it.SetMatrixValue(0,3,14,0); it.SetMatrixValue(0,4,15,0);
  it.SetMatrixValue(1,1,22,0); 
  it.SetMatrixValue(2,2,33,0); 
  it.SetMatrixValue(3,0,14,0); it.SetMatrixValue(3,3,44,0); it.SetMatrixValue(3,4,45,0);
  it.SetMatrixValue(4,0,15,0); it.SetMatrixValue(4,3,45,0); it.SetMatrixValue(4,4,55,0);


  /* print matrix 0 */
  cout << "Matrix 0" << endl;
  for(i=0; i<N; i++) 
  {
    for (j=0; j<N; j++) 
    {
      cout << it.GetMatrixValue(i,j,0) << " ";
    }
    cout << endl;
  }
  cout << endl;


  /*     matrix 1
   * |11  0  0 14 15|
   * | 0 22  0  0  0|
   * | 0  0 33  0  0|
   * |14  0  0 44 45|
   * |15  0  0 45 55|
   */
  it.SetMatrixValue(0,0,11,1); it.SetMatrixValue(0,3,14,1); it.SetMatrixValue(0,4,15,1);
  it.SetMatrixValue(1,1,22,1); 
  it.SetMatrixValue(2,2,33,1); 
  it.SetMatrixValue(3,0,14,1); it.SetMatrixValue(3,3,44,1); it.SetMatrixValue(3,4,45,1);
  it.SetMatrixValue(4,0,15,1); it.SetMatrixValue(4,3,45,1); it.SetMatrixValue(4,4,55,1);


  /* print Matrix 1 */
  cout << "Matrix 1" << endl;
  for(i=0; i<N; i++) 
  {
    for (j=0; j<N; j++) 
    {
      cout << it.GetMatrixValue(i,j,1) << " ";
    }
    cout << std::endl;
  }
  cout << endl;


  /* matrix 2 = matrix 0 * matrix 1 */
  it.MultiplyMatrixMatrix(2,0,1);


  /* print Matrix 2 */
  cout << "matrix 2 = matrix 0 and matrix 1" << endl;
  for(i=0; i<N; i++) {
    for (j=0; j<N; j++) {
      cout << it.GetMatrixValue(i,j,2) << " ";
    }
    cout << endl;
  }
  cout << endl;    


  /* Vector 0 = [1 2 3 4 5] */
  it.SetVectorValue(0,1,0);
  it.SetVectorValue(1,2,0);
  it.SetVectorValue(2,3,0);
  it.SetVectorValue(3,4,0);
  it.SetVectorValue(4,5,0);


  /* print Vector 0 */
  cout << "Vector 0" << endl;
  for (i=0; i<N; i++) 
  {
    cout << it.GetVectorValue(i,0) << " ";
  }
  cout << endl << endl;


  /* vector 1 = matrix 0 * vector 0 */
  cout << "Vector 1 =  Matrix 0 * Vector 0" << endl;
  it.MultiplyMatrixVector(1, 0, 0);
  for (i=0; i<N; i++) 
  {
    cout << it.GetVectorValue(i,1) << " ";
  }
  cout << endl << endl;


  /* swap vectors */
  cout << "swap Vector 0 and Vector 1" << endl;
  cout << "Vector 0" << endl;
  it.SwapVectors(0,1);
  for (i=0; i<N; i++) 
  {
    cout << it.GetVectorValue(i,0) << " ";
  }
  cout << endl << "Vector 1" << endl;
  for (i=0; i<5; i++) 
  {
    cout << it.GetVectorValue(i,1) << " ";
  }
  cout << endl << endl;


  /* swap matrices */
  cout << "swap Matrix 0 and Matrix 2" << endl;
  it.SwapMatrices(0,2);
  cout << "Matrix 0" << endl;
  for(i=0; i<N; i++) 
  {
    for (j=0; j<N; j++) 
    {
      cout << it.GetMatrixValue(i,j,0) << " ";
    }
    cout << endl;
  }
  cout << endl << "Matrix 2" << endl;
  for(i=0; i<N; i++) 
  {
    for (j=0; j<N; j++) 
    {
      cout << it.GetMatrixValue(i,j,2) << " ";
    }
    cout << endl;
  }
  cout << endl;


  /* solve system */
  cout << "Solve for x in: Matrix 0 * x = Vector 0" << endl;
  it.Solve();
  cout << "Solution 0" << endl;
  for (i=0; i<N; i++) 
  {
    cout << it.GetSolutionValue(i,0) << " ";
  }
  cout << endl << endl;
  

  /* set solution */
  cout << "Solution 1" << endl;
  it.SetSolutionValue(0,1,1);
  it.SetSolutionValue(1,2,1);
  it.SetSolutionValue(2,3,1);
  it.SetSolutionValue(3,4,1);
  it.SetSolutionValue(4,5,1);
  for (i=0; i<5; i++) 
  {
    cout << it.GetSolutionValue(i,1) << " ";
  }
  cout << endl << endl;


  /* swap solutions */
  cout << "swap Solution 0 and Solution 1" << endl;
  it.SwapSolutions(0,1);
  cout << "Solution 0" << endl;
  for (i=0; i<N; i++) 
  {
    cout << it.GetSolutionValue(i,0) << " ";
  }
  cout << endl << "Solution 1" << endl;
  for (i=0; i<N; i++) 
  {
    cout << it.GetSolutionValue(i,1) << " ";
  }
  cout << endl << endl;


  /* copy solution to vector */
  cout << "copy Solution 1 to Vector 0" << endl;
  it.CopySolution2Vector(1,0);
  cout << "Vector 0" << endl;
  for (i=0; i<N; i++) 
  {
    cout << it.GetVectorValue(i,0) << " ";
  }
  cout << endl << endl;


  /* scale matrix */
  cout << "scale Matrix 2 by 2.0" << endl;
  it.ScaleMatrix(2.0, 2);
  cout << "Matrix 2" << endl;
  for(i=0; i<N; i++) 
  {
    for (j=0; j<N; j++) 
    {
      cout << it.GetMatrixValue(i,j,2) << " ";
    }
    cout << endl;
  }
  cout << endl;


  /* scale vector */
  cout << "scale Vector 0 by 3.0" << endl;
  it.ScaleVector(3.0, 0);
  cout << "Vector 0" << endl;
  for (i=0; i<5; i++) 
  {
    cout << it.GetVectorValue(i,0) << " ";
  }
  cout << endl << endl;


  /* destroy matrix,vector,solution */
  it.DestroyMatrix(0);
  it.DestroyVector(1);
  it.DestroySolution(0);


  return EXIT_SUCCESS;
 
}


