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

#include <iostream>
#include "itkVariableSizeMatrix.h"

int itkVariableSizeMatrixTest(int, char*[])
{
  typedef itk::VariableSizeMatrix<float>   FloatVariableSizeMatrixType;
  typedef itk::VariableSizeMatrix<double>  DoubleVariableSizeMatrixType;

  FloatVariableSizeMatrixType f( 3, 2 );
    f(0,0)=1.0; f(0,1) = 2.0;
    f(1,0)=3.0; f(1,1) = 4.0;
    f(2,0)=5.0; f(2,1) = 6.0;

  FloatVariableSizeMatrixType g( 3, 2 );
    g(0,0)=10.0; g(0,1) = 10.0;
    g(1,0)=100.0; g(1,1) = 100.0;
    g(2,0)=1000.0; g(2,1) = 1000.0;

  FloatVariableSizeMatrixType h;
    h = g + f;
    h *= 2.0;
    h /= 2.0;
    h += g;
    h -= g;
    h = g - h;
    h = -h;

  std::cout << "***** h" << std::endl << h << std::endl;  // should be (1,2)(3,4)(5,6)]
  std::cout << "***** h transpose" << std::endl << h.GetTranspose() << std::endl;
  std::cout << "***** h inverse" << std::endl << h.GetInverse() << std::endl;

  std::cout << "***** h" << std::endl << h << std::endl;

  FloatVariableSizeMatrixType hDouble = h * 2.0;
  std::cout << "***** h * 2" << std::endl << hDouble << std::endl;

  FloatVariableSizeMatrixType hHalf = h / 2.0;
  std::cout << "***** h / 2" << std::endl << hHalf << std::endl;

  if (hDouble == hHalf)
    {
    std::cout << "h*2 should not be equal to h/2" << std::endl;
    return EXIT_FAILURE;
    }

  h = g.GetVnlMatrix();
  std::cout << "***** g" << std::endl << g << std::endl;
  std::cout << "***** h = g.GetVnlMatrix()" << std::endl << h << std::endl;

  if (h != g)
    {
    std::cout << "after h = g.GetVnlMatrix(), h and g should be equal, buut they are not!" << std::endl;
    std::cout << "[FAILED]" << std::endl;

    return EXIT_FAILURE;

    }

  // Get each row separately
  for ( unsigned int i = 0; i < h.Rows(); i++)
    {
    float *row = h[i];
    std::cout << "h[" << i << "] = ";
    for (unsigned int j = 0; j < h.Cols(); j++)
      {
      std::cout << *(row + j) << ", ";
      }
    std::cout << std::endl;
    }

  // Get each row separately, const version
  for ( unsigned int i = 0; i < h.Rows(); i++)
    {
    const float *row = h[i];
    std::cout << "h[" << i << "] = ";
    for (unsigned int j = 0; j < h.Cols(); j++)
      {
      std::cout << *(row + j) << ", ";
      }
    std::cout << std::endl;
    }

  // Product by vnl_matrix
  vnl_matrix<float> dVnl(2,3,10.0);
  std::cout << "***** h" << std::endl << h << std::endl;
  std::cout << "***** dVnl" << std::endl << dVnl << std::endl;
  std::cout << "***** h * dVnl" << std::endl << h * dVnl << std::endl;
  h *= dVnl;
  std::cout << "***** h *= dVnl" << std::endl << h << std::endl;

  DoubleVariableSizeMatrixType d13(1,3);
    d13(0,0) = 2;
    d13(0,1) = 1;
    d13(0,2) = -2;

  // Verify the 4 conditions of the pseudoinverse
  std::cout << "***** d13" << std::endl << d13 << std::endl;
  std::cout << "***** d13 inverse (d31+)" << std::endl
            << d13.GetInverse() << std::endl;

  std::cout << "***** d13 * d13+" << std::endl
            << d13.GetVnlMatrix() * d13.GetInverse() << std::endl;

  std::cout << "***** d13+ * d13" << std::endl
            << d13.GetInverse() * d13.GetVnlMatrix() << std::endl;

  std::cout << "***** d13 * d13+ * d13 = d13" << std::endl
            << d13.GetVnlMatrix() * d13.GetInverse() * d13.GetVnlMatrix() << std::endl;
  std::cout << "***** d13+ * d13 * d13+ = d13+" << std::endl
            << d13.GetInverse() * d13.GetVnlMatrix() * d13.GetInverse() << std::endl;
  std::cout << "***** (d13 * d13+)T = d13 * d13+" << std::endl
            << (d13.GetVnlMatrix() * d13.GetInverse()).transpose() << std::endl;
  std::cout << "***** (d13+ * d13)T = d13+ * d13" << std::endl
            << (d13.GetInverse() * d13.GetVnlMatrix()).transpose() << std::endl;

  DoubleVariableSizeMatrixType dm(10,10);
    dm.Fill(10.0);
    dm.SetIdentity();

  std::cout << "***** dm(5,5)" << std::endl << dm(5,5) << std::endl;

  if (d13 == dm)
    {
    std::cout << "d13 should not be equal to dw" << std::endl;
    return EXIT_FAILURE;
    }
  itk::Array<float> array(3);
  array.Fill(10.0);

  FloatVariableSizeMatrixType fm(5,3);
  fm.Fill(10.0);

  std::cout << "***** array" << std::endl << array << std::endl;
  std::cout << "***** fm" << std::endl << fm << std::endl;
  std::cout << "***** fm * array" << std::endl << fm * array << std::endl;

  vnl_vector<float> vnlvector(3);
  vnlvector.fill(10.0);
  std::cout << "***** vnlvector" << std::endl << vnlvector << std::endl;
  std::cout << "***** fm" << std::endl << fm << std::endl;
  std::cout << "***** fm * vnlvector" << std::endl << fm * vnlvector << std::endl;


  DoubleVariableSizeMatrixType d53(5,3);
  d53.Fill(1);
  DoubleVariableSizeMatrixType d34(3,4);
  d34.Fill(2);
  itk::Array<double> darray(5);
  darray.Fill(10.0);

  std::cout << "***** d53" << std::endl << d53 << std::endl;
  std::cout << "***** d34" << std::endl << d34 << std::endl;
  std::cout << "***** d53 * d34" << std::endl << d53 * d34 << std::endl;

  DoubleVariableSizeMatrixType d55(5,5);
  d55.SetIdentity();
  d55 *= d55;

  // Exercise the exceptions
  try
    {
    std::cout << d34 * darray << std::endl;
    }
  catch (itk::ExceptionObject &e)
    {
    (&e)->Print(std::cout);
    }

  try
    {
    std::cout << d34 * d53 << std::endl;
    }
  catch (itk::ExceptionObject &e)
    {
    (&e)->Print(std::cout);
    }

  try
    {
    std::cout << d34 + d53 << std::endl;
    }
  catch (itk::ExceptionObject &e)
    {
    (&e)->Print(std::cout);
    }

  try
    {
    std::cout << d34 - d53 << std::endl;
    }
  catch (itk::ExceptionObject &e)
    {
    (&e)->Print(std::cout);
    }

  try
    {
    d34 += d53;
    }
  catch (itk::ExceptionObject &e)
    {
    (&e)->Print(std::cout);
    }

  try
    {
    d34 -= d53;
    }
  catch (itk::ExceptionObject &e)
    {
    (&e)->Print(std::cout);
    }

  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;

}
