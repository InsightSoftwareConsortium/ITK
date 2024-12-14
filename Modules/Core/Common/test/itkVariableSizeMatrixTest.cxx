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

#include <iostream>
#include "itkVariableSizeMatrix.h"

int
itkVariableSizeMatrixTest(int, char *[])
{
  using FloatVariableSizeMatrixType = itk::VariableSizeMatrix<float>;
  using DoubleVariableSizeMatrixType = itk::VariableSizeMatrix<double>;

  FloatVariableSizeMatrixType f(3, 2);
  f(0, 0) = 1.0;
  f(0, 1) = 2.0;
  f(1, 0) = 3.0;
  f(1, 1) = 4.0;
  f(2, 0) = 5.0;
  f(2, 1) = 6.0;

  FloatVariableSizeMatrixType g(3, 2);
  g(0, 0) = 10.0;
  g(0, 1) = 10.0;
  g(1, 0) = 100.0;
  g(1, 1) = 100.0;
  g(2, 0) = 1000.0;
  g(2, 1) = 1000.0;

  FloatVariableSizeMatrixType h;
  h = g + f;
  h *= 2.0;
  h /= 2.0;
  h += g;
  h -= g;
  h = g - h;
  h = -h;

  std::cout << "***** h" << '\n' << h << '\n'; // should be (1,2)(3,4)(5,6)]
  std::cout << "***** h transpose" << '\n' << h.GetTranspose() << '\n';
  std::cout << "***** h inverse" << '\n' << h.GetInverse() << '\n';

  std::cout << "***** h" << '\n' << h << '\n';

  const FloatVariableSizeMatrixType hDouble = h * 2.0;
  std::cout << "***** h * 2" << '\n' << hDouble << '\n';

  const FloatVariableSizeMatrixType hHalf = h / 2.0;
  std::cout << "***** h / 2" << '\n' << hHalf << '\n';

  if (hDouble == hHalf)
  {
    std::cout << "h*2 should not be equal to h/2" << '\n';
    return EXIT_FAILURE;
  }

  h = g.GetVnlMatrix();
  std::cout << "***** g" << '\n' << g << '\n';
  std::cout << "***** h = g.GetVnlMatrix()" << '\n' << h << '\n';

  if (h != g)
  {
    std::cout << "after h = g.GetVnlMatrix(), h and g should be equal, buut they are not!" << '\n';
    std::cout << "[FAILED]" << '\n';

    return EXIT_FAILURE;
  }

  // Get each row separately
  for (unsigned int i = 0; i < h.Rows(); ++i)
  {
    float * row = h[i];
    std::cout << "h[" << i << "] = ";
    for (unsigned int j = 0; j < h.Cols(); ++j)
    {
      std::cout << *(row + j) << ", ";
    }
    std::cout << '\n';
  }

  // Get each row separately, const version
  for (unsigned int i = 0; i < h.Rows(); ++i)
  {
    const float * row = h[i];
    std::cout << "h[" << i << "] = ";
    for (unsigned int j = 0; j < h.Cols(); ++j)
    {
      std::cout << *(row + j) << ", ";
    }
    std::cout << '\n';
  }

  // Product by vnl_matrix
  const vnl_matrix<float> dVnl(2, 3, 10.0);
  std::cout << "***** h" << '\n' << h << '\n';
  std::cout << "***** dVnl" << '\n' << dVnl << '\n';
  std::cout << "***** h * dVnl" << '\n' << h * dVnl << '\n';
  h *= dVnl;
  std::cout << "***** h *= dVnl" << '\n' << h << '\n';

  DoubleVariableSizeMatrixType d13(1, 3);
  d13(0, 0) = 2;
  d13(0, 1) = 1;
  d13(0, 2) = -2;

  // Verify the 4 conditions of the pseudoinverse
  std::cout << "***** d13" << '\n' << d13 << '\n';
  std::cout << "***** d13 inverse (d31+)" << '\n' << d13.GetInverse() << '\n';

  std::cout << "***** d13 * d13+" << '\n' << d13.GetVnlMatrix() * d13.GetInverse() << '\n';

  std::cout << "***** d13+ * d13" << '\n' << d13.GetInverse() * d13.GetVnlMatrix() << '\n';

  std::cout << "***** d13 * d13+ * d13 = d13" << '\n'
            << d13.GetVnlMatrix() * d13.GetInverse() * d13.GetVnlMatrix() << '\n';
  std::cout << "***** d13+ * d13 * d13+ = d13+" << '\n'
            << d13.GetInverse() * d13.GetVnlMatrix() * d13.GetInverse() << '\n';
  std::cout << "***** (d13 * d13+)T = d13 * d13+" << '\n'
            << (d13.GetVnlMatrix() * d13.GetInverse()).transpose() << '\n';
  std::cout << "***** (d13+ * d13)T = d13+ * d13" << '\n'
            << (d13.GetInverse() * d13.GetVnlMatrix()).transpose() << '\n';

  DoubleVariableSizeMatrixType dm(10, 10);
  dm.Fill(10.0);
  dm.SetIdentity();

  std::cout << "***** dm(5,5)" << '\n' << dm(5, 5) << '\n';

  if (d13 == dm)
  {
    std::cout << "d13 should not be equal to dw" << '\n';
    return EXIT_FAILURE;
  }
  itk::Array<float> array(3);
  array.Fill(10.0);

  FloatVariableSizeMatrixType fm(5, 3);
  fm.Fill(10.0);

  std::cout << "***** array" << '\n' << array << '\n';
  std::cout << "***** fm" << '\n' << fm << '\n';
  std::cout << "***** fm * array" << '\n' << fm * array << '\n';

  vnl_vector<float> vnlvector(3);
  vnlvector.fill(10.0);
  std::cout << "***** vnlvector" << '\n' << vnlvector << '\n';
  std::cout << "***** fm" << '\n' << fm << '\n';
  std::cout << "***** fm * vnlvector" << '\n' << fm * vnlvector << '\n';


  DoubleVariableSizeMatrixType d53(5, 3);
  d53.Fill(1);
  DoubleVariableSizeMatrixType d34(3, 4);
  d34.Fill(2);
  itk::Array<double> darray(5);
  darray.Fill(10.0);

  std::cout << "***** d53" << '\n' << d53 << '\n';
  std::cout << "***** d34" << '\n' << d34 << '\n';
  std::cout << "***** d53 * d34" << '\n' << d53 * d34 << '\n';

  DoubleVariableSizeMatrixType d55(5, 5);
  d55.SetIdentity();
  d55 *= d55;

  // Exercise the exceptions
  try
  {
    std::cout << d34 * darray << '\n';
  }
  catch (const itk::ExceptionObject & e)
  {
    e.Print(std::cout);
  }

  try
  {
    std::cout << d34 * d53 << '\n';
  }
  catch (const itk::ExceptionObject & e)
  {
    e.Print(std::cout);
  }

  try
  {
    std::cout << d34 + d53 << '\n';
  }
  catch (const itk::ExceptionObject & e)
  {
    e.Print(std::cout);
  }

  try
  {
    std::cout << d34 - d53 << '\n';
  }
  catch (const itk::ExceptionObject & e)
  {
    e.Print(std::cout);
  }

  try
  {
    d34 += d53;
  }
  catch (const itk::ExceptionObject & e)
  {
    e.Print(std::cout);
  }

  try
  {
    d34 -= d53;
  }
  catch (const itk::ExceptionObject & e)
  {
    e.Print(std::cout);
  }

  std::cout << "[PASSED]" << '\n';

  return EXIT_SUCCESS;
}
