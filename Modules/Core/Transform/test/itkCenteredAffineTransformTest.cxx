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

#include "itkCenteredAffineTransform.h"
#include "itkImage.h"

using MatrixType = itk::Matrix<double, 2, 2>;
using VectorType = itk::Vector<double, 2>;

namespace
{

void
PrintVector(const VectorType & v)
{
  for (unsigned int i = 0; i < VectorType::Dimension; ++i)
  {
    std::cout << v[i] << ", ";
  }
  std::cout << '\n';
}

} // namespace

int
itkCenteredAffineTransformTest(int, char *[])
{
  /* FIXME: This code exercises most of the methods but doesn't
     actually check that the results are correct. */

  /* Create a 2D identity transformation and show its parameters */
  using Affine2DType = itk::CenteredAffineTransform<double, 2>;
  auto id2 = Affine2DType::New();

  MatrixType matrix2 = id2->GetMatrix();
  VectorType vector2 = id2->GetOffset();
  std::cout << "Matrix from instantiating an identity transform:" << '\n' << matrix2;
  std::cout << "Vector from instantiating an identity transform:" << '\n';
  PrintVector(vector2);

  /* Create and show a simple 2D transform from given parameters */
  matrix2[0][0] = 1;
  matrix2[0][1] = 2;
  matrix2[1][0] = 3;
  matrix2[1][1] = 4;
  vector2[0] = 5;
  vector2[1] = 6;

  auto aff2 = Affine2DType::New();
  auto inverse2 = Affine2DType::New();
  aff2->SetMatrix(matrix2);
  aff2->SetOffset(vector2);
  for (unsigned int i = 0; i < 2; ++i)
  {
    for (unsigned int j = 0; j < 2; ++j)
    {
      matrix2[i][j] = 0.0;
    }
    vector2[i] = 0.0;
  }
  std::cout << "Instantiation of a given 2D transform:" << '\n';
  aff2->Print(std::cout);

  aff2->GetInverse(inverse2);
  std::cout << "Inverse matrix for the given transform:" << '\n' << inverse2->GetMatrix();

  /* Set parameters of a 2D transform */
  matrix2[0][0] = 6;
  matrix2[0][1] = 5;
  matrix2[1][0] = 4;
  matrix2[1][1] = 3;
  vector2[0] = 2;
  vector2[1] = 1;
  aff2->SetMatrix(matrix2);
  aff2->SetOffset(vector2);
  for (unsigned int i = 0; i < 2; ++i)
  {
    for (unsigned int j = 0; j < 2; ++j)
    {
      matrix2[i][j] = 0.0;
    }
    vector2[i] = 0.0;
  }
  matrix2 = aff2->GetMatrix();
  vector2 = aff2->GetOffset();
  std::cout << "Setting the matrix in an existing transform:" << '\n' << matrix2;
  std::cout << "Setting the offset in an existing  transform:" << '\n';
  PrintVector(vector2);

  /* Try composition of two transformations */
  aff2->Compose(aff2);
  std::cout << "Result of a composition:" << '\n';
  aff2->Print(std::cout);

  /* Compose with a translation */
  VectorType trans;
  trans[0] = 1;
  trans[1] = 2;
  aff2->Translate(trans);
  std::cout << "Result of a translation:" << '\n';
  aff2->Print(std::cout);

  /* Compose with an isotropic scaling */
  aff2->Scale(.3, true);
  std::cout << "Result of isotropic scaling:" << '\n';
  aff2->Print(std::cout);

  /* Compose with an anisotropic scaling */
  VectorType scale;
  scale[0] = .3;
  scale[1] = .2;
  aff2->Scale(scale);
  std::cout << "Result of anisotropic scaling:" << '\n';
  aff2->Print(std::cout);

  /* Compose with a general N-D rotation */
  aff2->Rotate(0, 1, 0.57, true);
  std::cout << "Result of general rotation:" << '\n';
  aff2->Print(std::cout);

  /* Compose with a 2-D rotation */
  aff2->Rotate(0, 1, -0.57, true);
  std::cout << "Result of 2-D rotation:" << '\n';
  aff2->Print(std::cout);

  /* Compose with a shear */
  aff2->Shear(1, 0, .2);
  std::cout << "Result of shear:" << '\n';
  aff2->Print(std::cout);

  /* Transform a point */
  itk::Point<double, 2> u2;
  u2[0] = 3;
  u2[1] = 5;
  itk::Point<double, 2> v2 = aff2->TransformPoint(u2);
  std::cout << "Transform a point:" << '\n' << v2[0] << " , " << v2[1] << '\n';

  // /* Back transform a point */
  // v2 = aff2->BackTransform(u2);
  // std::cout << "Back transform a point:" << '\n'
  // << v2[0] << " , " << v2[1] << '\n';

  /* Transform a vnl_vector */
  vnl_vector_fixed<double, 2> x2;
  x2[0] = 1;
  x2[1] = 2;
  vnl_vector_fixed<double, 2> y2 = aff2->TransformVector(x2);
  std::cout << "Transform a vnl_vector:" << '\n' << y2[0] << " , " << y2[1] << '\n';

  // /* Back transform a vector */
  // y2 = aff2->BackTransform(x2);
  // std::cout << "Back transform a vnl_vector:" << '\n'
  // << y2[0] << " , " << y2[1] << '\n';

  /* Transform a vector */
  itk::Vector<double, 2> u3;
  u3[0] = 3;
  u3[1] = 5;
  itk::Vector<double, 2> v3 = aff2->TransformVector(u3);
  std::cout << "Transform a vector:" << '\n' << v3[0] << " , " << v3[1] << '\n';

  // /* Back transform a vector */
  // v3 = aff2->BackTransform(u3);
  // std::cout << "Back transform a vector :" << '\n'
  // << v3[0] << " , " << v3[1] << '\n';

  /* Transform a Covariant vector */
  itk::Vector<double, 2> u4;
  u4[0] = 3;
  u4[1] = 5;
  itk::Vector<double, 2> v4 = aff2->TransformVector(u4);
  std::cout << "Transform a Covariant vector:" << '\n' << v4[0] << " , " << v4[1] << '\n';

  // /* Back transform a vector */
  // v4 = aff2->BackTransform(u4);
  // std::cout << "Back transform a vector :" << '\n'
  // << v4[0] << " , " << v4[1] << '\n';

  /* Create a 3D transform and rotate in 3D */
  using Affine3DType = itk::CenteredAffineTransform<double, 3>;
  auto aff3 = Affine3DType::New();
  auto axis = itk::MakeFilled<itk::Vector<double, 3>>(0.707);
  aff3->Rotate3D(axis, 1.0, true);
  std::cout << "Create and rotate a 3D transform:" << '\n';
  aff3->Print(std::cout);

  /* Generate inverse transform */
  auto inv3 = Affine3DType::New();
  if (!aff3->GetInverse(inv3))
  {
    std::cout << "Cannot create inverse transformation" << '\n';
  }
  std::cout << "Create an inverse transformation:" << '\n';
  inv3->Print(std::cout);

  const Affine3DType::Pointer inv4 = dynamic_cast<Affine3DType *>(aff3->GetInverseTransform().GetPointer());
  if (!inv4)
  {
    std::cout << "Cannot compute inverse transformation" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "Create an inverse transformation:" << '\n';
  inv4->Print(std::cout);

  /* Create an image for testing index<->physical transforms */
  std::cout << "Creating image for testing index<->physical transforms" << '\n';
  double                                      spacing[3] = { 1.0, 2.0, 3.0 };
  double                                      origin[3] = { 4.0, 5.0, 6.0 };
  const itk::Image<unsigned char, 3>::Pointer image = itk::Image<unsigned char, 3>::New();
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  /* Test output of ComputeJacobianWithRespectToParameters */
  auto                           jaff = Affine3DType::New();
  const Affine3DType::MatrixType jaffMatrix = jaff->GetMatrix();
  std::cout << "GetMatrix:" << '\n';
  std::cout << jaffMatrix << '\n';

  const Affine3DType::OffsetType jaffVector = jaff->GetOffset();
  std::cout << "GetOffset:" << '\n';
  std::cout << jaffVector << '\n';

  Affine3DType::InputPointType jpoint;
  jpoint[0] = 5.0;
  jpoint[1] = 10.0;
  jpoint[2] = 15.0;
  Affine3DType::JacobianType jaffJacobian;
  jaff->ComputeJacobianWithRespectToParameters(jpoint, jaffJacobian);

  std::cout << "ComputeJacobianWithRespectToParameters: " << '\n';
  std::cout << jaffJacobian << '\n';

  /* Get the parameters */
  Affine3DType::ParametersType parameters3D;
  parameters3D = aff3->GetParameters();

  std::cout << "Parameters 3D: " << parameters3D << '\n';

  /* Now set the parameters of another matrix */
  jaff->SetParameters(parameters3D);

  std::cout << "A transform after SetParameters:" << '\n';
  jaff->Print(std::cout);

  const int any = 0; // Any errors detected in testing?
  return any;
}
