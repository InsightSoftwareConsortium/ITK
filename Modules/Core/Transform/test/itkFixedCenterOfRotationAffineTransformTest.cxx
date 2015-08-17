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

#include "itkFixedCenterOfRotationAffineTransform.h"


int itkFixedCenterOfRotationAffineTransformTest(int, char *[])
{
  typedef itk::FixedCenterOfRotationAffineTransform<double,2>
                                            FCoRAffine2DType;
  typedef itk::AffineTransform<double,2>    FAffine2DType;
  FCoRAffine2DType::MatrixType              matrix2;
  FAffine2DType::Pointer                    inverse2;
  FCoRAffine2DType::InputVectorType         vector2;
  FCoRAffine2DType::InputPointType          point2;

  FCoRAffine2DType::Pointer id2 = FCoRAffine2DType::New();
  matrix2 = id2->GetMatrixComponent();
  vector2 = id2->GetOffsetComponent();
  point2 = id2->GetCenterOfRotationComponent();

  std::cout << "Instantiation of an identity Transform: ";

  bool fail = false;
  for(unsigned int i=0;i<2;i++)
    {
    for(unsigned int j=0;j<2;j++)
      {
      if( (i!=j) && (matrix2.GetVnlMatrix().get(i,j) != 0.0))
        {
        fail = true;
        }
      else if((i==j) && (matrix2.GetVnlMatrix().get(i,j) != 1.0))
        {
        fail = true;
        }
      }
    if((vector2[i] != 0.0) || (point2[i] != 0.0))
      {
      fail = true;
      }
    }

  if(fail)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  /* Create and show a simple 2D transform from given parameters */
  matrix2[0][0] = 1.0;
  matrix2[0][1] = 2.0;
  matrix2[1][0] = 3.0;
  matrix2[1][1] = 4.0;
  vector2[0] = 5.0;
  vector2[1] = 6.0;
  point2[0] = 1.0;
  point2[1] = 1.0;

  FCoRAffine2DType::Pointer aff2 = FCoRAffine2DType::New();
  aff2->SetCenterOfRotationComponent( point2 );
  aff2->SetMatrixComponent( matrix2 );
  aff2->SetOffsetComponent( vector2 );

  std::cout << "Instantiation of a given 2D transform: ";

  matrix2 = aff2->GetMatrixComponent();
  vector2 = aff2->GetOffsetComponent();
  point2 = aff2->GetCenterOfRotationComponent();

  if(
    matrix2[0][0] != 1.0 ||
    matrix2[0][1] != 2.0 ||
    matrix2[1][0] != 3.0 ||
    matrix2[1][1] != 4.0 ||
    vector2[0] != 5.0 ||
    vector2[1] != 6.0 ||
    point2[0] != 1.0 ||
    point2[1] != 1.0
    )
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  /** Test set matrix after setting components */
  double scale[2];
  scale[0]=2.0;
  scale[1]=4.0;

  aff2->SetScaleComponent(scale);
  aff2->SetMatrix(matrix2);

  matrix2 = aff2->GetMatrixComponent();
  vector2 = aff2->GetOffsetComponent();
  const double* resultingScale = aff2->GetScaleComponent();

  std::cout << "Modify the affine matrix: ";

  if(
    matrix2[0][0] != 1.0 ||
    matrix2[0][1] != 2.0 ||
    matrix2[1][0] != 3.0 ||
    matrix2[1][1] != 4.0 ||
    vector2[0] != 5.0 ||
    vector2[1] != 6.0 ||
    point2[0] != 1.0 ||
    point2[1] != 1.0 ||
    resultingScale[0] !=2.0 ||
    resultingScale[1] !=4.0
    )
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  /** Try scaling */
  std::cout << "Testing scaling: ";
  aff2->SetIdentity();
  aff2->SetScaleComponent(scale);

  matrix2 = aff2->GetMatrix();

  if(
      itk::Math::NotAlmostEquals( matrix2[0][0], 2.0 ) ||
      itk::Math::NotAlmostEquals( matrix2[0][1], 0.0 ) ||
      itk::Math::NotAlmostEquals( matrix2[1][0], 0.0 ) ||
      itk::Math::NotAlmostEquals( matrix2[1][1], 4.0 )
    )
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  /** Test the parameters */
  std::cout << "Setting/Getting parameters: ";

  FCoRAffine2DType::ParametersType parameters(6);
  parameters.Fill(0);

  point2[0] = 1;
  point2[1] = 2;

  aff2->SetCenterOfRotationComponent(point2);

  // Set the identity matrix
  parameters[0]=1.0;
  parameters[1]=2.0;
  parameters[2]=3.0;
  parameters[3]=4.0;

  // Set the offset
  parameters[4]=3.0;
  parameters[5]=4.0;

  aff2->SetParameters(parameters);
  FCoRAffine2DType::ParametersType parameters2;
  parameters2 = aff2->GetParameters();

  if(
      parameters2[0] != 1.0 ||
      parameters2[1] != 2.0 ||
      parameters2[2] != 3.0 ||
      parameters2[3] != 4.0 ||
      parameters2[4] != 3.0 ||
      parameters2[5] != 4.0
    )
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  /** Testing point transformation */
  std::cout << "Transforming Point: ";

  FCoRAffine2DType::InputPointType point;
  point[0] = 1;
  point[1] = 2;

  FCoRAffine2DType::InputPointType transformedPoint = aff2->TransformPoint(point);


  FCoRAffine2DType::InputPointType expectedPoint;
  FCoRAffine2DType::MatrixType matrix;
  matrix[0][0] = 1;
  matrix[0][1] = 2;
  matrix[1][0] = 3;
  matrix[1][1] = 4;
  FCoRAffine2DType::OffsetType offset;
  offset[0] = 3;
  offset[1] = 4;
  FCoRAffine2DType::InputVectorType v = matrix*(point-point2);

  for(unsigned int i=0;i<2;i++)
    {
    expectedPoint[i] = v[i]+point2[i]+offset[i];
    }

  if(transformedPoint != expectedPoint)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS]" << std::endl;
    }

  std::cout << "Done!" << std::endl;

  return EXIT_SUCCESS;
}
