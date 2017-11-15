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
#include "itkPolarToCartesianTransform.h"
#include "itkCartesianToPolarTransform.h"
#include "itkMath.h"

int
itkPolarTransformTest(int, char *[])
{
  const unsigned int Dimension = 4;
  const double       epsilon = 1e-10;

  /* Create 3D polar transforms and print them. */
  typedef itk::PolarToCartesianTransform<double, Dimension> P2CTransformType;
  typedef itk::CartesianToPolarTransform<double, Dimension> C2PTransformType;
  P2CTransformType::Pointer                                 p2c = P2CTransformType::New();
  C2PTransformType::Pointer                                 c2p = C2PTransformType::New();

  P2CTransformType::InputPointType center;
  center.Fill(0.0);
  center[0] = -1.0;

  p2c->SetCenter(center);
  c2p->SetCenter(center);

  std::cout << "Polar to cartesian transform: " << p2c << std::endl;
  std::cout << "Cartesian to polar transform: " << c2p << std::endl;


  /* Create testing points in cartesian and polar space. */
  itk::Point<double, Dimension> c, p, tmp;
  c[0] = 0.0;
  c[1] = std::sqrt(3.0);
  p[0] = itk::Math::pi / 3.0;
  p[1] = 2.0;
  for (unsigned int i = 2; i < Dimension; ++i)
  {
    c[i] = 3.0;
    p[i] = c[i];
  }

  /* Transform point from polar to cartesian space. */
  tmp = p2c->TransformPoint(p);
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (std::abs(tmp[i] - c[i]) > epsilon)
    {
      return EXIT_FAILURE;
    }
  }

  /* Transform point from cartesian to polar space. */
  tmp = c2p->TransformPoint(c);
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (std::abs(tmp[i] - p[i]) > epsilon)
    {
      std::cout << "Invalid cartesian to polar computed." << std::endl;
      return EXIT_FAILURE;
    }
  }

  /* Transform point from polar to cartesian space and back. */
  tmp = c2p->TransformPoint(p2c->TransformPoint(p));
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (std::abs(tmp[i] - p[i]) > epsilon)
    {
      std::cout << "Invalid polar to cartesian and back computed." << std::endl;
      return EXIT_FAILURE;
    }
  }

  /* Transform point from cartesian to polar space and back. */
  tmp = p2c->TransformPoint(c2p->TransformPoint(c));
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (std::abs(tmp[i] - c[i]) > epsilon)
    {
      std::cout << "Invalid cartesian to polar and back computed." << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
