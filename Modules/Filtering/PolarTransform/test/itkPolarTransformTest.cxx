#if defined(_MSC_VER)
#  pragma warning(disable : 4786)
#endif

#include <iostream>

#include "itkPolarToCartesianTransform.h"
#include "itkCartesianToPolarTransform.h"
#include "vnl/vnl_math.h"

int
main(int, char *[])
{
  const unsigned int Dimension = 6;
  const double       epsilon = 1e-10;

  int i;

  /* Create 3D polar transforms and print them. */
  typedef itk::PolarToCartesianTransform<double, Dimension> P2CTransformType;
  typedef itk::CartesianToPolarTransform<double, Dimension> C2PTransformType;
  P2CTransformType::Pointer                                 p2c = P2CTransformType::New();
  C2PTransformType::Pointer                                 c2p = C2PTransformType::New();

  std::cout << "Polar to cartesian transform: " << p2c << std::endl;
  std::cout << "Cartesian to polar transform: " << c2p << std::endl;


  /* Create testing points in cartesian and polar space. */
  itk::Point<double, Dimension> c, p, tmp;
  c[0] = 1;
  c[1] = sqrt(3.0);
  p[0] = vnl_math::pi / 3.0;
  p[1] = 2;
  for (i = 2; i < Dimension; i++)
  {
    c[i] = 3;
    p[i] = c[i];
  }

  /* Transform point from polar to cartesian space. */
  tmp = p2c->TransformPoint(p);
  for (i = 0; i < Dimension; i++)
    if (abs(tmp[i] - c[i]) > epsilon)
    {
      std::cout << "Invalid polar to cartesian computed." << std::endl;
      return EXIT_FAILURE;
    }

  /* Transform point from cartesian to polar space. */
  tmp = c2p->TransformPoint(c);
  for (i = 0; i < Dimension; i++)
    if (abs(tmp[i] - p[i]) > epsilon)
    {
      std::cout << "Invalid cartesian to polar computed." << std::endl;
      return EXIT_FAILURE;
    }

  /* Transform point from polar to cartesian space and back. */
  tmp = c2p->TransformPoint(p2c->TransformPoint(p));
  for (i = 0; i < Dimension; i++)
    if (abs(tmp[i] - p[i]) > epsilon)
    {
      std::cout << "Invalid polar to cartesian and back computed." << std::endl;
      return EXIT_FAILURE;
    }

  /* Transform point from cartesian to polar space and back. */
  tmp = p2c->TransformPoint(c2p->TransformPoint(c));
  for (i = 0; i < Dimension; i++)
    if (abs(tmp[i] - c[i]) > epsilon)
    {
      std::cout << "Invalid cartesian to polar and back computed." << std::endl;
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
