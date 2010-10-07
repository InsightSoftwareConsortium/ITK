#ifndef __itkGeometryUtilities_h
#define __itkGeometryUtilities_h

#include "itkObject.h"

namespace itk
{

/** \class GeometryUtilities
 * \brief Groups some utility functions related to geometry
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa
 */
class GeometryUtilities
{
public:

  static long Factorial(const long n);

  static long DoubleFactorial(const long n);

  static double GammaN2p1(const long n);

  static double HyperSphereVolume(const int dim, const double radius);

  static double HyperSpherePerimeter(const int dim, const double radius);

  static double HyperSphereRadiusFromVolume(const int dim, const double volume);

};

}
#endif
