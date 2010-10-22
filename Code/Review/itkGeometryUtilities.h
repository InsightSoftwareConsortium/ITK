/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeBasedKmeansEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
