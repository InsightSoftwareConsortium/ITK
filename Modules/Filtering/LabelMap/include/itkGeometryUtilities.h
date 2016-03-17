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
#ifndef itkGeometryUtilities_h
#define itkGeometryUtilities_h

#include "itkObject.h"
#include "ITKLabelMapExport.h"

namespace itk
{

/** \class GeometryUtilities
 * \brief Groups some utility functions related to geometry
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ITKLabelMap
 */
class ITKLabelMap_EXPORT GeometryUtilities
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
