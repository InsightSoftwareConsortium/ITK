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
#ifndef itkFEMUtility_h
#define itkFEMUtility_h

#include <string>
#include <iostream>
#include "metaObject.h"
#include "ITKFEMExport.h"

class MetaObject;

namespace itk
{
namespace fem
{
/**
 * \file itkFEMUtility.h
 * \brief Includes various helper classes and functions used
          throughout the FEM code.
 */

/**
 * \class GaussIntegrate
 * \brief Use the Gauss-Legendre formula to perform integration
 *
 * Numerical integration (Gauss-Legendre formula).
 * Integrates function f(x) from x=a to x=b in n points.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT GaussIntegrate
{
public:
  static const double zero;
  static const double one;
  static const double two;
  static const double z[110];
  static const double w[110];

  double Integrate(double ( *f )(double), double a, double b, int n = 3);

};

}
}  /* end namespace itk */

#endif /* #ifndef itkFEMUtility_h */
