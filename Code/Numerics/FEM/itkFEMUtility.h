/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMUtility.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFEMUtility_h
#define __itkFEMUtility_h

#include <string>
#include <istream>

namespace itk {
namespace fem {




/**
 * \file itkFEMUtility.h
 * \brief Includes various helper classes and functions used 
          througout the FEM code.
 */




/**
 * Function that skips all the whitespace and comments in an input stream.
 */
void SkipWhiteSpace(std::istream& f);

/**
 * Const string of all whitespace characters. This string is used by
 * #SkipWhiteSpace function.
 */
static const std::string whitespaces=" \t\n\r";

/**
 * \class GaussIntegrate
 * \brief Use the Gauss-Legendre formula to perform integration
 *
 * Numerical integration (Gauss-Legendre formula).
 * Integrates function f(x) from x=a to x=b in n points.
 */
class GaussIntegrate {
public:
  static const double zero;
  static const double one;
  static const double two;
  static const double z[110];
  static const double w[110];
  double Integrate(double (*f)(double), double a, double b, int n=3);
};




}} /* end namespace itk */

#endif /* #ifndef __itkFEMUtility_h */
