/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMUtility.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkFEMUtility_h
#define __itkFEMUtility_h

#include "itkFEMNodeBase.h"
#include <string>
#include <iostream>

namespace itk {
namespace fem {




/**
 * Various classes, functions and other utilities required by FEM toolkit
 */

/**
 * function that skips all the whitespace and comments in an input stream
 */
void SkipWhiteSpace(std::istream& f);

/**
 * const string of all whitespace characters
 */
static const std::string whitespaces=" \t\n\r";

/**
 * Finds the node that holds a specific DOF. Sets the pnode to point to the
 * found node object and n to DOF number within that Node. pnode must be a
 * reference to a pointer to a Node object, and n must be a reference to an
 * int that will hold the number of DOF within a node.
 */
void FindNode(  Node::ArrayType::ConstPointer nodes, const Node::Displacement *dof, 
        Node::ConstPointer &pnode, int& n);

/**
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
