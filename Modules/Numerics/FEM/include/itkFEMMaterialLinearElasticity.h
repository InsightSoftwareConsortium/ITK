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
#ifndef __itkFEMMaterialLinearElasticity_h
#define __itkFEMMaterialLinearElasticity_h

#include "itkFEMMaterialBase.h"

namespace itk {
namespace fem {

/**
 * \class MaterialLinearElasticity
 * \brief Linear elasticity material class
 *
 * This class includes material and other kind of properties required to
 * define material properties of finite elements applied to linear
 * elasticity problems in FEM toolkit.
 * \ingroup ITK-FEM
 */
class MaterialLinearElasticity : public Material {
FEM_CLASS(MaterialLinearElasticity,Material)
public:
  virtual void Read(std::istream& f, void* info);
  virtual void Write(std::ostream& f ) const;

  /**
   * Default constructor only initializes the members.
   */
  MaterialLinearElasticity();

  /* Data members of MaterialLinearElasticity class */

  /**
   * Young modulus
   */
  double E;

  /**
   * Cross section area of a line element
   */
  double A;  //

  /**
   * Moment of inertia
   */
  double I;

  /**
   * Poisson's ratio
   */
  double nu;

  /**
   * Thickness
   */
  double h;

  /*
   * ... we can add properties here as required without the influence on the already defined elements
   */

  /**
   * Density times Heat Capacity
   */
  double RhoC;

};

FEM_CLASS_INIT(MaterialLinearElasticity)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMMaterialLinearElasticity_h
