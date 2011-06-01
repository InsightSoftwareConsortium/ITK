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
#ifndef __itkFEMLoadEdge_h
#define __itkFEMLoadEdge_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_matrix.h"

namespace itk {
namespace fem {

/**
 * \class LoadEdge
 * \brief A generic load that can be applied to an edge of the element.
 *
 * Can also be used to apply natural (Neumann) boundary condition on the
 * edge of the element. In this case m_Edge defines the edge or surfance
 * of the element on which the BC exists and matrix m_Force holds the actual
 * prescribed values of the BC.
 * \ingroup ITK-FEM
 */
class LoadEdge : public LoadElement
{
  FEM_CLASS(LoadEdge,LoadElement)
public:
  /**
   * Read a Load object from input stream.
   * We need arrays of elements and nodes to do that.
   */
  virtual void Read( std::istream& f, void* info );

  /**
   * Write a Load object to the output stream
   */
  virtual void Write( std::ostream& f ) const;

public:
  /**
   * Local number of the edge (face) of the element on which the load acts.
   * Check the corresponding element class for more info on edge numbering.
   */
  int m_Edge;

  /**
   * An edge force matrix. This matrix specifies nodal forces on all
   * nodes within the edge or face on which the load acts. Each nodal
   * force is decomposed into several components (check the documentation
   * inside the element class). In case of 2D elements this components
   * are normal (1st component) and tangential (2nd component) force
   * acting on the edge of the element. A positive normal load acts in
   * a direction INTO the element. A positive tangential load acts in
   * an ANTICLOCKWISE direction with respect to the loaded elemenet.
   * Each nodal force is stored in a column of the matrix. The number
   * of columns in the Force matrix must correspond to the number of
   * nodes that define the edge (face...). The force is interpolated
   * over the entire edge (face) by using the shape functions of the
   * element. Again check the documentation of the element class to which
   * the force is applied.
   */
  vnl_matrix<Float> m_Force;

};

FEM_CLASS_INIT(LoadEdge)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadEdge_h
