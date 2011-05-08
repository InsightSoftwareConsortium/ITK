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
#ifndef __itkFEMGenerateMesh_h
#define __itkFEMGenerateMesh_h

#include "vnl/vnl_vector.h"
#include "itkFEMSolver.h"

namespace itk {
namespace fem {

/**
 * \brief Use this function to generate 2D meshes in Solver.
 * Generate a rectangular mesh of quadrilateral elements.
 *
 * This function uses the generic quadrilateral elements
 * to build meshes that can be used with specific elements for solving
 * membrane or linear elasticity problems.
 *
 * See other functions if you need to constuct the mesh from other types
 * of elements.
 *
 * \note All elements will be created by copying the existing element which
 *       is passed to the function. Only number and node pointers will
 *       be changed in copied element. Make sure that this element has material
 *       class and any other properties defined before generating a mesh.
 *
 * \sa Generate3DRectilinearMesh
 */
void Generate2DRectilinearMesh(itk::fem::Element::ConstPointer e0, Solver& S, vnl_vector<double>& orig, vnl_vector<double>& size, vnl_vector<double>& Nel);


/**
 * \brief Use this function to generate 3D meshes in Solver.
 *
 * Generate a rectangular mesh of hexahedron elements.
 *
 * \sa Generate2DRectilinearMesh
 */
void Generate3DRectilinearMesh(itk::fem::Element::ConstPointer e0, Solver& S, vnl_vector<double>& orig,
 vnl_vector<double>& size, vnl_vector<double>& Nel);

}} // end namespace itk::fem

#endif // #ifndef __itkFEMGenerateMesh_h
