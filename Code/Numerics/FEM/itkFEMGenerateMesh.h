/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMGenerateMesh.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMGenerateMesh_h
#define __itkFEMGenerateMesh_h

#include "vnl/vnl_vector.h"
#include "itkFEMSolver.h"

namespace itk {
namespace fem {




/**
 * \function Generate2DRectilinearMesh
 * \brief Use this function to generate 2D meshes in Solver.
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

/**
 * Generate a rectangular mesh of quadrilateral elements
 */
void Generate2DRectilinearMesh(itk::fem::Element::ConstPointer e0, Solver& S, vnl_vector<double>& orig, vnl_vector<double>& size, vnl_vector<double>& Nel);


/**
 * \fucntion Generate3DRectilinearMesh
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
