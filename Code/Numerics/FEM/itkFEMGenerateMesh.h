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
 * \class GenerateMesh
 * \brief Use this class to generate (simple) meshes in Solver.
 *
 * This class is templated over the class of the element, that will be
 * used in a mesh. Call the corresponding static member function to
 * create the mesh.
 *
 * \note All elements will be created by copying the existing element which
 *       is passed to the function. Only number and node pointers will
 *       be changed in copied element. Make sure that this element has material
 *       class and any other properties defined before generating a mesh.
 */
template<class TElementType>
class GenerateMesh
{
public:

  /**
   * Class which will be used to represent elements in a mesh.
   * Must be derived from Element base class.
   */
  typedef TElementType ElementType;
  
  /**
   * Vector type used in this class
   */
  typedef vnl_vector<double> VectorType;

  /**
   * Generate a uniform rectangular mesh by creating specific elements.
   *
   * \param e0   Pointer to an element object which will copied to create
   *             other elements in a mesh.
   * \param S    Reference to the Solver object that will hold the mesh. All
   *             existing elements, nodes, loads and materials in Solver
   *             are destroyed.
   * \param orig Vector specifying the origin of the rectangular mesh.
   * \param size Vector specifying the size of a mesh in each dimension.
   * \param Nel  Vector specifying the number of elements in each dimension.
   */
  static void Rectangular(
                          typename ElementType::ConstPointer e0,
                          Solver& S,
                          VectorType& orig,
                          VectorType& size,
                          VectorType& Nel
                         );

};




}} // end namespace itk::fem




// SGI's compiler requires declaration of specialized functions.
#ifdef __sgi

#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "itkFEMElement3DC0LinearHexahedron.h"

template<>
void
itk::fem::GenerateMesh<itk::fem::Element2DC0LinearQuadrilateral>
::Rectangular( ElementType::ConstPointer e0, Solver& S, VectorType& orig, VectorType& size, VectorType& Nel);

template<>
void
itk::fem::GenerateMesh<itk::fem::Element3DC0LinearHexahedron>
::Rectangular( ElementType::ConstPointer e0, Solver& S, VectorType& orig, VectorType& size, VectorType& Nel);

#endif // #ifndef __sgi




#endif // #ifndef __itkFEMGenerateMesh_h
