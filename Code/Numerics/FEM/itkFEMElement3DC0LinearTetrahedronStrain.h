/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DC0LinearTetrahedronStrain.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement3DC0LinearTetrahedronStrain_h
#define __itkFEMElement3DC0LinearTetrahedronStrain_h

#include "itkFEMElement3DC0LinearTetrahedron.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk {
namespace fem {




/**
 * \class TetrahedronPlaneStrain
 * \brief 4-noded finite element class in 3D space for linear elasticity problem
 */
class Element3DC0LinearTetrahedronStrain : public Element3DC0LinearTetrahedron
{
FEM_CLASS(Element3DC0LinearTetrahedronStrain,Element3DC0LinearTetrahedron)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Read data for this class from input stream
   */
  void Read( std::istream&, void* info );

  /**
   * Write this class to output stream
   */
  void Write( std::ostream& f, int clid ) const;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearTetrahedronStrain();

  /**
   * Construct an element by specifying pointers to
   * an array of 4 points and a material.
   */
  Element3DC0LinearTetrahedronStrain(
      NodeIDType ns_[], 
      Material::ConstPointer p_ );



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the physics of the problem.
   */

  /**
   * Compute the B matrix.
   */
  virtual void GetStrainDisplacementMatrix(MatrixType& B, const MatrixType& shapeDgl) const;

  /**
   * Compute the D matrix.
   */
  virtual void GetMaterialMatrix(MatrixType& D) const;

  /**
   * 3D strain elements have 3 DOFs per node.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode( void ) const
  { return 3; }

public:

  /**
   * Pointer to material properties of the element
   */
  MaterialLinearElasticity::ConstPointer m_mat;

}; // class QuadPlaneStress

FEM_CLASS_INIT(Element3DC0LinearTetrahedronStrain)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement3DC0LinearTetrahedronStrain_h
