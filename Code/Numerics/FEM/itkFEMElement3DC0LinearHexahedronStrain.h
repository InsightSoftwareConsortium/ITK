/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DC0LinearHexahedronStrain.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement3DC0LinearHexahedronStrain_h
#define __itkFEMElement3DC0LinearHexahedronStrain_h

#include "itkFEMElement3DC0LinearHexahedron.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk {
namespace fem {




/**
 * \class HexahedronPlaneStrain
 * \brief 8-noded finite element class in 3D space for linear elasticity problem
 */
class Element3DC0LinearHexahedronStrain : public Element3DC0LinearHexahedron<3>
{
FEM_CLASS(Element3DC0LinearHexahedronStrain,Element3DC0LinearHexahedron<3>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Read data for this class from input stream
   */
  void Read( std::istream&, void* info );

  /**
   * Write this class to output stream
   */
  void Write( std::ostream& f, int ofid ) const;

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearHexahedronStrain();

  /**
   * Construct an element by specifying pointers to
   * an array of 8 points and a material.
   */
  Element3DC0LinearHexahedronStrain(
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


public:

  /**
   * Pointer to material properties of the element
   */
  MaterialLinearElasticity::ConstPointer m_mat;

}; // class QuadPlaneStress

FEM_CLASS_INIT(Element3DC0LinearHexahedronStrain)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement3DC0LinearHexahedronStrain_h
