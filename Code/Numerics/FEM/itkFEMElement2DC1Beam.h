/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC1Beam.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC1Beam_h
#define __itkFEMElement2DC1Beam_h

#include "itkFEMElementStd.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC1Beam
 * \brief 1D Beam (spring that also bends) finite element in 2D space.
 */
class Element2DC1Beam : public ElementStd<2,2>
{
typedef ElementStd<2,2> TemplatedParentClass;
FEM_CLASS(Element2DC1Beam,TemplatedParentClass)
public:

  // FIXME: Write this class in the same way as the others - 
  //        properly define all virtual functions.

  /**
   * Default constructor only clears the internal storage
   */
  Element2DC1Beam();

  /**
   * Construct an element by specifying two nodes and material
   */
  Element2DC1Beam(  Node::ConstPointer n1_, 
      Node::ConstPointer n2_, 
      Material::ConstPointer mat_);

  /**
   * Read data of this class from input stream
   */
  void Read( std::istream&, void* info );

  /**
   * Write this class to output stream
   */
  void Write( std::ostream& f ) const;



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the physics of the problem.
   */
  virtual void GetStiffnessMatrix( MatrixType& Ke ) const;
  virtual void GetMassMatrix( MatrixType& Me ) const;
  HANDLE_ELEMENT_LOADS();

  virtual void GetStrainDisplacementMatrix( MatrixType&, const MatrixType& ) const {}
  virtual void GetMaterialMatrix( MatrixType& ) const {}


//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to numeric integration
   */

  enum { DefaultIntegrationOrder = 1 };
  virtual void GetIntegrationPointAndWeight( unsigned int i, VectorType& pt, Float& w, unsigned int order=0 ) const;
  virtual unsigned int GetNumberOfIntegrationPoints( unsigned int order ) const;




//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the geometry of an element
   */

  virtual VectorType ShapeFunctions( const VectorType& pt ) const;
  virtual void ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const;
  virtual bool GetLocalFromGlobalCoordinates( const VectorType&, VectorType& ) const
  {
    return false;
  }
  virtual Float JacobianDeterminant( const VectorType& pt, const MatrixType* pJ ) const;

  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode( void ) const
  { return 3; }

  /**
   * Draws the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC, Solution::ConstPointer sol) const;
#endif


public:

  /**
   * Pointer to geometric and material properties of the element
   */
  MaterialLinearElasticity::ConstPointer m_mat;
  virtual Material::ConstPointer GetMaterial(void) const { return m_mat; }
  virtual void SetMaterial(Material::ConstPointer mat_ ) { m_mat=dynamic_cast<const MaterialLinearElasticity*>(&*mat_); }


};

FEM_CLASS_INIT(Element2DC1Beam)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElement2DC1Beam_h
