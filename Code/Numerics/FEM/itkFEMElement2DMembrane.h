/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DMembrane.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DMembrane_h
#define __itkFEMElement2DMembrane_h

#include "itkFEMElementBase.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk {
namespace fem {




/**
 * \class Element2DMembrane
 * \brief Class that is used to define a membrane energy problem in 2D space.
 *
 * This class only defines the physics of the problem. Use his class together
 * with element classes that specify the geometry to fully define the element.
 *
 * You can specify one template parameter:
 *
 *   TBaseClass - Class from which Element2DMembrane is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element base class.
 */
template<class TBaseClass=Element>
class Element2DMembrane : public TBaseClass
{
FEM_ABSTRACT_CLASS(Element2DMembrane,TBaseClass)
public:

  // Repeat the required typedefs and enums from parent class
  typedef typename Superclass::Float Float;
  typedef typename Superclass::MatrixType MatrixType;
  typedef typename Superclass::VectorType VectorType;

  /**
   * Read data for this class from input stream
   */
  virtual void Read( std::istream&, void* info );

  /**
   * Write this class to output stream
   */
  virtual void Write( std::ostream& f ) const;

  /**
   * Default constructor only clears the internal storage
   */
  Element2DMembrane();




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
   * Compute the mass matrix specific for 2D stress problems.
   */
  void GetMassMatrix(MatrixType& Me) const;


  /**
   * 2D stress elements have 2 DOFs per node.
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode( void ) const
  { return 2; }



public:

  /**
   * Pointer to material properties of the element
   */
  MaterialLinearElasticity::ConstPointer m_mat;
  virtual Material::ConstPointer GetMaterial(void) const { return m_mat; }
  virtual void SetMaterial(Material::ConstPointer mat_ ) { m_mat=dynamic_cast<const MaterialLinearElasticity*>(&*mat_); }


}; // class Element2DMembrane




#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMElement2DMembrane.txx"
#endif

#endif  // #ifndef __itkFEMElement2DMembrane_h
