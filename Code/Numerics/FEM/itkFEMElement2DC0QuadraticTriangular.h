/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0QuadraticTriangular.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0QuadraticTriangular_h
#define __itkFEMElement2DC0QuadraticTriangular_h

#include "itkFEMElementStd.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC0QuadraticTriangular
 * \brief 3-noded, quadratic, C0 continuous finite element in 2D space.
 *
 * This class is templated over number of unknowns per point. This
 * constant must be defined in derived classes, when the physics of
 * the problem is known.
 */
template<unsigned int VNumberOfDegreesOfFreedomPerNode>
class Element2DC0QuadraticTriangular : public ElementStd<6,VNumberOfDegreesOfFreedomPerNode,2>
{
typedef ElementStd<3,VNumberOfDegreesOfFreedomPerNode,2> TemplatedParentClass;
FEM_ABSTRACT_CLASS( Element2DC0QuadraticTriangular, TemplatedParentClass )
public:


  // Repeat typedefs and enums from parent class
  typedef typename Superclass::Float Float;
  typedef typename Superclass::MatrixType MatrixType;
  typedef typename Superclass::VectorType VectorType;
  typedef typename Superclass::LoadElementType LoadElementType;
  typedef typename Superclass::LoadElementPointer LoadElementPointer;
  typedef typename Superclass::NodeIDType NodeIDType;
  typedef typename Superclass::DegreeOfFreedomIDType DegreeOfFreedomIDType;
  typedef typename Superclass::ReadInfoType ReadInfoType;
  enum{ InvalidDegreeOfFreedomID = Superclass::InvalidDegreeOfFreedomID };
  enum{ NumberOfNodes=Superclass::NumberOfNodes };
  enum{ NumberOfDegreesOfFreedomPerNode=Superclass::NumberOfDegreesOfFreedomPerNode };
  enum{ NDOF=Superclass::NDOF };

//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to numeric integration
   */

  enum { DefaultIntegrationOrder = 2 };

  virtual void GetIntegrationPointAndWeight(unsigned int i, VectorType& pt, Float& w, unsigned int order) const;

  virtual unsigned int GetNumberOfIntegrationPoints(unsigned int order) const;



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the geometry of an element
   */

  virtual VectorType ShapeFunctions( const VectorType& pt ) const;

  virtual void ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const;

  // FIXME: Write a proper implementation
  virtual VectorType GetLocalFromGlobalCoordinates( const VectorType& pt ) const { throw; return VectorType(); }

  // Since the Jacobian is not quadratic, we need to provide our
  // own implementation of calculating the determinant and inverse.
  virtual Float JacobianDeterminant( const VectorType& pt, const MatrixType* pJ = 0 ) const;
  virtual void JacobianInverse( const VectorType& pt, MatrixType& invJ, const MatrixType* pJ = 0 ) const;

  /**
   * Draw the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC, Solution::ConstPointer sol) const;
#endif

private:
  /**
   * Constants for integration rules.
   */
  static const Float trigGaussRuleInfo[6][7][4];

  /**
   * Array that holds number of integration point for each order
   * of numerical integration.
   */
  static const unsigned int Nip[6];

};




#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMElement2DC0QuadraticTriangular.txx"
#endif

#endif  // #ifndef __itkFEMElement2DC0QuadraticTriangular_h
