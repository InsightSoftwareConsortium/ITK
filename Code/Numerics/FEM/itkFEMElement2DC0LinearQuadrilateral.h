/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearQuadrilateral.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0LinearQuadrilateral_h
#define __itkFEMElement2DC0LinearQuadrilateral_h

#include "itkFEMElementStd.h"
#include "itkFEMNodeXY.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC0LinearQuadrilateral
 * \brief 4-noded, linear, C0 continuous finite element in 2D space.
 *
 * This class is templated over number of unknowns per point. This
 * constant must be defined in derived classes.
 */
template<unsigned int VNumberOfDegreesOfFreedomPerNode>
class Element2DC0LinearQuadrilateral : public ElementStd<4,VNumberOfDegreesOfFreedomPerNode,NodeXY>
{
typedef ElementStd<4,VNumberOfDegreesOfFreedomPerNode,NodeXY> TemplatedParentClass;
FEM_CLASS_SP( Element2DC0LinearQuadrilateral, TemplatedParentClass )
public:


  // Repeat typedefs and enums from parent class
  typedef typename Superclass::Float Float;
  typedef typename Superclass::MatrixType MatrixType;
  typedef typename Superclass::VectorType VectorType;
  typedef typename Superclass::LoadElementType LoadElementType;
  typedef typename Superclass::LoadElementPointer LoadElementPointer;
  typedef typename Superclass::PointIDType PointIDType;
  typedef typename Superclass::DegreeOfFreedomIDType DegreeOfFreedomIDType;
  typedef typename Superclass::NodeDefinitionType NodeDefinitionType;
  typedef typename Superclass::ReadInfoType ReadInfoType;
  typedef typename Superclass::PointClass PointClass;
  enum{ InvalidDegreeOfFreedomID = Superclass::InvalidDegreeOfFreedomID };
  enum{ NumberOfPoints=Superclass::NumberOfPoints };
  enum{ NumberOfNodes=Superclass::NumberOfNodes };
  enum{ NumberOfDegreesOfFreedomPerNode=Superclass::NumberOfDegreesOfFreedomPerNode };
  enum{ NDOF=Superclass::NDOF };


//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to numeric integration
   */

  virtual VectorType GetIntegrationPoint(unsigned int i) const;

  virtual Float GetWeightAtIntegrationPoint(unsigned int i) const;

  virtual unsigned int GetNumberOfIntegrationPoints() const;



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the geometry of an element
   */

  virtual VectorType ShapeFunctions( const VectorType& pt ) const;

  virtual void ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const;

  virtual VectorType GetNodeCoordinates( unsigned int n ) const;

  /**
   * Draw the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC, Solution::ConstPointer sol) const;
#endif


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
#include "itkFEMElement2DC0LinearQuadrilateral.txx"
#endif

#endif  // #ifndef __itkFEMElement2DC0LinearQuadrilateral_h
