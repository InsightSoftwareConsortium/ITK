/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementC1IsoCurve2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkElementC1IsoCurve2D_h
#define __itkElementC1IsoCurve2D_h

#include "itkFEMElementBase.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
#include "itkFEMNode2DIsotropic.h"
#include "itkFEMMaterialStandard.h"
#include "vnl/vnl_math.h"

namespace itk {
namespace fem {




/**
 * \class C1IsoCurve2D
 * \brief An isotropic C1 curve element for 2D.
 *
 * An isotropic C1 curve element for 2D.  To expand to ND, must
 * implement correct node class.  Derivatives are estimated by using
 * neighboring node positions.  interpolation is with cubic hermite
 * polynomials.    
 */
class C1IsoCurve2D : public Element
{
FEM_CLASS(C1IsoCurve2D,Element)
public:  
   
  /**
   * 4 DOF. constant for faster access within the class
   *        3 of which overlap
   */
  enum {NDOF=4};

  /**
   * 2 isotropic dimensions for each DOF
   */
  enum {IsotropicDOF=2};
  
  /**
   * Required virtual functions
   */

  /**
   * Access to NDOF from base class
   */
  int N() const { return NDOF; };
  
  /**
   * Access to IsotropicDOF
   */
  int NI() const { return IsotropicDOF; };

  /**
   * Element stiffness matrix
   */
  vnl_matrix<Float> Ke() const;

  /*
   * Macro that defines a specific version of the Fe() function
   */
  LOAD_FUNCTION();

  /**
   * Pointers to DOF displacements, which are stored in node classes.
   */
  Displacement* uDOF(int i) const {
    switch ( i ) {
    case 0:
      return &neg_node1->v;
      break;
    case 1:
      return &cur_node->v;
      break;  
    case 2:
      return &pos_node1->v;
      break;
    case 3:
      return &pos_node2->v;
      break;  
    }
    return Element::uDOF(i);
  };

  int current_match_index;

  
  /** interpolation with hermite shape functions */
  vnl_vector<Float> InterpolateWithShapeFunctions(Float s);  


  /**
   * Default constructor only clears the internal storage
   */
  C1IsoCurve2D() : cur_node(0), neg_node1(0),
      pos_node1(0), pos_node2(0), mat(0) {}

  /**
   * Construct an element by specifying four nodes and material
   */
  C1IsoCurve2D(  Node::ConstPointer nn1_,
          Node::ConstPointer cn_,
          Node::ConstPointer pn1_,
          Node::ConstPointer pn2_,
          Material::ConstPointer mat_);

  /**
   * Read data of this class from input stream
   */
  void Read( std::istream& f, void* info );

  /**
   * Write this class to output stream
   */
  void Write( std::ostream& f, int ofid ) const ;

protected:
  
  vnl_matrix<Float> ControlVec;

public:
  /** Pointer to node that holds current position */
  Node2DIsotropic::ConstPointer cur_node;

  /** pointer to negative side of pos_node */
  Node2DIsotropic::ConstPointer neg_node1;
  
  /** pointer to negative sides of cur_node */
  Node2DIsotropic::ConstPointer pos_node1;
  Node2DIsotropic::ConstPointer pos_node2;

  /**
   * Pointer to geometric and material properties of the element
   */
  MaterialStandard::ConstPointer mat;
  
};

FEM_CLASS_INIT(C1IsoCurve2D)




}} //end namespace itk::fem

#endif // #ifndef __itkElementC1IsoCurve2D_h             
