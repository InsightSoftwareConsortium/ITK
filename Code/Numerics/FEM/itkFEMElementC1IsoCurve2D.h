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

#include "itkFEMElementStandard.h"
#include "itkFEMNodeXY.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"
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
class C1IsoCurve2D : public ElementStandard<4,1,NodeXY>
{
typedef ElementStandard<4,1,NodeXY> TemplatedParentClass;
FEM_CLASS(C1IsoCurve2D,TemplatedParentClass)
public:  
   
  /**
   * 2 isotropic dimensions for each DOF
   */
  enum {IsotropicDOF=2};
  
  /**
   * Required virtual functions
   */

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

  int current_match_index;

  
  /** interpolation with hermite shape functions */
  vnl_vector<Float> InterpolateWithShapeFunctions(Float s);  


  /**
   * Default constructor only clears the internal storage
   */
  C1IsoCurve2D() : mat(0) {}

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
  void Write( std::ostream& f, int clid ) const ;

protected:
  
  vnl_matrix<Float> ControlVec;

public:
  /**
   * Pointer to geometric and material properties of the element
   */
  MaterialStandard::ConstPointer mat;
  
};

FEM_CLASS_INIT(C1IsoCurve2D)




}} //end namespace itk::fem

#endif // #ifndef __itkElementC1IsoCurve2D_h             
