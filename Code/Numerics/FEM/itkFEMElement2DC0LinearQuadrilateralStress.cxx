/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearQuadrilateralStress.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {


Element2DC0LinearQuadrilateralStress
::Element2DC0LinearQuadrilateralStress() : Element2DC0LinearQuadrilateral<2>(), m_mat(0) {}

Element2DC0LinearQuadrilateralStress
::Element2DC0LinearQuadrilateralStress(
      PointIDType n1_,
      PointIDType n2_,
      PointIDType n3_,
      PointIDType n4_,
      Material::ConstPointer m_)
{
  // Set the geometrical points
  this->SetPoint( 0, n1_ );
  this->SetPoint( 1, n2_ );
  this->SetPoint( 2, n3_ );
  this->SetPoint( 3, n4_ );

  /*
   * Initialize the pointer to material object and check that
   * we were given the pointer to the right class.
   * If the material class was incorrect an exception is thrown.
   */
  if( (m_mat=dynamic_cast<const MaterialLinearElasticity*>(&*m_)) == 0 )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element2DC0LinearQuadrilateralStress::Element2DC0LinearQuadrilateralStress()");
  }
}




//////////////////////////////////////////////////////////////////////////
/*
 * Methods related to the physics of the problem.
 */

void Element2DC0LinearQuadrilateralStress
::GetStrainDisplacementMatrix(VectorType pt, MatrixType& B) const
{
  int p;
  MatrixType shapeINVD(2,4);
  B.resize(3,8);
  
  /*
   * Computes the shape function derivatives in Cartesian coordinates
   * at integration point
   */
  ShapeFunctionGlobalDerivatives(pt, shapeINVD);

  /* Computes the inverse shape function derivatives */
  for (int i=0; i<4; i++) {
    /* Computes B index */
    p = i << 1;

    /* Compute B elements */
    B[0][p]   = shapeINVD[0][i];
    B[0][p+1] = 0;
    B[1][p]   = 0;
    B[1][p+1] = shapeINVD[1][i];
    B[2][p]   = shapeINVD[1][i];
    B[2][p+1] = shapeINVD[0][i];
  }
}




void
Element2DC0LinearQuadrilateralStress
::GetMaterialMatrix(MatrixType& D) const
{
  D.resize(3,3);

  /* Material properties matrix */
  Float disot = (m_mat->h * m_mat->E)/(1.0 - (m_mat->ni*m_mat->ni));
    
  D[0][0] = disot;
  D[0][1] = disot * (m_mat->ni);
  D[0][2] = 0.0;

  D[1][0] = D[0][1];
  D[1][1] = disot;
  D[1][2] = 0.0;

  D[2][0] = 0.0;
  D[2][1] = 0.0;
  D[2][2] = disot * (1.0 - m_mat->ni)/2.0;
}



void
Element2DC0LinearQuadrilateralStress
::Read( std::istream& f, void* info )
{
  int n;
  /*
   * Convert the info pointer to a usable objects
   */
  Material::ArrayType::Pointer mats=static_cast<ReadInfoType*>(info)->m_mat;


  /* first call the parent's read function */
  Superclass::Read(f,info);

  try
  {
    /*
     * Read and set the material pointer
     */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<const MaterialLinearElasticity*>( &*mats->Find(n));

  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Element2DC0LinearQuadrilateralStress::Read()",e.m_baseClassName,e.m_GN);
  }


out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DC0LinearQuadrilateralStress::Read()","Error reading FEM element!");
  }

}



/*
 * Write the element to the output stream.
 */
void
Element2DC0LinearQuadrilateralStress
::Write( std::ostream& f, int ofid ) const {

  /* If not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** First call the parent's write function */
  Superclass::Write(f,ofid);

  /*
   * then write the actual data (material number)
   * We also add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialLinearElasticity ID\n";

  // check for errors
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DC0LinearQuadrilateralStress::Write()","Error writing FEM element!");
  }

}




FEM_CLASS_REGISTER(Element2DC0LinearQuadrilateralStress)




}} // end namespace itk::fem
