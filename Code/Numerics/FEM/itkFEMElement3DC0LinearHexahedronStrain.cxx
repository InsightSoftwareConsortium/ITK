/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DC0LinearHexahedronStrain.cxx
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

#include "itkFEMElement3DC0LinearHexahedronStrain.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {


Element3DC0LinearHexahedronStrain
::Element3DC0LinearHexahedronStrain() : Element3DC0LinearHexahedron<3>(), m_mat(0) {}

Element3DC0LinearHexahedronStrain
::Element3DC0LinearHexahedronStrain(
      NodeIDType ns_[],
      Material::ConstPointer m_)
{
  // Set the geometrical points
  for (int k=0; k<8; k++) { 
    this->SetNode( k, ns_[k] ); 
  }

  /*
   * Initialize the pointer to material object and check that
   * we were given the pointer to the right class.
   * If the material class was incorrect an exception is thrown.
   */
  if( (m_mat=dynamic_cast<const MaterialLinearElasticity*>(&*m_)) == 0 )
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element3DC0LinearHexahedronStrain::Element3DC0LinearHexahedronStrain()");
  }
}




//////////////////////////////////////////////////////////////////////////
/*
 * Methods related to the physics of the problem.
 */

void Element3DC0LinearHexahedronStrain
::GetStrainDisplacementMatrix(MatrixType& B, const MatrixType& shapeDgl) const
{
  int p;
  B.resize(6,24);

  // Initialize the B matrix to zero - subsequently, only the nonzero
  // terms will be filled in
  B.fill(0.0);

  // Copy the shape function derivatives wrt global coordinates
  // in right position in B matrix.

  for (int i=0; i<24; i++) {  
    p = i / 3;
    
    switch(i % 3) {
      case 0:  /** Columns 1, 4, 7, ..., 22 */
        B[0][i] = shapeDgl[0][p];
        B[3][i] = shapeDgl[1][p];
        B[5][i] = shapeDgl[2][p];
        break;
        
        case 1:  /** Columns 2, 5, 8, ..., 23 */
        B[1][i] = shapeDgl[1][p];
        B[3][i] = shapeDgl[0][p];
        B[4][i] = shapeDgl[2][p];
        break;

      case 2:  /** Columns 3, 6, 9, ..., 24 */
        B[2][i] = shapeDgl[2][p];
        B[4][i] = shapeDgl[1][p];
        B[5][i] = shapeDgl[0][p];
        break;    
    }
  }

}




void
Element3DC0LinearHexahedronStrain
::GetMaterialMatrix(MatrixType& D) const
{
  D.resize(6,6);
  D.fill(0.0);

  /* Material properties matrix */
  Float fac = (m_mat->h * m_mat->E) / ((1 + m_mat->ni) * (1 - 2 * m_mat->ni));
    
  /** Set the elements in the top left quadrant */
  for (int j=0; j < 3; j++) {
    for (int k=0; k < 3; k++) {
      D[j][k] = m_mat->ni;
    }
  }

  /** Set the diagonal elements */
  for (int k=0; k < 3; k++) {
    D[k][k] = 1 - m_mat->ni;
  }
  for (int k=3; k < 6; k++) {
    D[k][k] = (1 - (2 * m_mat->ni)) * 0.5;
  }

  /** Multiply by the factor */
  D = D * fac;
}



void
Element3DC0LinearHexahedronStrain
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
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Element3DC0LinearHexahedronStrain::Read()",e.m_baseClassName,e.m_GN);
  }


out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element3DC0LinearHexahedronStrain::Read()","Error reading FEM element!");
  }

}



/*
 * Write the element to the output stream.
 */
void
Element3DC0LinearHexahedronStrain
::Write( std::ostream& f, int clid ) const {

  /* If not set already, se set the clid */
  if (clid<0) clid=CLID;

  /** First call the parent's write function */
  Superclass::Write(f,clid);

  /*
   * then write the actual data (material number)
   * We also add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialLinearElasticity ID\n";

  // check for errors
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element3DC0LinearHexahedronStrain::Write()","Error writing FEM element!");
  }

}




FEM_CLASS_REGISTER(Element3DC0LinearHexahedronStrain)




}} // end namespace itk::fem
