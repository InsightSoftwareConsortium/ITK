/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DStrain.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement3DStrain_txx
#define __itkFEMElement3DStrain_txx

#include "itkFEMElement3DStrain.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




template<class TBaseClass>
Element3DStrain<TBaseClass>
::Element3DStrain() : Superclass(), m_mat(0) {}




//////////////////////////////////////////////////////////////////////////
/*
 * Methods related to the physics of the problem.
 */

template<class TBaseClass>
void Element3DStrain<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType& B, const MatrixType& shapeDgl) const
{
  int p;
  unsigned int Nn=2*this->GetNumberOfNodes();
  B.resize(6,Nn);

  // Initialize the B matrix to zero - subsequently, only the nonzero
  // terms will be filled in
  B.fill(0.0);

  // Copy the shape function derivatives wrt global coordinates
  // in right position in B matrix.

  for (int i=0; i<Nn; i++) {  
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




template<class TBaseClass>
void
Element3DStrain<TBaseClass>
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




template<class TBaseClass>
void
Element3DStrain<TBaseClass>
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
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Element3DStrain::Read()",e.m_baseClassName,e.m_GN);
  }

  // Check if the material object was of correct class
  if(!m_mat)
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element3DStress::Read()");
  }


out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element3DStrain::Read()","Error reading FEM element!");
  }

}



/*
 * Write the element to the output stream.
 */
template<class TBaseClass>
void
Element3DStrain<TBaseClass>
::Write( std::ostream& f, int clid ) const {

  // Element3DStress cannot be the most derived class, so
  // if clid was not set already, we throw an exception.
  if (clid<0)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Element3DStress::Write()","Error writing FEM element!. Parameter clid was not set!");
  }

  // First call the parent's write function
  Superclass::Write(f,clid);

  /*
   * then write the actual data (material number)
   * We also add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialLinearElasticity ID\n";

  // check for errors
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element3DStrain::Write()","Error writing FEM element!");
  }

}




#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#endif // #ifndef __itkFEMElement3DStrain_txx
