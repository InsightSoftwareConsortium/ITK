/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DMembrane.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DMembrane_txx
#define __itkFEMElement2DMembrane_txx

#include "itkFEMElement2DMembrane.h"

namespace itk {
namespace fem {




template<class TBaseClass>
Element2DMembrane<TBaseClass>
::Element2DMembrane() : Superclass(), m_mat(0) {}




//////////////////////////////////////////////////////////////////////////
/*
 * Methods related to the physics of the problem.
 */

template<class TBaseClass>
void
Element2DMembrane<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType& B, const MatrixType& shapeDgl) const
{
  unsigned int p;
  unsigned int Nn=this->GetNumberOfNodes();
  B.resize(4,2*Nn); // note minor difference from linear elasticity
  
  // Copy the shape function derivatives to the B matrix.
  for (unsigned int i=0; i<Nn; i++) {
    // Compute B index
    p = i << 1;

    // Compute B elements
    B[0][p]   = shapeDgl[0][i];
    B[0][p+1] = 0.0;

    B[1][p]   = 0.0;
    B[1][p+1] = shapeDgl[0][i];

    B[2][p]   = shapeDgl[1][i];
    B[2][p+1] = 0.0;

    B[3][p]   = 0.0;
    B[3][p+1] = shapeDgl[1][i];
  }
}




template<class TBaseClass>
void
Element2DMembrane<TBaseClass>
::GetMassMatrix(MatrixType& Me) const
{
  // Call the parent's get matrix function
  Superclass::GetMassMatrix(Me);

  // Since parent class doesn't have the material properties,
  // we need to adjust Me matrix here for the density of the element.
  Me=Me*m_mat->RhoC;
}




template<class TBaseClass>
void
Element2DMembrane<TBaseClass>
::GetMaterialMatrix(MatrixType& D) const
{
  unsigned int d=4;
  D.resize(d,d);

  D.fill(0.0);

  // This is the main difference from the linear elasticity problem.
  /* Material properties matrix.  Simpler than linear elasticity. */
  Float disot = m_mat->E;
    
  for (unsigned int i=0; i<d; i++) D[i][i] = disot;

}



template<class TBaseClass>
void
Element2DMembrane<TBaseClass>
::Read( std::istream& f, void* info )
{
  int n;
  /*
   * Convert the info pointer to a usable objects
   */
  ReadInfoType::MaterialArrayPointer mats=static_cast<ReadInfoType*>(info)->m_mat;


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
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Element2DMembrane::Read()",e.m_baseClassName,e.m_GN);
  }

  // Check if the material object was of correct class
  if(!m_mat)
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element2DMembrane::Read()");
  }

out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DMembrane::Read()","Error reading FEM element!");
  }

}



/*
 * Write the element to the output stream.
 */
template<class TBaseClass>
void
Element2DMembrane<TBaseClass>
::Write( std::ostream& f ) const
{
  // First call the parent's write function
  Superclass::Write(f);

  /*
   * then write the actual data (material number)
   * We also add some comments in the output file
   */
  f<<"\t"<<m_mat->GN<<"\t% MaterialLinearElasticity ID\n";

  // check for errors
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DMembrane::Write()","Error writing FEM element!");
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

#endif // #ifndef __itkFEMElement2DMembrane_txx
