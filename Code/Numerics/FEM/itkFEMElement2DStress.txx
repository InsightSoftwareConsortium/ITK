/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DStress.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DStress_txx
#define __itkFEMElement2DStress_txx

#include "itkFEMElement2DStress.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




template<class TBaseClass>
Element2DStress<TBaseClass>
::Element2DStress() : Superclass(), m_mat(0) {}




//////////////////////////////////////////////////////////////////////////
/*
 * Methods related to the physics of the problem.
 */

template<class TBaseClass>
void
Element2DStress<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType& B, const MatrixType& shapeDgl) const
{
  int p;
  unsigned int Nn=this->GetNumberOfNodes();
  B.resize(3,2*Nn);
  
  // Copy the shape function derivatives to the B matrix.
  for (int i=0; i<Nn; i++) {
    // Compute B index
    p = i << 1;

    // Compute B elements
    B[0][p]   = shapeDgl[0][i];
    B[0][p+1] = 0;
    B[1][p]   = 0;
    B[1][p+1] = shapeDgl[1][i];
    B[2][p]   = shapeDgl[1][i];
    B[2][p+1] = shapeDgl[0][i];
  }
}




template<class TBaseClass>
void
Element2DStress<TBaseClass>
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



template<class TBaseClass>
void
Element2DStress<TBaseClass>
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
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Element2DStress::Read()",e.m_baseClassName,e.m_GN);
  }

  // Check if the material object was of correct class
  if(!m_mat)
  {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element2DStress::Read()");
  }

out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DStress::Read()","Error reading FEM element!");
  }

}



/*
 * Write the element to the output stream.
 */
template<class TBaseClass>
void
Element2DStress<TBaseClass>
::Write( std::ostream& f, int clid ) const
{
  // Element2DStress cannot be the most derived class, so
  // if clid was not set already, we throw an exception.
  if (clid<0)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DStress::Write()","Error writing FEM element!. Parameter clid was not set!");
  }

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
    throw FEMExceptionIO(__FILE__,__LINE__,"Element2DStress::Write()","Error writing FEM element!");
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

#endif // #ifndef __itkFEMElement2DStress_txx
