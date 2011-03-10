/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkFEMElement3DStrain_txx
#define __itkFEMElement3DStrain_txx

#include "itkFEMElement3DStrain.h"

namespace itk {
namespace fem {

template<class TBaseClass>
Element3DStrain<TBaseClass>
::Element3DStrain() : Superclass(), m_mat(0) {}

//////////////////////////////////////////////////////////////////////////
/**
 * Methods related to the physics of the problem.
 */

template<class TBaseClass>
void Element3DStrain<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType& B, const MatrixType& shapeDgl) const
{
  unsigned int p;
  unsigned int Nn=3*this->GetNumberOfNodes();
  B.set_size(6,Nn);

  // Initialize the B matrix to zero - subsequently, only the nonzero
  // terms will be filled in
  B.fill(0.0);

  // Copy the shape function derivatives wrt global coordinates
  // in right position in B matrix.

  for (unsigned int i=0; i<Nn; i++)
    {
    p = i / 3;

    switch(i % 3)
      {
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
  D.set_size(6,6);
  D.fill(0.0);

  /* Material properties matrix */
  Float fac = (m_mat->h * m_mat->E) / ((1 + m_mat->nu) * (1 - 2 * m_mat->nu));

  /** Set the elements in the top left quadrant */
  for (int j=0; j < 3; j++) {
  for (int k=0; k < 3; k++) {
  D[j][k] = m_mat->nu;
  }
  }

  /** Set the diagonal elements */
  for (int k=0; k < 3; k++) {
  D[k][k] = 1 - m_mat->nu;
  }
  for (int k=3; k < 6; k++) {
  D[k][k] = (1 - (2 * m_mat->nu)) * 0.5;
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
  ReadInfoType::MaterialArrayPointer mats=static_cast<ReadInfoType*>(info)->m_mat;


  /* first call the parent's read function */
  Superclass::Read(f,info);

  try
    {
    /**
     * Read and set the material pointer
     */
    this->SkipWhiteSpace(f); f>>n; if(!f) goto out;
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
