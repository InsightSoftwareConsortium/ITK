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
#ifndef __itkFEMElement1DStress_txx
#define __itkFEMElement1DStress_txx

#include "itkFEMElement1DStress.h"

namespace itk {
namespace fem {

template<class TBaseClass>
Element1DStress<TBaseClass>
::Element1DStress() : Superclass(), m_mat(0) {}

//////////////////////////////////////////////////////////////////////////
/**
 * Methods related to the physics of the problem.
 */

template<class TBaseClass>
void
Element1DStress<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType& B, const MatrixType& shapeDgl) const
{
  B.set_size(1,2);

  // Copy the shape function derivatives to the B matrix.
  B[0][0] = shapeDgl[0][0];
  B[0][1] = shapeDgl[0][1];
}

template<class TBaseClass>
void
Element1DStress<TBaseClass>
::GetMaterialMatrix(MatrixType& D) const
{
  D.set_size(1,1);
  D.fill(0.0);

  // Material properties matrix is a scalar
  D[0][0] = (m_mat->E*m_mat->A);
}

template<class TBaseClass>
void
Element1DStress<TBaseClass>
::GetStiffnessMatrix(MatrixType& Ke) const
{
  const unsigned int Ndims=this->GetNumberOfSpatialDimensions();
  const unsigned int Nn=this->GetNumberOfNodes();

  // First we obtain the Ke by calling the parent's
  // GetStiffnessMatrix function. This is the stiffness matrix
  // with only one DOF per node.
  Superclass::GetStiffnessMatrix(Ke);

  // Calculate the nodal displacement transformation matrix according
  // to the number of dimensions in global coordinate system
  MatrixType T(2,2*Ndims,0.0);
  VectorType d=this->GetNodeCoordinates(1)-this->GetNodeCoordinates(0);
  d=d/d.magnitude();
  for(unsigned int i=0;i<Ndims;i++)
    {
    for(unsigned int n=0;n<Nn;n++)
      {
      T[n][n*Ndims+i]=d[i];
      }
    }

  // Apply the nodal displacement transformation matrix to original
  // element stiffness matrix to obtain the element stiffness
  // matrix in global coordinates.
  Ke=T.transpose()*Ke*T;

}

template<class TBaseClass>
void
Element1DStress<TBaseClass>
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
    this->SkipWhiteSpace(f); f>>n; if(!f) goto out;
    m_mat=dynamic_cast<const MaterialLinearElasticity*>( &*mats->Find(n));

    }
  catch ( FEMExceptionObjectNotFound e )
    {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"Element1DStress::Read()",e.m_baseClassName,e.m_GN);
    }

  // Check if the material object was of correct class
  if(!m_mat)
    {
    throw FEMExceptionWrongClass(__FILE__,__LINE__,"Element1DStress::Read()");
    }

out:

  if( !f )
    {
    throw FEMExceptionIO(__FILE__,__LINE__,"Element1DStress::Read()","Error reading FEM element!");
    }

}

/**
 * Write the element to the output stream.
 */
template<class TBaseClass>
void
Element1DStress<TBaseClass>
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
    throw FEMExceptionIO(__FILE__,__LINE__,"Element1DStress::Write()","Error writing FEM element!");
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

#endif // #ifndef __itkFEMElement1DStress_txx
