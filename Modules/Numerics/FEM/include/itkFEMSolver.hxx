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
#ifndef itkFEMSolver_hxx
#define itkFEMSolver_hxx

#include "itkFEMSolver.h"

#include "itkFEMLoadNode.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMElementBase.h"
#include "itkFEMLoadBC.h"
#include "itkFEMLoadBCMFC.h"
#include "itkFEMLoadLandmark.h"
#include "itkTimeProbe.h"
#include "itkImageRegionIterator.h"

#include <algorithm>
#include "itkMath.h"

namespace itk
{
namespace fem
{

template <unsigned int VDimension>
Solver<VDimension>
::Solver()
{
  this->SetLinearSystemWrapper(&m_LinearSystemVNL);

  this->m_NGFN = 0;
  this->m_NMFC = 0;
  this->m_FEMObject = ITK_NULLPTR;
  this->m_Origin.Fill( 0.0 );
  this->m_Spacing.Fill( 1.0 );

  this->ProcessObject::SetNumberOfRequiredInputs(1);
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, this->MakeOutput(0) );
}

template <unsigned int VDimension>
Solver<VDimension>
::~Solver()
{
  FEMObjectType *output = this->GetOutput();
  output->Clear();
}

template <unsigned int VDimension>
void
Solver<VDimension>
::SetInput(FEMObjectType *fem)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0,
                                   const_cast<FEMObjectType *>( fem ) );
  this->m_FEMObject = fem;
  this->m_NGFN = fem->GetNumberOfDegreesOfFreedom();
  this->m_NMFC = fem->GetNumberOfMultiFreedomConstraints();
}

template <unsigned int VDimension>
void
Solver<VDimension>
::SetInput( unsigned int index, FEMObjectType * fem )
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(index,
                                   const_cast<FEMObjectType *>( fem ) );
  this->m_FEMObject = fem;
  this->m_NGFN = fem->GetNumberOfDegreesOfFreedom();
  this->m_NMFC = fem->GetNumberOfMultiFreedomConstraints();

}

template <unsigned int VDimension>
typename Solver<VDimension>::FEMObjectType *
Solver<VDimension>
::GetInput(void)
  {
  if( this->GetNumberOfInputs() < 1 )
    {
    return ITK_NULLPTR;
    }

  return itkDynamicCastInDebugMode<FEMObjectType *>(this->ProcessObject::GetInput(0) );
  }

template <unsigned int VDimension>
typename Solver<VDimension>::FEMObjectType *
Solver<VDimension>
::GetInput(unsigned int idx)
  {
  return itkDynamicCastInDebugMode<FEMObjectType *>(this->ProcessObject::GetInput(idx) );
  }

template <unsigned int VDimension>
typename Solver<VDimension>::Float
Solver<VDimension>
::GetTimeStep() const
{
  return NumericTraits< Float >::ZeroValue();
}

template <unsigned int VDimension>
void
Solver<VDimension>
::SetTimeStep(Float itkNotUsed(dt))
{
}

template <unsigned int VDimension>
typename Solver<VDimension>::Float
Solver<VDimension>
::GetSolution(unsigned int i, unsigned int which)
{
  return this->m_LinearSystem->GetSolutionValue(i, which);
}

template <unsigned int VDimension>
typename Solver<VDimension>::DataObjectPointer
Solver<VDimension>
::MakeOutput(DataObjectPointerArraySizeType itkNotUsed(idx))
{
  return FEMObjectType::New().GetPointer();
}

template <unsigned int VDimension>
typename Solver<VDimension>::FEMObjectType *
Solver<VDimension>
::GetOutput()
  {
  if( this->GetNumberOfOutputs() < 1 )
    {
    return ITK_NULLPTR;
    }

  return itkDynamicCastInDebugMode<FEMObjectType *>(this->ProcessObject::GetOutput(0));
  }

template <unsigned int VDimension>
typename Solver<VDimension>::FEMObjectType *
Solver<VDimension>
::GetOutput(unsigned int idx)
  {
  FEMObjectType* out = dynamic_cast<FEMObjectType *>
    (this->ProcessObject::GetOutput(idx) );

  if( out == ITK_NULLPTR )
    {
    itkWarningMacro( << "dynamic_cast to output type failed" );
    }
  return out;
  }

// ----------------------------------------------------------------------------
template <unsigned int VDimension>
void
Solver<VDimension>
::GenerateData()
{
  // Call Solver
  this->RunSolver();
}

template <unsigned int VDimension>
void
Solver<VDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Global degrees of freedom: " << m_NGFN << std::endl;
  os << indent << "Multi freedom constraints: " << m_NMFC << std::endl;
  os << indent << "FEM Object: " << m_FEMObject << std::endl;
}

template <unsigned int VDimension>
void
Solver<VDimension>
::SetLinearSystemWrapper(LinearSystemWrapper::Pointer ls)
{
  m_LinearSystem = ls; // update the pointer to LinearSystemWrapper object

  this->InitializeLinearSystemWrapper();
}

template <unsigned int VDimension>
void
Solver<VDimension>
::InitializeLinearSystemWrapper(void)
{
  // set the maximum number of matrices and vectors that
  // we will need to store inside.
  m_LinearSystem->SetNumberOfMatrices(1);
  m_LinearSystem->SetNumberOfVectors(2);
  m_LinearSystem->SetNumberOfSolutions(1);
}

template <unsigned int VDimension>
void
Solver<VDimension>
::AssembleK()
{
  // if no DOFs exist in a system, we have nothing to do
  int NGFN = m_FEMObject->GetNumberOfDegreesOfFreedom();

  if( NGFN <= 0 )
    {
    return;
    }

  int NMFC = 0;  // reset number of MFC in a system

  // Before we can start the assembly procedure, we need to know,
  // how many boundary conditions if form of MFCs are there in a system.

  // Search for MFC's in Loads array, because they affect the master stiffness
  // matrix
  int numLoads = m_FEMObject->GetLoadContainer()->Size();
  for( int l = 0; l < numLoads; l++ )
    {
    if( LoadBCMFC::Pointer l1 = dynamic_cast<LoadBCMFC *>( m_FEMObject->GetLoad(l).GetPointer() ) )
      {
      // store the index of an LoadBCMFC object for later
      l1->SetIndex(NMFC);

      // increase the number of MFC
      NMFC++;
      }
    }

  // Now we can assemble the master stiffness matrix from
  // element stiffness matrices.
  //
  // Since we're using the Lagrange multiplier method to apply the MFC,
  // each constraint adds a new global DOF.

  this->InitializeMatrixForAssembly(NGFN + NMFC);

  // Step over all elements
  unsigned int numberOfElements = m_FEMObject->GetNumberOfElements();
  for( unsigned int i = 0; i < numberOfElements; i++ )
    {
    // Call the function that actually moves the element matrix
    // to the master matrix.
    Element::Pointer e = m_FEMObject->GetElement( i );
    this->AssembleElementMatrix(e);
    }

  // Step over all the loads again to add the landmark contributions
  // to the appropriate place in the stiffness matrix
  unsigned int numberOfLoads = m_FEMObject->GetNumberOfLoads();
  for( unsigned int i = 0; i < numberOfLoads; i++ )
    {
    if( LoadLandmark::Pointer l3 = dynamic_cast<LoadLandmark *>( m_FEMObject->GetLoad(i).GetPointer() ) )
      {
      l3->AssignToElement(m_FEMObject->GetModifiableElementContainer() );
      // dynamic_cast< LoadLandmark * >( &( *( *l2 ) ) ) )
      Element::ConstPointer ep = l3->GetElement(0).GetPointer();
      this->AssembleLandmarkContribution( ep, l3->GetEta() );
      }
    }

  this->FinalizeMatrixAfterAssembly();

}

template <unsigned int VDimension>
void
Solver<VDimension>
::InitializeMatrixForAssembly(unsigned int N)
{
  // We use LinearSystemWrapper object, to store the K matrix.
  this->m_LinearSystem->SetSystemOrder(N);
  this->m_LinearSystem->InitializeMatrix();
}

template <unsigned int VDimension>
void
Solver<VDimension>
::AssembleLandmarkContribution(Element::ConstPointer e, float eta)
{
  // Copy the element "landmark" matrix for faster access.
  Element::MatrixType Le;

  e->GetLandmarkContributionMatrix(eta, Le);

  // Same for number of DOF
  int Ne = e->GetNumberOfDegreesOfFreedom();
  // step over all rows in element matrix
  for( int j = 0; j < Ne; j++ )
    {
    // step over all columns in element matrix
    for( int k = 0; k < Ne; k++ )
      {
      // error checking. all GFN should be =>0 and <NGFN
      if( e->GetDegreeOfFreedom(j) >= this->m_NGFN
          || e->GetDegreeOfFreedom(k) >= this->m_NGFN  )
        {
        throw FEMExceptionSolution(__FILE__, __LINE__, "Solver::AssembleLandmarkContribution()", "Illegal GFN!");
        }

      // Here we finally update the corresponding element
      // in the master stiffness matrix. We first check if
      // element in Le is zero, to prevent zeros from being
      // allocated in sparse matrix.
      if( Math::NotExactlyEquals(Le[j][k], Float(0.0)) )
        {
        this->m_LinearSystem->AddMatrixValue(e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Le[j][k]);
        }
      }
    }
}

template <unsigned int VDimension>
void
Solver<VDimension>
::AssembleElementMatrix(Element::Pointer e)
{
  // Copy the element stiffness matrix for faster access.
  Element::MatrixType Ke;

  e->GetStiffnessMatrix(Ke);

  // Same for number of DOF
  int Ne = e->GetNumberOfDegreesOfFreedom();
  // step over all rows in element matrix
  for( int j = 0; j < Ne; j++ )
    {
    // step over all columns in element matrix
    for( int k = 0; k < Ne; k++ )
      {
      // error checking. all GFN should be =>0 and <NGFN
      if( e->GetDegreeOfFreedom(j) >= this->m_NGFN
          || e->GetDegreeOfFreedom(k) >= this->m_NGFN  )
        {
        throw FEMExceptionSolution(__FILE__, __LINE__, "Solver::AssembleElementMatrix()", "Illegal GFN!");
        }

      // Here we finally update the corresponding element
      // in the master stiffness matrix. We first check if
      // element in Ke is zero, to prevent zeros from being
      // allocated in sparse matrix.
      if( Math::NotExactlyEquals(Ke[j][k], Float(0.0)) )
        {
        this->m_LinearSystem->AddMatrixValue(e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Ke[j][k]);
        }
      }
    }
}

template <unsigned int VDimension>
void
Solver<VDimension>
::AssembleF(int dim)
{
  // Vector that stores element nodal loads
  Element::VectorType Fe;

  // Type that stores IDs of fixed DOF together with the values to
  // which they were fixed.
  typedef std::map<Element::DegreeOfFreedomIDType, Float> BCTermType;
  BCTermType bcterm;

  // If no DOFs exist in a system, we have nothing to do
  if( m_NGFN <= 0 )
    {
    return;
    }

  // Initialize the master force vector
  m_LinearSystem->InitializeVector();

  // Convert the external loads to the nodal loads and
  // add them to the master force vector F.
  unsigned int numberOfLoads = m_FEMObject->GetNumberOfLoads();
  for( unsigned int l = 0; l < numberOfLoads; l++ )
    {
    Load::Pointer l0 = m_FEMObject->GetLoad( l );

    // Pass the vector to the solution to the Load object.
    // FIXME: Can this be removed?
    l0->SetSolution(m_LinearSystem);

    // Here we only handle Nodal loads
    if( LoadNode::Pointer l1 = dynamic_cast<LoadNode *>( l0.GetPointer() ) )
      {
      // We have a nodal load
      // The size of a force vector in load must match number of DOFs in node
      if( ( l1->GetForce().size() % l1->GetElement()->GetNumberOfDegreesOfFreedomPerNode() ) != 0 )
        {
        throw FEMExceptionSolution(__FILE__,
                                   __LINE__,
                                   "Solver::AssembleF()",
                                   "Illegal size of a force vector in LoadNode object!");
        }
      // We simply copy the load to the force vector
      for( unsigned int d = 0; d < ( l1->GetElement()->GetNumberOfDegreesOfFreedomPerNode() ); d++ )
        {
        Element::DegreeOfFreedomIDType dof = l1->GetElement()->GetNode( l1->GetNode() )->GetDegreeOfFreedom(d);
        if( dof >= m_NGFN )
          {
          throw FEMExceptionSolution(__FILE__, __LINE__, "Solver::AssembleF()", "Illegal GFN!");
          }

        // If using the extra dim parameter, we can apply the force to
        // different isotropic dimension.
        //
        // FIXME: We assume that the implementation of force vector
        // inside the LoadNode class is correct for given number of
        // dimensions
        m_LinearSystem->AddVectorValue(dof, l1->GetForce()
                             [d + l1->GetElement()->GetNumberOfDegreesOfFreedomPerNode() * dim]);
        }

      // That's all there is to DOF loads, go to next load in an array
      continue;
      }

    // Element loads
    if( LoadElement::Pointer l1 = dynamic_cast<LoadElement *>( l0.GetPointer() ) )
      {
      if( !( l1->GetElementArray().empty() ) )
        {
        // If the array of element pointers is not empty,
        // we apply the load to all elements in that array.
        for( LoadElement::ElementPointersVectorType::const_iterator i = l1->GetElementArray().begin();
             i != l1->GetElementArray().end(); i++ )
          {
          const Element *el0 = ( *i );
          // Call the Fe() function of the element that we are applying the load
          // to.
          // We pass a pointer to the load object as a paramater and a reference
          // to the nodal loads vector.
          l1->ApplyLoad(el0, Fe);

          unsigned int Ne = el0->GetNumberOfDegreesOfFreedom(); // ... element's
                                                                // number of DOF
          for( unsigned int j = 0; j < Ne; j++ )                // step over all
                                                                // DOF
            {
            // Error checking
            if( el0->GetDegreeOfFreedom(j) >= m_NGFN )
              {
              throw FEMExceptionSolution(__FILE__, __LINE__, "Solver::AssembleF()", "Illegal GFN!");
              }

            // Update the master force vector (take care of the correct
            // isotropic dimensions).
            m_LinearSystem->AddVectorValue( el0->GetDegreeOfFreedom(j), Fe(j + dim * Ne) );
            }
          }
        }
      else
        {
        // If the list of element pointers in load object is empty,
        // we apply the load to all elements in a system.
        unsigned int numberOfElements = m_FEMObject->GetNumberOfElements();
        for( unsigned int e = 0; e < numberOfElements; e++ )
          {
          // Element::Pointer el = m_FEMObject->GetElement(e);
          const Element *el = m_FEMObject->GetElement(e);
          l1->ApplyLoad(el, Fe);                                   // ...
                                                                   // element's
                                                                   // force
                                                                   // vector

          unsigned int Ne = el->GetNumberOfDegreesOfFreedom(); // ...
                                                               // element's
                                                               // number of
                                                               // DOF
          for( unsigned int j = 0; j < Ne; j++ )               // step over all DOF
            {
            if( el->GetDegreeOfFreedom(j) >= m_NGFN )
              {
              throw FEMExceptionSolution(__FILE__, __LINE__, "Solver::AssembleF()", "Illegal GFN!");
              }

            // Update the master force vector (take care of the correct
            // isotropic dimensions).
            m_LinearSystem->AddVectorValue( el->GetDegreeOfFreedom(j), Fe(j + dim * Ne) );
            }
          }
        }

      // Skip to next load in an array
      continue;
      }

    // Handle boundary conditions in form of MFC loads are handled next.
    if( LoadBCMFC::Pointer l1 = dynamic_cast<LoadBCMFC *>( l0.GetPointer() ) )
      {
      m_LinearSystem->SetVectorValue( m_NGFN + l1->GetIndex(), l1->GetRightHandSideTerm(dim) );

      // Skip to next load in an array
      continue;
      }

    // Handle essential boundary conditions.
    if( LoadBC::Pointer l1 = dynamic_cast<LoadBC *>( l0.GetPointer() ) )
      {
      // Here we just store the values of fixed DOFs. We can't set it here,
      // because
      // it may be changed by other loads that are applied later.

      bcterm[l1->GetElement()->GetDegreeOfFreedom( l1->GetDegreeOfFreedom() )] =
        l1->GetValue()[dim];

      // skip to the next load in an array
      continue;
      }

    // If we got here, we were unable to handle that class of Load object.
    // We do nothing...
    //
    }  // for(LoadArray::iterator l ... )

  // Adjust the master force vector for essential boundary
  // conditions as required.
  if( m_LinearSystem->IsVectorInitialized(1) )
    {
    // Add the vector generated by ApplyBC to the solution vector
    const unsigned int totGFN = m_NGFN + m_NMFC;
    for( unsigned int i = 0; i < totGFN; i++ )
      {
      m_LinearSystem->AddVectorValue( i, m_LinearSystem->GetVectorValue(i, 1) );
      }
    }
  // Set the fixed DOFs to proper values
  for( BCTermType::iterator q = bcterm.begin(); q != bcterm.end(); q++ )
    {
    m_LinearSystem->SetVectorValue(q->first, q->second);
    }
}

template <unsigned int VDimension>
void
Solver<VDimension>
::DecomposeK()
{
}

template <unsigned int VDimension>
void
Solver<VDimension>
::RunSolver()
{

  itk::TimeProbe timer;

  timer.Start();

  this->AssembleK();

  this->AssembleF();

  // Check if master stiffness matrix and master force vector were
  // properly initialized.
  if( !m_LinearSystem->IsMatrixInitialized() )
    {
    throw FEMExceptionSolution(__FILE__, __LINE__, "FEMObject::Solve()", "Master stiffness matrix was not initialized!");
    }
  if( !m_LinearSystem->IsVectorInitialized() )
    {
    throw FEMExceptionSolution(__FILE__, __LINE__, "FEMObject::Solve()", "Master force vector was not initialized!");
    }
  timer.Stop();
  itkDebugMacro( << "Assemble Matrix took " << timer.GetMean() << " seconds.\n" );

  itk::TimeProbe timer1;
  timer1.Start();
  // Solve the system of linear equations
  m_LinearSystem->InitializeSolution();
  m_LinearSystem->Solve();

  // copy the input to the output and add the displacements to update the nodal co-ordinates
  this->GetOutput()->DeepCopy(this->GetInput() );
  this->UpdateDisplacements();
  timer1.Stop();
  itkDebugMacro( << "FE Solution took " << timer1.GetMean() << " seconds.\n" );
}

template <unsigned int VDimension>
void
Solver<VDimension>
::UpdateDisplacements()
{
  FEMObjectType *femObject = this->GetOutput();

  int numNodes = femObject->GetNumberOfNodes();

  typedef Element::Node NodeType;

  itk::fem::Element::VectorType pt(VDimension);
  for( int i = 0; i < numNodes; i++ )
    {
    NodeType::Pointer node = femObject->GetNode(i);
    for( unsigned int j = 0; j < VDimension; j++ )
      {
      pt[j] = node->GetCoordinates()[j] + m_LinearSystem->GetSolutionValue(node->GetDegreeOfFreedom(j));
      }
    node->SetCoordinates(pt);
    }
}

template <unsigned int VDimension>
typename Solver<VDimension>::Float
Solver<VDimension>
::GetDeformationEnergy(unsigned int SolutionIndex)
{
  Float U = 0.0f;
  Element::MatrixType LocalSolution;

  unsigned int numberOfElements = m_FEMObject->GetNumberOfElements();
  for( unsigned int index = 0; index < numberOfElements; index++ )
    {
    Element::Pointer e = m_FEMObject->GetElement( index );
    unsigned int Ne = e->GetNumberOfDegreesOfFreedom();
    LocalSolution.set_size(Ne, 1);
    // Step over all DOFs of element
    for( unsigned int j = 0; j < Ne; j++ )
      {
      LocalSolution[j][0] = m_LinearSystem->GetSolutionValue( e->GetDegreeOfFreedom(j), SolutionIndex );
      }

    U += e->GetElementDeformationEnergy(LocalSolution);
    }
  return U;
}

template <unsigned int VDimension>
void Solver<VDimension>
::ApplyBC(int dim, unsigned int matrix)
{
  // Vector with index 1 is used to store force correctios for BCs
  this->m_LinearSystem->DestroyVector(1);

  // Step over all Loads
  unsigned int numberOfLoads = this->m_FEMObject->GetNumberOfLoads();
  for( unsigned int i = 0; i < numberOfLoads; i++ )
    {

    Load::Pointer l0 = this->m_FEMObject->GetLoad( i );

    // Apply boundary conditions in form of MFC loads.
    //
    // We add the multi freedom constraints contribution to the master
    // stiffness matrix using the lagrange multipliers. Basically we only
    // change the last couple of rows and columns in K.
    if( LoadBCMFC::Pointer c = dynamic_cast<LoadBCMFC *>( l0.GetPointer() ) )
      {
      // Step over all DOFs in MFC
      for( LoadBCMFC::LhsType::iterator q = c->GetLeftHandSideArray().begin();
           q != c->GetLeftHandSideArray().end();
           q++ )
        {
        // Obtain the GFN of DOF that is in the MFC
        Element::DegreeOfFreedomIDType gfn =
          q->m_element->GetDegreeOfFreedom(q->dof);

        // Error checking. all GFN should be =>0 and <NGFN
        if( gfn >= m_NGFN )
          {
          throw FEMExceptionSolution(__FILE__, __LINE__, "Solver::ApplyBC()", "Illegal GFN!");
          }

        // Set the proper values in master stiffnes matrix.
        // It is a symetric matrix.
        this->m_LinearSystem->SetMatrixValue(gfn, m_NGFN + c->GetIndex(), q->value, matrix);
        this->m_LinearSystem->SetMatrixValue(m_NGFN + c->GetIndex(), gfn, q->value, matrix); //
                                                                                   // this
                                                                                   // is
                                                                                   // a
                                                                                   // symetric
                                                                                   // matrix...
        }

      // Skip to next load in an array
      continue;
      }


    // Apply essential boundary conditions
    if( LoadBC::Pointer c = dynamic_cast<LoadBC *>( l0.GetPointer() ) )
      {
      Element::DegreeOfFreedomIDType fdof = c->GetElement()->GetDegreeOfFreedom( c->GetDegreeOfFreedom() );
      Float                          fixedvalue = c->GetValue()[dim];

      // Copy the corresponding row of the matrix to the vector that will
      // be later added to the master force vector.
      // NOTE: We need to copy the whole row first, and then clear it. This
      //       is much more efficient when using sparse matrix storage, than
      //       copying and clearing in one loop.

      // Get the column indices of the nonzero elements in an array.
      LinearSystemWrapper::ColumnArray cols;
      this->m_LinearSystem->GetColumnsOfNonZeroMatrixElementsInRow(fdof, cols, matrix);

      // Force vector needs updating only if DOF was not fixed to 0.0.
      if( fixedvalue != 0.0 )
        {
        // Initialize the master force correction vector as required
        if( !this->m_LinearSystem->IsVectorInitialized(1) )
          {
          this->m_LinearSystem->InitializeVector(1);
          }
        // Step over each nonzero matrix element in a row
        for( LinearSystemWrapper::ColumnArray::iterator cc = cols.begin(); cc != cols.end(); cc++ )
          {
          // Get value from the stiffness matrix
          Float d = this->m_LinearSystem->GetMatrixValue(fdof, *cc, matrix);

          // Store the appropriate value in bc correction vector (-K12*u2)
          //
          // See
          // http://titan.colorado.edu/courses.d/IFEM.d/IFEM.Ch04.d/IFEM.Ch04.pdf
          // chapter 4.1.3 (Matrix Forms of DBC Application Methods) for more
          // info.
          this->m_LinearSystem->AddVectorValue(*cc, -d * fixedvalue, 1);
          }
        }
      // Clear that row and column in master matrix
      for( LinearSystemWrapper::ColumnArray::iterator cc = cols.begin(); cc != cols.end(); cc++ )
        {
        this->m_LinearSystem->SetMatrixValue(fdof, *cc, 0.0, matrix);
        this->m_LinearSystem->SetMatrixValue(*cc, fdof, 0.0, matrix); // this is a
                                                            // symetric matrix
        }
      this->m_LinearSystem->SetMatrixValue(fdof, fdof, 1.0, matrix); // Set the diagonal
                                                           // element to one

      // Skip to next load in an array
      continue;
      }
    } // End for LoadArray::iterator l
}

template <unsigned int VDimension>
void
Solver<VDimension>
::InitializeInterpolationGrid(const InterpolationGridSizeType & size,
                              const InterpolationGridPointType & bb1,
                              const InterpolationGridPointType & bb2)
{
  // Discard any old image object an create a new one
  m_InterpolationGrid = InterpolationGridType::New();

  // Set the interpolation grid (image) size, origin and spacing
  // from the given vectors, so that physical point of v1 is (0,0,0) and
  // phisical point v2 is (size[0],size[1],size[2]).
  InterpolationGridSizeType image_size;
  image_size.Fill(1);
  for( unsigned int i = 0; i < FEMDimension; i++ )
    {
    image_size[i] = size[i];
    }

  InterpolationGridPointType image_origin;
  image_origin.Fill(0.0);
  for( unsigned int i = 0; i < FEMDimension; i++ )
    {
    image_origin[i] = bb1[i];
    }

  InterpolationGridSpacingType image_spacing;
  image_origin.Fill(1.0);
  for( unsigned int i = 0; i < FEMDimension; i++ )
    {
    image_spacing[i] = ( bb2[i] - bb1[i] ) / ( image_size[i] - 1 );
    }

  // All regions are the same
  m_InterpolationGrid->SetRegions(image_size);
  m_InterpolationGrid->Allocate();

  // Set origin and spacing
  m_InterpolationGrid->SetOrigin(image_origin);
  m_InterpolationGrid->SetSpacing(image_spacing);

  // Initialize all pointers in interpolation grid image to 0
  m_InterpolationGrid->FillBuffer(0);

  FillInterpolationGrid();
}

template <unsigned int VDimension>
void
Solver<VDimension>
::FillInterpolationGrid( )
{
  VectorType v1, v2;

  InterpolationGridSizeType imageSize = m_InterpolationGrid->GetBufferedRegion().GetSize();

  // Fill the interpolation grid with proper pointers to elements
  unsigned int numberOfElements = m_FEMObject->GetNumberOfElements();
  for( unsigned int index = 0; index < numberOfElements; index++ )
    {
    Element::Pointer e = m_FEMObject->GetElement( index );
    // Get square boundary box of an element
    v1 = e->GetNodeCoordinates(0);      // lower left corner
    v2 = v1;                            // upper right corner

    const unsigned int NumberOfDimensions = e->GetNumberOfSpatialDimensions();
    for( unsigned int n = 1; n < e->GetNumberOfNodes(); n++ )
      {
      const VectorType & v = e->GetNodeCoordinates(n);
      for( unsigned int d = 0; d < NumberOfDimensions; d++ )
        {
        if( v[d] < v1[d] )
          {
          v1[d] = v[d];
          }
        if( v[d] > v2[d] )
          {
          v2[d] = v[d];
          }
        }
      }

    // Convert boundary box corner points into discrete image indexes.
    InterpolationGridIndexType vi1, vi2;

    Point<Float, FEMDimension> vp1, vp2, pt;
    for( unsigned int i = 0; i < FEMDimension; i++ )
      {
      vp1[i] = v1[i];
      vp2[i] = v2[i];
      }

    // Obtain the Index of BB corner and check whether it is within image.
    bool validLowerBound = m_InterpolationGrid->TransformPhysicalPointToIndex(vp1, vi1);
    bool validUpperBound = m_InterpolationGrid->TransformPhysicalPointToIndex(vp2, vi2);
    if( !validLowerBound && !validUpperBound )
      {
      continue;
      }

    // Adjust the lower bound if required
    if (!validLowerBound)
      {
      for( unsigned int i = 0; i < FEMDimension; i++ )
        {
        if ( vi1[i] < 0 )
          {
          vi1[i] = 0;
          }
        }
      }

    // Adjust the upper bound if required
    if (!validUpperBound)
      {
      for( unsigned int i = 0; i < FEMDimension; i++ )
        {
        if ( vi2[i] >= static_cast<int>(imageSize[i]) )
          {
          vi2[i] = static_cast<int>( imageSize[i] ) - 1;
          }
        }
      }

    InterpolationGridSizeType region_size;
    for( unsigned int i = 0; i < FEMDimension; i++ )
      {
      region_size[i] = vi2[i] - vi1[i] + 1;
      }
    InterpolationGridRegionType region(vi1, region_size);

    // Initialize the iterator that will step over all grid points within
    // element boundary box.
    ImageRegionIterator<InterpolationGridType> iter(m_InterpolationGrid, region);

    //
    // Update the element pointers in the points defined within the region.
    //
    VectorType global_point(NumberOfDimensions); // Point in the image as a
                                                 // vector.
    VectorType local_point(NumberOfDimensions);  // Same point in local element
                                                 // coordinate system
    // Step over all points within the region
    for( iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
      {
      // Note: Iteratior is guarantied to be within image, since the
      //       elements with BB outside are skipped before.
      m_InterpolationGrid->TransformIndexToPhysicalPoint(iter.GetIndex(), pt);
      for( unsigned int d = 0; d < NumberOfDimensions; d++ )
        {
        global_point[d] = pt[d];
        }

      // If the point is within the element, we update the pointer at
      // this point in the interpolation grid image.
      if( e->GetLocalFromGlobalCoordinates(global_point, local_point) )
        {
        iter.Set( e.GetPointer() );
        }
      } // next point in region
    }   // next element
}

template <unsigned int VDimension>
void
Solver<VDimension>
::InitializeInterpolationGrid(const InterpolationGridRegionType& region,
                              const InterpolationGridPointType& origin,
                              const InterpolationGridSpacingType& spacing,
                              const InterpolationGridDirectionType& direction)
{
  InterpolationGridSizeType size = region.GetSize();
  for( unsigned int i = 0; i < FEMDimension; i++ )
    {
    if( size[i] == 0 )
      {
      itkExceptionMacro("Size must be specified.");
      }
    }

  m_InterpolationGrid = InterpolationGridType::New();
  m_InterpolationGrid->SetOrigin( origin );
  m_InterpolationGrid->SetSpacing( spacing );
  m_InterpolationGrid->SetDirection( direction );
  m_InterpolationGrid->SetRegions( region );
  m_InterpolationGrid->Allocate();

   // Initialize all pointers in interpolation grid image to 0
  m_InterpolationGrid->FillBuffer(ITK_NULLPTR);

  FillInterpolationGrid();
}

template <unsigned int VDimension>
const Element *
Solver<VDimension>
::GetElementAtPoint(const VectorType & pt) const
{
  // Add zeros to the end of physical point if necesarry
  Point<Float, FEMDimension> pp;
  for( unsigned int i = 0; i < FEMDimension; i++ )
    {
    if( i < pt.size() )
      {
      pp[i] = pt[i];
      }
    else
      {
      pp[i] = 0.0;
      }
    }

  InterpolationGridIndexType index;

  // Return value only if given point is within the interpolation grid
  if( m_InterpolationGrid->TransformPhysicalPointToIndex(pp, index) )
    {
    return m_InterpolationGrid->GetPixel(index);
    }
  else
    {
    // Return 0, if outside the grid.
    return ITK_NULLPTR;
    }
}

} // end namespace fem
} // end namespace itk
#endif // itkFEMSolver_hxx
