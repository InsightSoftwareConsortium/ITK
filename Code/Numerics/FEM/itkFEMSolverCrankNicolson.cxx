/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMSolverCrankNicolson.cxx
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

#include "itkFEMSolverCrankNicolson.h"

#include "itkFEMLoadNode.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMLoadBC.h"
#include "itkFEMLoadBCMFC.h"

namespace itk {
namespace fem {

void SolverCrankNicolson::InitializeForSolution() 
{
  m_ls->SetSystemOrder(NGFN+NMFC);
  m_ls->SetNumberOfVectors(5);
  m_ls->SetNumberOfSolutions(2);
  m_ls->SetNumberOfMatrices(2);
  m_ls->InitializeMatrix(SumMatrixIndex);
  m_ls->InitializeMatrix(DifferenceMatrixIndex);
  m_ls->InitializeVector(ForceTIndex);
  m_ls->InitializeVector(ForceTMinus1Index);
  m_ls->InitializeVector(SolutionTMinus1Index);
  m_ls->InitializeVector(DiffMatrixBySolutionTMinus1Index);
  m_ls->InitializeSolution(SolutionTIndex);
  m_ls->InitializeSolution(TotalSolutionIndex);
}

/*
 * Assemble the master stiffness matrix (also apply the MFCs to K)
 */  
void SolverCrankNicolson::AssembleKandM() 
{

  // if no DOFs exist in a system, we have nothing to do
  if (NGFN<=0) return;

  NMFC=0;  // number of MFC in a system

  // temporary storage for pointers to LoadBCMFC objects
  typedef std::vector<LoadBCMFC::Pointer> MFCArray;
  MFCArray mfcLoads;

  /*
   * Before we can start the assembly procedure, we need to know,
   * how many boundary conditions (MFCs) there are in a system.
   */
  mfcLoads.clear();
  // search for MFC's in Loads array, because they affect the master stiffness matrix
  for(LoadArray::iterator l=load.begin(); l!=load.end(); l++) {
    if ( LoadBCMFC::Pointer l1=dynamic_cast<LoadBCMFC*>( &(*(*l))) ) {
      // store the index of an LoadBCMFC object for later
      l1->Index=NMFC;
      // store the pointer to a LoadBCMFC object for later
      mfcLoads.push_back(l1);
      // increase the number of MFC
      NMFC++;
    }
  }
 
  /*
   * Now we can assemble the master stiffness matrix
   * from element stiffness matrices
   */
  InitializeForSolution(); 
  
 
std::cout << "Begin Assembly." << std::endl;
  /*
   * Step over all elements
   */

  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++)
  {
    vnl_matrix<Float> Ke;
    (*e)->GetStiffnessMatrix(Ke);  /*Copy the element stiffness matrix for faster access. */
    vnl_matrix<Float> Me;
    (*e)->GetMassMatrix(Me);  /*Copy the element mass matrix for faster access. */
    int Ne=(*e)->GetNumberOfDegreesOfFreedom();          /*... same for element DOF */

    /* step over all rows in in element matrix */
    for(int j=0; j<Ne; j++)
    {
      /* step over all columns in in element matrix */
      for(int k=0; k<Ne; k++) 
      {
        /* error checking. all GFN should be =>0 and <NGFN */
        if ( (*e)->GetDegreeOfFreedom(j) >= NGFN ||
             (*e)->GetDegreeOfFreedom(k) >= NGFN  )
        {
          throw FEMExceptionSolution(__FILE__,__LINE__,"SolverCrankNicolson::AssembleK()","Illegal GFN!");
        }
        
        /* Here we finaly update the corresponding element
         * in the master stiffness matrix. We first check if 
         * element in Ke is zero, to prevent zeros from being 
         * allocated in sparse matrix.
         */
        if ( Ke(j,k)!=Float(0.0) || Me(j,k) != Float(0.0) )
        {
          // left hand side matrix
          Float lhsval=(Me(j,k) + m_alpha*m_deltaT*Ke(j,k));
          m_ls->AddMatrixValue( (*e)->GetDegreeOfFreedom(j) , 
                    (*e)->GetDegreeOfFreedom(k), 
                    lhsval, SumMatrixIndex );
          // right hand side matrix
          Float rhsval=(Me(j,k) - (1.-m_alpha)*m_deltaT*Ke(j,k));
          rhsval=1.0; // BUG FIXME
          m_ls->AddMatrixValue( (*e)->GetDegreeOfFreedom(j) , 
                    (*e)->GetDegreeOfFreedom(k), 
                    rhsval, DifferenceMatrixIndex );

          if (k == 0 && j == 0) std::cout << " ke " << Ke(j,k) << " me " << Me(j,k) << std::endl;
        }
      }

    }

  }

//  BUG SHOULD DO THIS ABOVE! FIXME
//  M=rho*M;

  /* step over all types of BCs */
  this->ApplyBC();  // BUG  -- are BCs applied appropriately to the problem?
  std::cout << "Done Assembling." << std::endl;
}


/*
 * Assemble the master force vector
 */
void SolverCrankNicolson::AssembleFforTimeStep(int dim) {
/* if no DOFs exist in a system, we have nothing to do */
  if (NGFN<=0) return;
 
  AssembleF(dim); // assuming assemblef uses index 0 in vector!

  typedef std::map<Element::DegreeOfFreedomIDType,Float> BCTermType;
  BCTermType bcterm;

   /* Step over all Loads */
  for(LoadArray::iterator l=load.begin(); l!=load.end(); l++)
  {
    Load::Pointer l0=*l;
    if ( LoadBC::Pointer l1=dynamic_cast<LoadBC*>(&*l0) )
    {
      bcterm[ l1->m_element->GetDegreeOfFreedom(l1->m_dof) ]=l1->m_value[dim];
    }
  } // end for LoadArray::iterator l

  // Now set the solution t_minus1 vector to fit the BCs
  for( BCTermType::iterator q=bcterm.begin(); q!=bcterm.end(); q++)
  { 
    m_ls->SetVectorValue(q->first,0.0,SolutionTMinus1Index); //FIXME? 
  }

  m_ls->MultiplyMatrixVector(DiffMatrixBySolutionTMinus1Index,
                             DifferenceMatrixIndex,SolutionTMinus1Index);
   
  for (unsigned int index=0; index<NGFN; index++) RecomputeForceVector(index);

  // Now set the solution and force vector to fit the BCs
  for( BCTermType::iterator q=bcterm.begin(); q!=bcterm.end(); q++)
  { 
    m_ls->SetVectorValue(q->first,q->second,ForceTIndex); 
  }


}




void  SolverCrankNicolson::RecomputeForceVector(unsigned int index)
{// 
  Float ft   = m_ls->GetVectorValue(index,ForceTIndex);
  Float ftm1 = m_ls->GetVectorValue(index,ForceTMinus1Index);
  Float utm1 = m_ls->GetVectorValue(index,DiffMatrixBySolutionTMinus1Index);
  m_ls->SetVectorValue(index , m_deltaT*(m_alpha*ftm1+(1.-m_alpha)*ft)+utm1 , ForceTIndex);
  m_ls->SetVectorValue(index ,ft,ForceTMinus1Index); // now set t minus one force vector correctly
}

/*
 * Solve for the displacement vector u
 */  
void SolverCrankNicolson::Solve() 
{
  std::cout << " begin solve " << std::endl;
 /* FIXME - must verify that this is correct use of wrapper */
  /* FIXME Initialize the solution vector */
  m_ls->InitializeSolution(SolutionTIndex);
  m_ls->Solve();  
// call this externally    AddToDisplacements(); 
}



/*
 * Copy solution vector u to the corresponding nodal values, which are
 * stored in node objects). This is standard post processing of the solution.
 */  
void SolverCrankNicolson::AddToDisplacements(Float optimum) 
{
  /*
   * Copy the resulting displacements from 
   * solution vector back to node objects.
   */
  Float mins=0.0, maxs=0.0;
  Float mins2=0.0, maxs2=0.0;
  for(unsigned int i=0;i<NGFN;i++)
  {  
    
    Float temp=m_ls->GetSolutionValue(i,SolutionTIndex);
    if (temp < mins2 )  mins2=temp;
    else if (temp > maxs2 )  maxs2=temp;
//  note: set rather than add - i.e. last solution of system not total solution
    m_ls->SetVectorValue(i,m_ls->GetSolutionValue(i,SolutionTIndex),SolutionTMinus1Index);    
    m_ls->AddSolutionValue(i,optimum*m_ls->GetSolutionValue(i,SolutionTIndex),TotalSolutionIndex);
    
    temp=m_ls->GetSolutionValue(i,TotalSolutionIndex);
    if (temp < mins )  mins=temp;
    else if (temp > maxs )  maxs=temp;
  }  
  
  std::cout << " min cur solution val " << mins2 << std::endl;
  std::cout << " max cur solution val " << maxs2 << std::endl;
  std::cout << " min tot solution val " << mins << std::endl;
  std::cout << " max tot solution val " << maxs << std::endl;

}

void SolverCrankNicolson::PrintDisplacements() 
{
  std::cout <<  " printing current displacements " << std::endl;
  for(unsigned int i=0;i<NGFN;i++)
  {  
    std::cout << m_ls->GetVectorValue(i,SolutionTMinus1Index) << std::endl;
  }
}

void SolverCrankNicolson::PrintForce() 
{
  std::cout <<  " printing current forces " << std::endl;
  for(unsigned int i=0;i<NGFN;i++)
  {  
    std::cout << m_ls->GetVectorValue(i,ForceTIndex) << std::endl;
  }
}




}} // end namespace itk::fem
