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
  m_ls->SetNumberOfVectors(6);
  m_ls->SetNumberOfSolutions(3);
  m_ls->SetNumberOfMatrices(2);
  m_ls->InitializeMatrix(SumMatrixIndex);
  m_ls->InitializeMatrix(DifferenceMatrixIndex);
  m_ls->InitializeVector(ForceTIndex);
  m_ls->InitializeVector(ForceTotalIndex);
  m_ls->InitializeVector(ForceTMinus1Index);
  m_ls->InitializeVector(SolutionVectorTMinus1Index);
  m_ls->InitializeVector(DiffMatrixBySolutionTMinus1Index);
  m_ls->InitializeSolution(SolutionTIndex);
  m_ls->InitializeSolution(TotalSolutionIndex);
  m_ls->InitializeSolution(SolutionTMinus1Index);
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

    Me=Me*m_rho;

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
          m_ls->AddMatrixValue( (*e)->GetDegreeOfFreedom(j) , 
                    (*e)->GetDegreeOfFreedom(k), 
                    rhsval, DifferenceMatrixIndex );

          //if (k == 0 && j == 0) std::cout << " ke " << Ke(j,k) << " me " << Me(j,k) << std::endl;
        }
      }

    }

  }

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
    m_ls->SetVectorValue(q->first,0.0,SolutionVectorTMinus1Index); //FIXME? 
    m_ls->SetSolutionValue(q->first,0.0,SolutionTMinus1Index); //FIXME? 
  }

  m_ls->MultiplyMatrixVector(DiffMatrixBySolutionTMinus1Index,
                             DifferenceMatrixIndex,SolutionVectorTMinus1Index);
   
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
  m_ls->SetVectorValue(index , m_deltaT*(m_alpha*ft+(1.-m_alpha)*ftm1)+utm1 , ForceTIndex);
  m_ls->AddVectorValue(index , m_deltaT*(m_alpha*ft+(1.-m_alpha)*ftm1)+utm1 , ForceTotalIndex);
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


void SolverCrankNicolson::GoldenSection()
{
 // in 1-D domain, we want to find a < b < c , s.t.  f(b) < f(a) && f(b) < f(c)
 //  see Numerical Recipes
 
  Float Gold=1.618034;
  Float Glimit=100.0;
  Float Tiny=1.e-20;
  
  Float ax=0., bx=1.0, cx=1.01,fc=0.0;
  Float fa=EvaluateResidual(ax);
  Float fb=EvaluateResidual(bx);
  Float ulim=1.0,u=1.0,r=1.0,q=1.0,fu=1.0,dum=1.0;

  if ( fb > fa ) 
  {
     dum=ax; ax=bx; bx=dum;
     dum=fb; fb=fa; fa=dum;
  }

  cx=bx+Gold*(bx-ax);  // first guess for c - the 3rd pt needed to bracket the min
  fc=EvaluateResidual(cx);

  bool bDone=false;
  while (fb > fc && !bDone)
  {
    r=(bx-ax)*(fb-fc);
    q=(bx-cx)*(fb-fa);
    u=(bx)-((bx-cx)*q-(bx-ax)*r)/(2.0*GSSign(GSMax(fabs(q-r),Tiny),q-r));
    ulim=bx + Glimit*(cx-bx);
    if ((bx-u)*(u-cx) > 0.0)
    {
      fu=EvaluateResidual(u);
      if (fu < fc)
      {
        ax=bx;
        bx=u;
        fa=fb;
        fb=fu;
        bDone=true;
      } else if (fu > fb) {
        cx=u;
        fc=fu;
        bDone=true;
      }
      if (!bDone){
        u=cx+Gold*(cx-bx);
        fu=EvaluateResidual(u);
      }
    } else if ( (cx-u)*(u-ulim) > 0.0) {
      fu=EvaluateResidual(u);
      if (fu < fc)
      {
        bx=cx; cx=u; u=cx+Gold*(cx-bx);
        fb=fc; fc=fu; fu=EvaluateResidual(u);
      }

    } else if ( (u-ulim)*(ulim-cx) >= 0.0) {
      u=ulim;
      fu=EvaluateResidual(u);
    } else {
      u=cx+Gold*(cx-bx);
      fu=EvaluateResidual(u);
    }
    if (!bDone){
      ax=bx; bx=cx; cx=u;
      fa=fb; fb=fc; fc=u;
    }
  }

  // We should now have a, b and c, as well as f(a), f(b), f(c), 
  // where b gives the minimum energy position;

  Float xmin=1.0;
  if (fb!=0.0)
  {
  Float f0=1.0,f1=1.0,f2=1.0,f3=1.0,x0=1.0,x1=1.0,x2=1.0,x3=1.0,fmin=1.0;

  Float R=0.6180339;
  Float C=(1.0-R);
  Float tol=1.e-6;

  x0=ax;
  x3=cx;
  if (fabs(cx-bx) > fabs(bx-ax)){
    x1=bx;
    x2=bx+C*(bx-ax);
  } else {
    x2=bx;
    x1=bx-C*(bx-ax);
  }
  f1=EvaluateResidual(x1);
  f2=EvaluateResidual(x2);
  while (fabs(x3-x0) > tol*(fabs(x1)+fabs(x2)))
  {
    if (f2 < f1){
      x0=x1; x1=x2; x2=R*x2+C*x3;
      f0=f1; f1=f2; f2=EvaluateResidual(x2);
    } else {
      x3=x2; x2=x1; x1=R*x2+C*x0;
      f3=f2; f2=f1; f1=EvaluateResidual(x1);
    }
  }
  if (f1<f2){
    xmin=x1;
    fmin=f1;
  } else {
    xmin=x2;
    fmin=f2;
  }
  }
  for (unsigned int j=0; j<NGFN; j++)
  {
    Float SolVal;
    SolVal=xmin*m_ls->GetSolutionValue(j,SolutionTIndex)
    +(1.-xmin)*m_ls->GetSolutionValue(j,SolutionTMinus1Index);
//    SolVal=xmin*m_ls->GetSolutionValue(j,SolutionTIndex);// FOR TOT E
    m_ls->SetSolutionValue(j,SolVal,SolutionTIndex);
  }
}


Element::Float SolverCrankNicolson::EvaluateResidual(Float t)
{
 
  Float ForceEnergy=0.0;
  Float DeformationEnergy=0.0;
  Float iSolVal=0.0,jSolVal=0.0;

  for (unsigned int i=0; i<NGFN; i++)
  {
// forming  U^T F
    iSolVal=t*(m_ls->GetSolutionValue(i,SolutionTIndex))
       +(1.-t)*m_ls->GetSolutionValue(i,SolutionTMinus1Index);
    ForceEnergy+=iSolVal*m_ls->GetVectorValue(i,ForceTIndex);
//    iSolVal=t*(m_ls->GetSolutionValue(i,SolutionTIndex))
//        +m_ls->GetSolutionValue(i,TotalSolutionIndex);// FOR TOT E
//    ForceEnergy+=iSolVal*m_ls->GetVectorValue(i,ForceTotalIndex);// FOR TOT E

// forming U^T K U
    Float TempRowVal=0.0;
    for (unsigned int j=0; j<NGFN; j++)
    {
      jSolVal=t*(m_ls->GetSolutionValue(j,SolutionTIndex))
         +(1.-t)*m_ls->GetSolutionValue(j,SolutionTMinus1Index);
//        jSolVal=t*(m_ls->GetSolutionValue(j,SolutionTIndex))
//       +m_ls->GetSolutionValue(j,TotalSolutionIndex);// FOR TOT E

      TempRowVal+=m_ls->GetMatrixValue(i,j,SumMatrixIndex)*jSolVal;
    }
    DeformationEnergy+=iSolVal*TempRowVal;
  }
  return fabs(DeformationEnergy-ForceEnergy);
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
    
    Float CurrentSolution=optimum*m_ls->GetSolutionValue(i,SolutionTIndex);
      //+(1.-optimum)*m_ls->GetVectorValue(i,SolutionVectorTMinus1Index);
    if (CurrentSolution < mins2 )  mins2=CurrentSolution;
    else if (CurrentSolution > maxs2 )  maxs2=CurrentSolution;
//  note: set rather than add - i.e. last solution of system not total solution  
    m_ls->SetVectorValue(i,CurrentSolution,SolutionVectorTMinus1Index);   
    m_ls->SetSolutionValue(i,CurrentSolution,SolutionTMinus1Index);  
//    m_ls->AddVectorValue(i,CurrentSolution,SolutionVectorTMinus1Index); // FOR TOT E   
//    m_ls->AddSolutionValue(i,CurrentSolution,SolutionTMinus1Index);  // FOR TOT E 
    m_ls->AddSolutionValue(i,CurrentSolution,TotalSolutionIndex);
    CurrentSolution=m_ls->GetSolutionValue(i,TotalSolutionIndex);
    if (CurrentSolution < mins )  mins=CurrentSolution;
    else if (CurrentSolution > maxs )  maxs=CurrentSolution;
  }  
  
  std::cout << " min cur solution val " << mins2 << std::endl;
  std::cout << " max cur solution val " << maxs2 << std::endl;
  std::cout << " min tot solution val " << mins << std::endl;
  std::cout << " max tot solution val " << maxs << std::endl;

}


/*
 * Copy solution vector u to the corresponding nodal values, which are
 * stored in node objects). This is standard post processing of the solution.
 */  
void SolverCrankNicolson::AverageLastTwoDisplacements(Float t) 
{
 
  Float maxs=0.0;
  for(unsigned int i=0;i<NGFN;i++)
  {  
    Float temp=m_ls->GetSolutionValue(i,SolutionTIndex);
    Float temp2=m_ls->GetSolutionValue(i,SolutionTMinus1Index);
    Float newsol=t*(temp)+(1.-t)*temp2;
    m_ls->SetSolutionValue(i,newsol,SolutionTMinus1Index);  
    m_ls->SetVectorValue(i,newsol,SolutionVectorTMinus1Index);  
    m_ls->SetSolutionValue(i,newsol,SolutionTIndex);    
    if ( newsol > maxs )  maxs=newsol;
  }  
  
  std::cout << " max cur solution val " << maxs << std::endl;
 
}

void SolverCrankNicolson::ZeroVector(int which) 
{
  for(unsigned int i=0;i<NGFN;i++)
  {  
    m_ls->SetVectorValue(i,0.0,which);
  }
}

void SolverCrankNicolson::PrintDisplacements() 
{
  std::cout <<  " printing current displacements " << std::endl;
  for(unsigned int i=0;i<NGFN;i++)
  {  
    std::cout << m_ls->GetSolutionValue(i,TotalSolutionIndex) << std::endl;
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
