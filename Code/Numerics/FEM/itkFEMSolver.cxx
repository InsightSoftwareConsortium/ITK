/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMSolver.cxx
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

#include "itkFEMSolver.h"

#include "itkFEMLoadNode.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMElementBase.h"
#include "itkFEMLoadBC.h"
#include "itkFEMLoadBCMFC.h"
#include "itkFEMLoadLandmark.h"

#include "itkImageRegionIterator.h"

#include <algorithm>

namespace itk {
namespace fem {




/*
 * Default constructor for Solver class
 */
Solver::Solver() : NGFN(0), NMFC(0)
{
  this->SetLinearSystemWrapper(&m_lsVNL);
}




void Solver::Clear( void )
{
  this->el.clear();
  this->node.clear();
  this->mat.clear();
  this->load.clear();

  this->NGFN=0;
  this->NMFC=0;
  this->SetLinearSystemWrapper(&m_lsVNL);
}




/*
 * Change the LinearSystemWrapper object used to solve
 * system of equations.
 */
void Solver::SetLinearSystemWrapper(LinearSystemWrapper::Pointer ls)
{ 
  m_ls=ls; // update the pointer to LinearSystemWrapper object

  this->InitializeLinearSystemWrapper();
}




void Solver::InitializeLinearSystemWrapper(void)
{ 
  // set the maximum number of matrices and vectors that
  // we will need to store inside.
  m_ls->SetNumberOfMatrices(1);
  m_ls->SetNumberOfVectors(2);
  m_ls->SetNumberOfSolutions(1);
}




/*
 * Reads the whole system (nodes, materials and elements) from input stream
 */
void Solver::Read(std::istream& f) {

  // clear all arrays
  el.clear();
  node.clear();
  mat.clear();
  load.clear();

  // Initialize the pointers to arrays in ReadInfoType object to the
  // arrays in solver object.
  ReadInfoType info(&this->node,&this->el,&this->mat);

  FEMLightObject::Pointer o;
  /* then we start reading objects from stream */
  do
  {
    o=FEMLightObject::CreateFromStream(f,&info);
    /*
     * If CreateFromStream returned 0, we're ok. That was the signal
     * for the end of stream. Just continue reading... and consequently
     * exit the do loop.
     */
    if (!o) { continue; }

    /*
     * Find out what kind of object did we read from stream
     * and store it in the appropriate array
     */
    if ( Node::Pointer o1=dynamic_cast<Node*>(&*o) )
    {
      node.push_back(FEMP<Node>(o1));
      continue;
    }
    if ( Material::Pointer o1=dynamic_cast<Material*>(&*o) )
    {
      mat.push_back(FEMP<Material>(o1));
      continue;
    }
    if ( Element::Pointer o1=dynamic_cast<Element*>(&*o) )
    {
      el.push_back(FEMP<Element>(o1));
      continue;
    }
    if ( Load::Pointer o1=dynamic_cast<Load*>(&*o) )
    {
      load.push_back(FEMP<Load>(o1));
      continue;
    }

    /*
     * If we got here, something strange was in the file...
     */

    // first we delete the allocated object
    #ifndef FEM_USE_SMART_POINTERS
    delete o;
    #endif
    o=0;

    // then we throw an exception
    throw FEMExceptionIO(__FILE__,__LINE__,"Solver::Read()","Error reading FEM problem stream!");

  } while ( o );

}




/*
 * Writes everything (nodes, materials and elements) to output stream
 */
void Solver::Write( std::ostream& f ) {

  for(NodeArray::iterator i=node.begin(); i!=node.end(); i++) {
    (*i)->Write(f);
  }
  f<<"\n<END>  % End of nodes\n\n";
  
  for(MaterialArray::iterator i=mat.begin(); i!=mat.end(); i++) {
    (*i)->Write(f);
  }
  f<<"\n<END>  % End of materials\n\n";

  for(ElementArray::iterator i=el.begin(); i!=el.end(); i++) {
    (*i)->Write(f);
  }
  f<<"\n<END>  % End of elements\n\n";

  for(LoadArray::iterator i=load.begin(); i!=load.end(); i++) {
    (*i)->Write(f);
  }
  f<<"\n<END>  % End of loads\n\n";


}




/*
 * Assign a global freedom number to each DOF in a system.
 */
void Solver::GenerateGFN() {

  // Clear the list of elements and global freedom numbers in nodes
  // FIXME: should be removed once Mesh is there
  for(NodeArray::iterator n=node.begin(); n!=node.end(); n++)
  {
   (*n)->m_elements.clear();
   (*n)->ClearDegreesOfFreedom();
  }

  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++) // step over all elements
  {

    // Add the elemens in the nodes list of elements
    // FIXME: should be removed once Mesh is there
    unsigned int Npts=(*e)->GetNumberOfNodes();
    for(unsigned int pt=0; pt<Npts; pt++)
    {
      (*e)->GetNode(pt)->m_elements.insert(*e);
    }
  }


  /*
   * Assign new ID to every DOF in a system
   */

  // Start numbering DOFs from 0
  NGFN=0;

  // Step over all elements
  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++)
  {
    // FIXME: Write a code that checks if two elements are compatible, when they share a node
    for(unsigned int n=0; n<(*e)->GetNumberOfNodes(); n++)
    {
      for(unsigned int dof=0; dof<(*e)->GetNumberOfDegreesOfFreedomPerNode(); dof++)
      {
        if( (*e)->GetNode(n)->GetDegreeOfFreedom(dof)==Element::InvalidDegreeOfFreedomID )
        {
          (*e)->GetNode(n)->SetDegreeOfFreedom(dof,NGFN);
          NGFN++;
        }
      }
    }
  } // end for e

//  NGFN=Element::GetGlobalDOFCounter()+1;
  if (NGFN>0) return;  // if we got 0 DOF, somebody forgot to define the system...

}




/*
 * Assemble the master stiffness matrix (also apply the MFCs to K)
 */  
void Solver::AssembleK()
{

  // if no DOFs exist in a system, we have nothing to do
  if (NGFN<=0) return;

  NMFC=0;  // reset number of MFC in a system

  /*
   * Before we can start the assembly procedure, we need to know,
   * how many boundary conditions if form of MFCs are there in a system.
   */

  // search for MFC's in Loads array, because they affect the master stiffness matrix
  for(LoadArray::iterator l=load.begin(); l!=load.end(); l++)
  {
    if ( LoadBCMFC::Pointer l1=dynamic_cast<LoadBCMFC*>( &(*(*l))) ) {
      // store the index of an LoadBCMFC object for later
      l1->Index=NMFC;
      // increase the number of MFC
      NMFC++;
    }
  }

  /*
   * Now we can assemble the master stiffness matrix from
   * element stiffness matrices.
   *
   * Since we're using the Lagrange multiplier method to apply the MFC,
   * each constraint adds a new global DOF.
   */
  this->InitializeMatrixForAssembly(NGFN+NMFC);

  /*
   * Step over all elements
   */
  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++)
  {
    // Call the function that actually moves the element matrix
    // to the master matrix.
    this->AssembleElementMatrix(&**e);
  }

  /*
   * Step over all the loads again to add the landmark contributions
   * to the appropriate place in the stiffness matrix
   */
  for(LoadArray::iterator l2=load.begin(); l2!=load.end(); l2++) {
    if ( LoadLandmark::Pointer l3=dynamic_cast<LoadLandmark*>( &(*(*l2))) ) {
      Element::Pointer ep = const_cast<Element*>( l3->el[0] );
      this->AssembleLandmarkContribution( ep , l3->eta );
    }
  }

  this->FinalizeMatrixAfterAssembly();

}




void Solver::InitializeMatrixForAssembly(unsigned int N)
{
  // We use LinearSystemWrapper object, to store the K matrix.
  this->m_ls->SetSystemOrder(N);
  this->m_ls->InitializeMatrix();
}


void Solver::AssembleLandmarkContribution(Element::Pointer e, float eta)
{
  // Copy the element "landmark" matrix for faster access.
  Element::MatrixType Le;
  e->GetLandmarkContributionMatrix(eta, Le);

  // ... same for number of DOF
  int Ne=e->GetNumberOfDegreesOfFreedom();

  // step over all rows in element matrix
  for(int j=0; j<Ne; j++)
  {
    // step over all columns in element matrix
    for(int k=0; k<Ne; k++) 
    {
      // error checking. all GFN should be =>0 and <NGFN
      if ( e->GetDegreeOfFreedom(j) >= NGFN ||
           e->GetDegreeOfFreedom(k) >= NGFN  )
      {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleLandmarkContribution()","Illegal GFN!");
      }

      /*
       * Here we finaly update the corresponding element
       * in the master stiffness matrix. We first check if 
       * element in Le is zero, to prevent zeros from being 
       * allocated in sparse matrix.
       */
      if ( Le[j][k]!=Float(0.0) )
      {
        this->m_ls->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Le[j][k] );
      }
    }
  }
}



void Solver::AssembleElementMatrix(Element::Pointer e)
{
  // Copy the element stiffness matrix for faster access.
  Element::MatrixType Ke;
  e->GetStiffnessMatrix(Ke);

  // ... same for number of DOF
  int Ne=e->GetNumberOfDegreesOfFreedom();

  // step over all rows in element matrix
  for(int j=0; j<Ne; j++)
  {
    // step over all columns in element matrix
    for(int k=0; k<Ne; k++) 
    {
      // error checking. all GFN should be =>0 and <NGFN
      if ( e->GetDegreeOfFreedom(j) >= NGFN ||
           e->GetDegreeOfFreedom(k) >= NGFN  )
      {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleElementMatrix()","Illegal GFN!");
      }

      /*
       * Here we finaly update the corresponding element
       * in the master stiffness matrix. We first check if 
       * element in Ke is zero, to prevent zeros from being 
       * allocated in sparse matrix.
       */
      if ( Ke[j][k]!=Float(0.0) )
      {
        this->m_ls->AddMatrixValue( e->GetDegreeOfFreedom(j), e->GetDegreeOfFreedom(k), Ke[j][k] );
      }

    }

  }

}




/*
 * Assemble the master force vector
 */
void Solver::AssembleF(int dim) {

  // Vector that stores element nodal loads
  Element::VectorType Fe; 

  // Type that stores IDs of fixed DOF together with the values to
  // which they were fixed.
  typedef std::map<Element::DegreeOfFreedomIDType,Float> BCTermType;
  BCTermType bcterm;

  /* if no DOFs exist in a system, we have nothing to do */
  if (NGFN<=0) return;
  
  /* Initialize the master force vector */
  m_ls->InitializeVector();

  /*
   * Convert the external loads to the nodal loads and
   * add them to the master force vector F.
   */
  for(LoadArray::iterator l=load.begin(); l!=load.end(); l++) {

    /*
     * Store a temporary pointer to load object for later,
     * so that we don't have to access it via the iterator
     */
    Load::Pointer l0=*l;

    /*
     * Pass the vector to the solution to the Load object.
     */
    l0->SetSolution(m_ls);

    /*
     * Here we only handle Nodal loads
     */
    if ( LoadNode::Pointer l1=dynamic_cast<LoadNode*>(&*l0) ) {
      // yep, we have a nodal load

      // size of a force vector in load must match number of DOFs in node
      if ( (l1->F.size() % l1->m_element->GetNumberOfDegreesOfFreedomPerNode())!=0 )
      {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal size of a force vector in LoadNode object!");
      }

      // we simply copy the load to the force vector
      for(unsigned int d=0; d < (l1->m_element->GetNumberOfDegreesOfFreedomPerNode()); d++)
      {
        Element::DegreeOfFreedomIDType dof=l1->m_element->GetNode(l1->m_pt)->GetDegreeOfFreedom(d);
        // error checking
        if ( dof >= NGFN )
        {
          throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal GFN!");
        }

        /*
         * If using the extra dim parameter, we can apply the force to different isotropic dimension.
         *
         * FIXME: We assume that the implementation of force vector inside the LoadNode class is correct for given
         * number of dimensions.
         */
        m_ls->AddVectorValue(dof , l1->F[d+l1->m_element->GetNumberOfDegreesOfFreedomPerNode()*dim]);
      }

      // that's all there is to DOF loads, go to next load in an array
      continue;  
    }


    /*
     * Element loads...
     */
    if ( LoadElement::Pointer l1=dynamic_cast<LoadElement*>(&*l0) )
    {

      if ( !(l1->el.empty()) )
      {
        /*
         * If array of element pointers is not empty,
         * we apply the load to all elements in that array
         */
        for(LoadElement::ElementPointersVectorType::const_iterator i=l1->el.begin(); i!=l1->el.end(); i++)
        {

          const Element* el0=(*i);
          // Call the Fe() function of the element that we are applying the load to.
          // We pass a pointer to the load object as a paramater and a reference to the nodal loads vector.
          el0->GetLoadVector(Element::LoadPointer(l1),Fe);
          unsigned int Ne=el0->GetNumberOfDegreesOfFreedom();          // ... element's number of DOF
          for(unsigned int j=0; j<Ne; j++)    // step over all DOF
          {
            // error checking
            if ( el0->GetDegreeOfFreedom(j) >= NGFN )
            {
              throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal GFN!");
            }

            // update the master force vector (take care of the correct isotropic dimensions)
            m_ls->AddVectorValue(el0->GetDegreeOfFreedom(j) , Fe(j+dim*Ne));
          }
        }
      
      } else {
        
        /*
         * If the list of element pointers in load object is empty,
         * we apply the load to all elements in a system.
         */
        for(ElementArray::iterator e=el.begin(); e!=el.end(); e++) // step over all elements in a system
        {
          (*e)->GetLoadVector(Element::LoadPointer(l1),Fe);  // ... element's force vector
          unsigned int Ne=(*e)->GetNumberOfDegreesOfFreedom();          // ... element's number of DOF

          for(unsigned int j=0; j<Ne; j++)        // step over all DOF
          {
            if ( (*e)->GetDegreeOfFreedom(j) >= NGFN )
            {
              throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal GFN!");
            }

            // update the master force vector (take care of the correct isotropic dimensions)
            m_ls->AddVectorValue((*e)->GetDegreeOfFreedom(j) , Fe(j+dim*Ne));

          }

        }

      }

      // skip to next load in an array
      continue;
    }

    /*
     * Handle boundary conditions in form of MFC loads are handled next.
     */
    if ( LoadBCMFC::Pointer l1=dynamic_cast<LoadBCMFC*>(&*l0) )
    {
      m_ls->SetVectorValue(NGFN+l1->Index , l1->rhs[dim]);

      // skip to next load in an array
      continue;
    }

    /*
     * Handle essential boundary conditions.
     */
    if ( LoadBC::Pointer l1=dynamic_cast<LoadBC*>(&*l0) )
    {

      // Here we just store the values of fixed DOFs. We can't set it here, because
      // it may be changed by other loads that are applied later.
      bcterm[ l1->m_element->GetDegreeOfFreedom(l1->m_dof) ]=l1->m_value[dim];

       // skip to the next load in an array
      continue;
    }

    /*
     * If we got here, we were unable to handle that class of Load object.
     * We do nothing...
     */


  }  // for(LoadArray::iterator l ... )

  /*
   * Adjust the master force vector for essential boundary
   * conditions as required.
   */
  if ( m_ls->IsVectorInitialized(1) )
  {
    // Add the vector generated by ApplyBC to the solution vector
    const unsigned int totGFN=NGFN+NMFC;
    for( unsigned int i=0; i<totGFN; i++ )
    {
      m_ls->AddVectorValue(i,m_ls->GetVectorValue(i,1));
    }

  }

  // Set the fixed DOFs to proper values
  for( BCTermType::iterator q=bcterm.begin(); q!=bcterm.end(); q++)
  {
    m_ls->SetVectorValue(q->first,q->second);
  }


}



/*
 * Decompose matrix using svd, qr, whatever ... if needed
 */  
void Solver::DecomposeK()
{
}




/*
 * Solve for the displacement vector u
 */  
void Solver::Solve()
{
  // Check if master stiffness matrix and master force vector were
  // properly initialized.
  if(!m_ls->IsMatrixInitialized())
  {
    throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::Solve()","Master stiffness matrix was not initialized!");
  }
  if(!m_ls->IsVectorInitialized())
  {
    throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::Solve()","Master force vector was not initialized!");
  }

  // Solve the system of linear equations
  m_ls->InitializeSolution();
  m_ls->Solve();
}




/*
 * Copy solution vector u to the corresponding nodal values, which are
 * stored in node objects). This is standard post processing of the solution.
 */  
void Solver::UpdateDisplacements()
{

}


Solver::Float Solver::GetDeformationEnergy(unsigned int SolutionIndex)
{
  float U=0.0;
  Element::MatrixType LocalSolution;

  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++)
  {
    unsigned int Ne=(*e)->GetNumberOfDegreesOfFreedom();
    LocalSolution.resize(Ne,1);
    // step over all DOFs of element
    for(unsigned int j=0; j<Ne; j++)
    {
      LocalSolution[j][0]=m_ls->GetSolutionValue((*e)->GetDegreeOfFreedom(j),SolutionIndex);
    }

    U+=(*e)->GetElementDeformationEnergy(LocalSolution);
  }
  return U;
}

/*
 * Apply the boundary conditions to the system.
 */
void Solver::ApplyBC(int dim, unsigned int matrix)
{

  // Vector with index 1 is used to store force correctios for BCs
  m_ls->DestroyVector(1);

  /* Step over all Loads */
  for(LoadArray::iterator l=load.begin(); l!=load.end(); l++)
  {

    /*
     * Store a temporary pointer to load object for later,
     * so that we don't have to access it via the iterator
     */
    Load::Pointer l0=*l;


    /*
     * Apply boundary conditions in form of MFC loads.
     *
     * We add the multi freedom constraints contribution to the master
     * stiffness matrix using the lagrange multipliers. Basically we only
     * change the last couple of rows and columns in K.
     */
    if ( LoadBCMFC::Pointer c=dynamic_cast<LoadBCMFC*>(&*l0) )
    {
      /* step over all DOFs in MFC */
      for(LoadBCMFC::LhsType::iterator q=c->lhs.begin(); q!=c->lhs.end(); q++) {
      
        /* obtain the GFN of DOF that is in the MFC */
        Element::DegreeOfFreedomIDType gfn=q->m_element->GetDegreeOfFreedom(q->dof);

        /* error checking. all GFN should be =>0 and <NGFN */
        if ( gfn>=NGFN )
        {
          throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::ApplyBC()","Illegal GFN!");
        }

        /* set the proper values in matster stiffnes matrix */
        this->m_ls->SetMatrixValue(gfn, NGFN+c->Index, q->value, matrix);
        this->m_ls->SetMatrixValue(NGFN+c->Index, gfn, q->value, matrix);  // this is a symetric matrix...

      }

      // skip to next load in an array
      continue;
    }



    /*
     * Apply essential boundary conditions
     */
    if ( LoadBC::Pointer c=dynamic_cast<LoadBC*>(&*l0) )
    {

      Element::DegreeOfFreedomIDType fdof = c->m_element->GetDegreeOfFreedom(c->m_dof);
      Float fixedvalue=c->m_value[dim];


      // Copy the corresponding row of the matrix to the vector that will
      // be later added to the master force vector.
      // NOTE: We need to copy the whole row first, and then clear it. This
      //       is much more efficient when using sparse matrix storage, than
      //       copying and clearing in one loop.

      // Get the column indices of the nonzero elements in an array.
      LinearSystemWrapper::ColumnArray cols;
      m_ls->GetColumnsOfNonZeroMatrixElementsInRow(fdof, cols, matrix);

      // Force vector needs updating only if DOF was not fixed to 0.0.
      if( fixedvalue!=0.0 )
      {
        // Initialize the master force correction vector as required
        if ( !this->m_ls->IsVectorInitialized(1) )
        {
          this->m_ls->InitializeVector(1);
        }

        // Step over each nonzero matrix element in a row
        for(LinearSystemWrapper::ColumnArray::iterator c=cols.begin(); c!=cols.end(); c++)
        {
          // Get value from the stiffness matrix
          Float d=this->m_ls->GetMatrixValue(fdof, *c, matrix);

          // Store the appropriate value in bc correction vector (-K12*u2)
          //
          // See http://titan.colorado.edu/courses.d/IFEM.d/IFEM.Ch04.d/IFEM.Ch04.pdf
          // chapter 4.1.3 (Matrix Forms of DBC Application Methods) for more info.
          this->m_ls->AddVectorValue(*c,-d*fixedvalue,1);
        }
      }


      // Clear that row and column in master matrix
      for(LinearSystemWrapper::ColumnArray::iterator c=cols.begin(); c!=cols.end(); c++)
      {
        this->m_ls->SetMatrixValue(fdof,*c, 0.0, matrix);
        this->m_ls->SetMatrixValue(*c,fdof, 0.0, matrix); // this is a symetric matrix
      }
      this->m_ls->SetMatrixValue(fdof,fdof, 1.0, matrix); // Set the diagonal element to one


      // skip to next load in an array
      continue;

    }


  } // end for LoadArray::iterator l


}




/*
 * Initialize the interpolation grid
 */
void Solver::InitializeInterpolationGrid(const VectorType& size, const VectorType& bb1, const VectorType& bb2)
{
  // Discard any old image object an create a new one
  m_InterpolationGrid=InterpolationGridType::New();

  // Set the interpolation grid (image) size, origin and spacing
  // from the given vectors, so that physical point of v1 is (0,0,0) and
  // phisical point v2 is (size[0],size[1],size[2]).
  InterpolationGridType::SizeType image_size={{1,1,1}};
  for(unsigned int i=0;i<size.size();i++) 
  {
    image_size[i] = static_cast<InterpolationGridType::SizeType::SizeValueType>( size[i] );
  }
  Float image_origin[MaxGridDimensions]={0.0,0.0,0.0};
  for(unsigned int i=0;i<size.size();i++)
  {
    image_origin[i]=bb1[i];
  }
  Float image_spacing[MaxGridDimensions]={1.0,1.0,1.0};
  for(unsigned int i=0;i<size.size();i++) 
  {
    image_spacing[i]=(bb2[i]-bb1[i])/(image_size[i]-1);
  }

  // All regions are the same
  m_InterpolationGrid->SetRegions(image_size);
  m_InterpolationGrid->Allocate();

  // Set origin and spacing
  m_InterpolationGrid->SetOrigin(image_origin);
  m_InterpolationGrid->SetSpacing(image_spacing);

  // Initialize all pointers in interpolation grid image to 0
  m_InterpolationGrid->FillBuffer(0);

  VectorType v1,v2;

  // Fill the interpolation grid with proper pointers to elements
  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++)
  {
    // Get square boundary box of an element
    v1=(*e)->GetNodeCoordinates(0); // lower left corner
    v2=v1; // upper right corner

    const unsigned int NumberOfDimensions=(*e)->GetNumberOfSpatialDimensions();

    for(unsigned int n=1; n < (*e)->GetNumberOfNodes(); n++ )
    {
      const VectorType& v=(*e)->GetNodeCoordinates(n);
      for(unsigned int d=0; d < NumberOfDimensions; d++ )
      {
        if( v[d] < v1[d] )
        {
          v1[d]=v[d];
        }
        if( v[d] > v2[d] ) 
        {
          v2[d]=v[d];
        }
      }
    }

    // Convert boundary box corner points into discrete image indexes.
    InterpolationGridType::IndexType vi1,vi2;
    
    Point<Float,MaxGridDimensions> vp1,vp2,pt;
    for(unsigned int i=0;i<MaxGridDimensions;i++)
    {
      if ( i < NumberOfDimensions ) 
      {
        vp1[i]=v1[i];
        vp2[i]=v2[i];
      }
      else 
      {
        vp1[i]=0.0;
        vp2[i]=0.0;
      }
    }

    // Obtain the Index of BB corner and check whether it is within image.
    // If it is not, we ignore the entire element.
    if(!m_InterpolationGrid->TransformPhysicalPointToIndex(vp1,vi1)) continue;
    if(!m_InterpolationGrid->TransformPhysicalPointToIndex(vp2,vi2)) continue;

    InterpolationGridType::SizeType region_size;
    for( unsigned int i=0; i<MaxGridDimensions; i++ ) 
    {
      region_size[i] = vi2[i]-vi1[i]+1;
    }
    InterpolationGridType::RegionType region(vi1,region_size);

    // Initialize the iterator that will step over all grid points within
    // element boundary box.
    ImageRegionIterator<InterpolationGridType> iter(m_InterpolationGrid,region);




    //
    // Update the element pointers in the points defined within the region.
    //
    VectorType global_point(NumberOfDimensions); // Point in the image as a vector.
    VectorType local_point; // Same point in local element coordinate system

    // Step over all points within the region
    for(iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
      // Note: Iteratior is guarantied to be within image, since the
      //       elements with BB outside are skipped before.
      m_InterpolationGrid->TransformIndexToPhysicalPoint(iter.GetIndex(),pt);
      for(unsigned int d=0; d<NumberOfDimensions; d++)
      {
        global_point[d]=pt[d];
      }

      // If the point is within the element, we update the pointer at
      // this point in the interpolation grid image.
      if( (*e)->GetLocalFromGlobalCoordinates(global_point,local_point) )
      {
        iter.Set(*e);
      }
    } // next point in region


  } // next element

}



const Element *
Solver::GetElementAtPoint(const VectorType& pt) const
{
  // Add zeros to the end of physical point if necesarry
  Point<Float,MaxGridDimensions> pp;
  for(unsigned int i=0;i<MaxGridDimensions;i++)
  {
    if ( i < pt.size() ) 
    {
      pp[i]=pt[i]; 
    }
    else 
    {
      pp[i]=0.0;
    }
  }

  InterpolationGridType::IndexType index;

  // Return value only if given point is within the interpolation grid
  if( m_InterpolationGrid->TransformPhysicalPointToIndex(pp,index) )
  {
    //itk::ContinuousIndex<Float,MaxGridDimensions> ci;
    //m_InterpolationGrid->TransformPhysicalPointToContinuousIndex(pp,ci);
    //std::cout<<"In:"<<index<<", ci="<<ci<<", c="<<pp<<"\n";
    return m_InterpolationGrid->GetPixel(index);
  }
  else
  {
    // Return 0, if outside the grid.
    return 0;
  }
}


}} // end namespace itk::fem
