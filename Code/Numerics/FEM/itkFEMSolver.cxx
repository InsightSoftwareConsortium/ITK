/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMSolver.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMSolver.h"

#include "itkFEMLoadNode.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMLoadBCMFC.h"

#include "itkFEMUtility.h"
#include "itkFEMObjectFactory.h"
#include <stdexcept>

namespace itk {
namespace fem {



/**
 * Read any object from stream
 */
FEMLightObject::Pointer Solver::ReadAnyObjectFromStream(std::istream& f)
{

/** local variables */
std::streampos l;
char buf[256];
std::string s;
std::string::size_type b,e;
int clID;
FEMLightObject::Pointer a=0;

start:
  l=f.tellg();    // remember the stream position
  SkipWhiteSpace(f);      // skip comments and whitespaces
  if ( f.eof() ) return 0; // end of stream. all was good

  if ( f.get()!='<' ) goto out; // we expect a token
  f.getline(buf,256,'>');  // read up to 256 characters until '>' is reached. we read and discard the '>'
  s=std::string(buf);

  // get rid of the whitespaces in front of and the back of token
  b=s.find_first_not_of(whitespaces);                // end of whitespaces in the beginning 
  if ( (e=s.find_first_of(whitespaces,b))==std::string::npos )  // beginning of whitespaces at the end
    e=s.size();

  s=s.substr(b,e-b);
  if ( s=="END" )
  {
    /** 
     * We can ignore this token. Start again by reading the next object.
     */
    goto start;
  }
  clID=FEMObjectFactory<FEMLightObject>::ClassName2ID(s);  // obtain the class ID from FEMObjectFactory
  if (clID<0) goto out;  // class not found

  // create a new object of the correct class
  a=FEMObjectFactory<FEMLightObject>::Create(clID);
  if (!a) goto out;    // error creating new object of the derived class

  /**
   * now we have to read additional data, which is
   * specific to the class of object we just created
   */
  try {

    /**
     * Since we may have to provide addditional data to the
     * read function defined within the class, we use the
     * dynamic_cast operator to check for the object type
     * and provide the required info.
     *
     * Elements for example need pointers to array of 
     * nodes and materials.
     */

    if (dynamic_cast<Element*>(&*a))
    {
      /** Element classes... */
      Element::ReadInfoType info(&node,&mat);
      a->Read(f,&info);
    }
    else if (dynamic_cast<Load*>(&*a))
    {
      /** Load classes... */
      Load::ReadInfoType info(&node,&el);
      a->Read(f,&info);
    }
    else
    {
      /** All other classes (Nodes and Materials) require no additional info */
      a->Read(f,0);
    }

  }
  /**
   * Catch possible exceptions while 
   * reading object's data from stream
   */
  catch (...) {
    #ifndef FEM_USE_SMART_POINTERS
    delete a;  // if something went wrong, we need to destroy the already created object
    #endif
    a=0;
    throw;    // rethrow the same exception
  }

  /**
   * Return a pointer to a newly created object if all was OK
   * technically everithing should be fine here (a!=0), but we
   * check again, just in case.
   */
  if (a) { return a; }

out:

  /**
   * Something went wrong.
   * Reset the stream position to where it was before reading the object.
   */
  f.seekg(l);

  /**
   * Throw an IO exception
   */
  throw std::runtime_error("Error reading system from stream!");

}




/**
 * Reads the whole system (nodes, materials and elements) from input stream
 */
void Solver::Read(std::istream& f) {

  /** clear all arrays */
  el.clear();
  node.clear();
  mat.clear();
  load.clear();

  FEMLightObject::Pointer o=0;
  /** then we start reading objects from stream */
  do
  {
    o=ReadAnyObjectFromStream(f);
    /**
     * If ReadAnyObjectFromStream returned 0, we're ok.
     * Just continue reading... and exit the do loop.
     */
    if (!o) { continue; }

    /**
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

    /**
     * If we got here something strange was in the file...
     */

    /** first we delete the allocated object */
    #ifndef FEM_USE_SMART_POINTERS
    delete o;
    #endif
    o=0;

    /** then we throw an exception */
    throw std::runtime_error("Error reading system from stream! Unknown object!");

  } while ( o );

}




/**
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




/**
 * Assign a global freedom numbers to each DOF in a system.
 */
void Solver::GenerateGFN() {

  try
  {
    // first we have to clear the global freedom numbers (GFN) in all DOF
    for(ElementArray::iterator e=el.begin(); e!=el.end(); e++) // step over all elements
    {
      for(int j=0; j<(*e)->N(); j++) // step over all DOF in this element
      {
        (*e)->uDOF(j)->GFN=-1; // reset the GFN for all displacements
      }
    }
  }
  catch ( FEMExceptionSolution e )
  {
    FEMExceptionSolution e1(__FILE__,__LINE__,"Solver::GenerateGFN()","");
    e1.SetDescription(e.GetDescription());
    throw e1;
  }

  /**
   * Build the table that maps from DOF displacement pointer to a global DOF number
   * the table is stored within the nodes. This is required for faster building of the
   * master stiffness matrix. we also build the look up table that does the oposite i.e.
   * DOF number back to DOF displacement pointer.
   */
  GFN2Disp.clear();  // clear the lookup table
  NGFN=0;  // GFNs are zero based
  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++) {  // step over all elements
    for(int j=0; j<(*e)->N(); j++) {  // step over all DOF in this element
      if ((*e)->uDOF(j)->GFN < 0) {  // GFN was <0, we got a new global DOF
        (*e)->uDOF(j)->GFN=NGFN;  // update the value of GFN for this DOF displacement
        GFN2Disp.push_back((*e)->uDOF(j));  // we also add a new pointer to DOF in the lookup table
        NGFN++;
      }
    }
  }
  if (NGFN>0) return;  // if we got 0 DOF, somebody forgot to define the system...

}




/**
 * Assemble the master stiffness matrix (also apply the MFCs to K)
 */  
void Solver::AssembleK() {

  // if no DOFs exist in a system, we have nothing to do
  if (NGFN<=0) return;

  NMFC=0;  // number of MFC in a system

  // temporary storage for pointers to LoadBCMFC objects
  typedef std::vector<LoadBCMFC::Pointer> MFCArray;
  MFCArray mfcLoads;

  /**
   * Before we can start the assembly procedure, we need to know,
   * how many boundary conditions (MFCs) are there in a system.
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

  /**
   * Now we can assemble the master stiffness matrix
   * from element stiffness matrices
   */

  /**
   * Since we're using the Lagrange multiplier method to apply the MFC,
   * each constraint adds a new global DOF.
   */
  K.resize(NGFN+NMFC,NGFN+NMFC);
  K.fill(0.0);

  /**
   * Step over all elements
   */
  for(ElementArray::iterator e=el.begin(); e!=el.end(); e++)
  {
    vnl_matrix<Float> Ke=(*e)->Ke();  /** Copy the element stiffness matrix for faster access. */
    int Ne=(*e)->N();          /** ... same for element DOF */
    
    /** step over all rows in in element matrix */
    for(int j=0; j<Ne; j++)
    {
      /** step over all columns in in element matrix */
      for(int k=0; k<Ne; k++) 
      {
        /* error checking. all GFN should be =>0 and <NGFN */
        if ( (*e)->uDOF(j)->GFN < 0 ||
             (*e)->uDOF(j)->GFN >= NGFN ||
             (*e)->uDOF(k)->GFN < 0 ||
             (*e)->uDOF(k)->GFN >= NGFN  )
        {
          throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleK()","Illegal GFN!");
        }

        /*
         * Here we finaly update the corresponding element
         * in the master stiffness matrix.
         */
        K( (*e)->uDOF(j)->GFN , (*e)->uDOF(k)->GFN )+=Ke(j,k);

      }

    }

  }

  /**
   * When K is assembled, we add the multi freedom constraints
   * contribution to the master stiffness matrix using the lagrange multipliers.
   * Basically we only change the last couple of rows and columns in K.
   */

  /** step over all MFCs */
  for(MFCArray::iterator c=mfcLoads.begin(); c!=mfcLoads.end(); c++)
  {  
    /** step over all DOFs in MFC */
    for(LoadBCMFC::LhsType::iterator q=(*c)->lhs.begin(); q!=(*c)->lhs.end(); q++) {
      
      /** obtain the GFN of DOF that is in the MFC */
      int gfn=q->node->uDOF(q->dof)->GFN;

      /** error checking. all GFN should be =>0 and <NGFN */
      if ( gfn<0 || gfn>=NGFN )
      {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleK()","Illegal GFN!");
      }

      K(gfn, NGFN+(*c)->Index)=q->value;
      K(NGFN+(*c)->Index, gfn)=q->value;  // this is a symetric matrix...

    }
  }

}




/**
 * Assemble the master force vector
 */
void Solver::AssembleF(int dim) {

  /** if no DOFs exist in a system, we have nothing to do */
  if (NGFN<=0) return;
  
  /** Initialize the master forces vector */
  F.resize(NGFN+NMFC);  // ... we include the DOF applied by MFC
  F.fill(0.0);

  /**
   * Convert the external loads to the nodal loads and
   * add them to the master force vector F.
   */
  for(LoadArray::iterator l=load.begin(); l!=load.end(); l++) {

    /**
     * Store a temporary pointer to load object for later,
     * so that we don't have to access it via the iterator
     */
    Load::Pointer l0=*l;

    /**
     * Here we only handle Nodal loads
     */
    if ( LoadNode::Pointer l1=dynamic_cast<LoadNode*>(&*l0) ) {
      // yep, we have a nodal load

      // size of a force vector in load must match number of DOFs in node
      if ( (l1->F.size() % l1->node->N())!=0 )
      {
        throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal size of a force vector in LoadNode object!");
      }

      // we simply copy the load to the force vector
      for(int dof=0; dof < (l1->node->N()); dof++)
      {
        // error checking
        if ( l1->node->uDOF(dof)->GFN < 0 || l1->node->uDOF(dof)->GFN >= NGFN )
        {
          throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal GFN!");
        }

        /**
         * If using the extra dim parameter, we can apply the force to different isotropic dimension
         * we assume that the force vector in side the load is long enough to store all dimensions
         */
        F(l1->node->uDOF(dof)->GFN)+=l1->F[dof+l1->node->N()*dim];
      }

      // that's all there is to DOF loads, go to next load in an array
      continue;  
    }


    /**
     * Element loads...
     */
    if ( LoadElement::Pointer l1=dynamic_cast<LoadElement*>(&*l0) )
    {

      if ( !(l1->el.empty()) )
      {
        /**
         * If array of element pointers is not empty,
         * we apply the load to all elements in that array
         */
        for(LoadElement::ElementPointersVectorType::const_iterator i=l1->el.begin(); i!=l1->el.end(); i++)
        {

          const Element* el0=(*i);
          // call the Fe() function of the element that we are applying the load to.
          // we pass a pointer to the load object as a paramater.
          vnl_vector<Float> Fe = el0->Fe(Element::LoadElementPointer(l1));
          int Ne=el0->N();          // ... element's number of DOF
          for(int j=0; j<Ne; j++)    // step over all DOF
          {
            // error checking
            if ( el0->uDOF(j)->GFN < 0 || el0->uDOF(j)->GFN >= NGFN )
            {
              throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal GFN!");
            }

            // update the master force vector (take care of the correct isotropic dimensions)
            F(el0->uDOF(j)->GFN)+=Fe(j+dim*Ne);
          }
        }
      
      } else {
        
        /**
         * If the list of element pointers in load object is empty,
         * we apply the load to all elements in a system.
         */
        for(ElementArray::iterator e=el.begin(); e!=el.end(); e++) // step over all elements in a system
        {
          vnl_vector<Float> Fe=(*e)->Fe(Element::LoadElementPointer(l1));  // ... element's force vector
          int Ne=(*e)->N();          // ... element's number of DOF

          for(int j=0; j<Ne; j++)        // step over all DOF
          {
            if ( (*e)->uDOF(j)->GFN < 0 || (*e)->uDOF(j)->GFN >= NGFN )
            {
              throw FEMExceptionSolution(__FILE__,__LINE__,"Solver::AssembleF()","Illegal GFN!");
            }

            // update the master force vector (take care of the correct isotropic dimensions)
            F((*e)->uDOF(j)->GFN)+=Fe(j+dim*Ne);

          }

        }

      }

      // skip to next load in an array
      continue;
    }

    /**
     * Boundary conditions in form of MFC loads are handled next.
     */
    if ( LoadBCMFC::Pointer l1=dynamic_cast<LoadBCMFC*>(&*l0) ) {

      F(NGFN+l1->Index)=l1->rhs[dim];

      // skip to next load in an array
      continue;
    }

    /**
     * If we got here a we were unable to handle that class of Load object.
     * We do nothing...
     */


  }  // for(LoadArray::iterator l ... )


}



/**
 * decompose matrix using svd, qr, whatever ...
 */  
void Solver::DecomposeK() {


  /**
   * First we delete the existing equation solver object.
   */
  delete EQS;
  EQS=0;

  /**
   * We can only create the solver object if NGFN>0...
   */
  if (NGFN<=0)
  {
    return;
  }

  /**
   * Create the equation solver object
   */
  EQS=new vnl_svd<Float>(K);

}




/**
 * Solve for the displacement vector u
 */  
void Solver::Solve() {

  /**
   * Check if the solver object has been properly defined
   */
  if (!EQS) return;

  /**
   * Initialize the master displacement vector and solve for the displacements
   */
  u=EQS->solve(F);

}




/**
 * Copy solution vector u to the corresponding nodal values, which are
 * stored in node objects). This is standard post processing of the solution.
 */  
void Solver::UpdateDisplacements() {

  /** Check for errors */
  if((int)u.size()!=NGFN+NMFC) return;

  /**
   * Copy the resulting displacements from 
   * solution vector back to node objects.
   */
  for(int i=0;i<NGFN;i++)
    GFN2Disp[i]->value=u(i);

}




}} // end namespace itk::fem
