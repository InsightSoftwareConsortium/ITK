/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadBCMFC.h
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
#ifndef __itkFEMLoadBCMFC_h
#define __itkFEMLoadBCMFC_h

#include "itkFEMLoadBase.h"

namespace itk {
namespace fem {




/**
 * \class LoadBCMFC
 * \brief Generic linear multi freedom displacement constraint in global coordinate system.
 *
 * These constraints are implemented using the Lagrange multiplier method.
 * We treat displacement constraints (or essential boundary conditions, which are
 * exactly the same but with less functionality) as a special kind of load on the system.
 * 
 * How to store data in a LoadBCMFC object... Suppose you want to impose the following
 * constraint to the system:
 *
 *     0.5*u1x + 2.1*u5y = 10.0
 *
 * u1x is the first DOF in node with global number 1, and u5y is the second DOF
 * in a node with GN=5.
 *
 * ... then use the following lines of code
 *        itk::LoadBCMFC m;
 *        m.lhs.push_back( LoadBCMFC::MFCTerm( nodes.Find(1), 0, 0.5 ) );
 *        m.lhs.push_back( LoadBCMFC::MFCTerm( nodes.Find(5), 1, 2.1 ) );
 *        m.rhs=10.0;
 */

// forward declaratons...
class Solver;

class LoadBCMFC : public Load
{
FEM_CLASS(LoadBCMFC,Load)
public:

  /**
   * \class MFCTerm
   * \brief Class that holds information about one term in MFC constraint equation.
   * \sa LoadBCMFC
   */
  class MFCTerm {
  public:
    /**
     * Pointer to node which holds the DOF that is affected by MFC
     */
    Node::ConstPointer node;

    /**
     * DOF number within the Node object
     */    
    int dof;

    /**
     * Value with which this displacement is multiplied on the lhs of MFC equation
     */
    Node::Float value;

    /**
     * Constructor for easy object creation.
     */
    MFCTerm(Node::ConstPointer node_, int dof_, Node::Float value_) : node(node_), dof(dof_), value(value_) {}

  };
  
  /**
   * Left hand side of the MFC constraint equation
   */
  typedef std::vector<MFCTerm> LhsType;
  LhsType lhs;

  /**
   * Right hand side of the linear equation that defines the constraints.
   * It is a vector so that implementation of BC on isotropic elements is easy.
   * Which value is applied to the master force vector is defined by optional
   * dim parameter (defaults to 0) in AssembleF function in solver.
   */
  vnl_vector<Node::Float> rhs;

  /** Default constructor */
  LoadBCMFC() {}

  /**
   * With this constructor, we can easy fix the global
   * displacement dof given by node to a value val.
   *
   * \param node Pointer to a node which holds a displacements that needs
                 to be fixed.
   * \param dof Number of a displacement in a node.
   * \param val The fixed value of a displacement.
   */
  LoadBCMFC(Node::ConstPointer node, int dof, vnl_vector<Node::Float> val);

  /** read a LoadBCMFC object from input stream.*/
  virtual void Read( std::istream& f, void* info );

  /** write a LoadBCMFC object to the output stream*/
  virtual void Write( std::ostream& f, int ofid ) const;

private:
  /** used internally by the Solver class */
  int Index;    
  friend class Solver;

};

FEM_CLASS_INIT(LoadBCMFC)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadBCMFC_h
