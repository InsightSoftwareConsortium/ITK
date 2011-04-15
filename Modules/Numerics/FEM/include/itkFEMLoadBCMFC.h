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
 * u1x is the first DOF in the element with global number 1, and u5y is the second DOF
 * in an element with GN=5.
 *
 * ... then use the following lines of code
 *        itk::LoadBCMFC m;
 *        m.lhs.push_back( LoadBCMFC::MFCTerm( elements.Find(1), 0, 0.5 ) );
 *        m.lhs.push_back( LoadBCMFC::MFCTerm( elements.Find(5), 1, 2.1 ) );
 *        m.rhs=10.0;
 * \ingroup ITK-FEM
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
   * \ingroup ITK-FEM
   */
  class MFCTerm
    {
    public:
    /**
     * Pointer to element, which holds the DOF that is affected by MFC
     */
      Element::ConstPointer m_element;

    /**
     * DOF number within the Element object
     */
      unsigned int dof;

    /**
     * Value with which this displacement is multiplied on the lhs of MFC equation
     */
      Element::Float value;

    /**
     * Constructor for easy object creation.
     */
      MFCTerm(Element::ConstPointer element_, int dof_, Element::Float value_) : m_element(element_), dof(dof_), value(value_) {}

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
  vnl_vector<Element::Float> rhs;

  /** Default constructor */
  LoadBCMFC() {}

  /**
   * With this constructor, we can easy fix the global
   * displacement dof given by node to a value val.
   *
   * \param element Pointer to an element, which holds a displacements that
   *                needs to be fixed.
   * \param dof Local DOF number in an element.
   * \param val The fixed value of a DOF.
   */
  LoadBCMFC(Element::ConstPointer element, int dof, vnl_vector<Element::Float> val);

  /** read a LoadBCMFC object from input stream. */
  virtual void Read( std::istream& f, void* info );

  /** write a LoadBCMFC object to the output stream. */
  virtual void Write( std::ostream& f ) const;

//private:  // FIXME: CrankNicolsonSolver class, which is derived from Solver class also needs access to Index.
  /** used internally by the Solver class */
  int Index;
  friend class Solver;

};

FEM_CLASS_INIT(LoadBCMFC)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadBCMFC_h
