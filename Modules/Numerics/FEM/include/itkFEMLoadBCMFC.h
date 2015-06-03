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
#ifndef itkFEMLoadBCMFC_h
#define itkFEMLoadBCMFC_h

#include "itkFEMLoadBase.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
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
 * \ingroup ITKFEM
 */

// forward declaratons...
// class Solver;

class ITKFEM_EXPORT LoadBCMFC : public Load
{
public:
  /** Standard class typedefs. */
  typedef LoadBCMFC                Self;
  typedef Load                     Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadBCMFC, Load);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

  /**
   * \class MFCTerm
   * \brief Class that holds information about one term in MFC constraint equation.
   * \sa LoadBCMFC
   * \ingroup ITKFEM
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
    MFCTerm(Element::ConstPointer element_, int dof_,
            Element::Float value_) : m_element(element_), dof(dof_), value(value_)
    {
    }
  };

  /**
   * Left hand side of the MFC constraint equation
   */
  typedef std::vector<MFCTerm> LhsType;

  /** Default constructor */
  LoadBCMFC() : m_Index(0), m_LeftHandSide(), m_RightHandSide()  {}

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

  /** Set the index variable for the multi freedom displacement constraint. This is used
  internally by itk::FEM::Solver*/
  void SetIndex(int ind);

  /** Get the index variable for the multi freedom displacement constraint. This is used
  internally by itk::FEM::Solver*/
  int GetIndex();

  /** Add terms to the left hand side of multi freedom displacement constraint*/
  void AddLeftHandSideTerm(LoadBCMFC::MFCTerm term);

  /** Add terms to the right hand side of multi freedom displacement
    constraint*/
  void AddRightHandSideTerm(Element::Float term);

  /** Returns the number of terms used to define the left hand side*/
  int GetNumberOfLeftHandSideTerms() const;

  /** Returns the number of terms used to define the right hand side*/
  int GetNumberOfRightHandSideTerms() const;

  /** Returns the specified left hand side term*/
  const MFCTerm GetLeftHandSideTerm(int lhs) const;

  /** Returns the number of terms used to define the right hand side*/
  Element::Float GetRightHandSideTerm(int rhs) const;

  /** Returns the array containing the left hand side boundary condition
    values*/
  const std::vector<MFCTerm> & GetLeftHandSideArray() const;
  std::vector<MFCTerm> & GetLeftHandSideArray();

  /** Returns the array containing the right hand side boundary condition
    values*/
  vnl_vector<Element::Float> & GetRightHandSideArray();

//  friend class Solver;

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  // private:  // FIXME: CrankNicolsonSolver class, which is derived from Solver
  // class also needs access to Index.
  /** used internally by the Solver class */
  int m_Index;

  LhsType m_LeftHandSide;

  /**
   * Right hand side of the linear equation that defines the constraints.
   * It is a vector so that implementation of BC on isotropic elements is easy.
   * Which value is applied to the master force vector is defined by optional
   * dim parameter (defaults to 0) in AssembleF function in solver.
   */
  vnl_vector<Element::Float> m_RightHandSide;
};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadBCMFC_h
