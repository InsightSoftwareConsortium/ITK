/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFEMLoadBase_h
#define __itkFEMLoadBase_h

#include "itkFEMElementBase.h"
#include "itkFEMSolution.h"
#include "itkFEMPArray.h"

namespace itk {
namespace fem {




/**
 * \class Load
 * \brief General abstract load base class.
 *
 * All other load classes that can be used in a FEM system are defined by deriving this one.
 * The load class defines an external load that acts on the system. For each specific subtype 
 * of load, a separate load abstract class should be derived. For example we have LoadElement,
 * which defines the base for all loads that act on a specific element in a system.
 */
class Load : public FEMLightObject
{
FEM_ABSTRACT_CLASS(Load,FEMLightObject)
public:

  /** Array class that holds special pointers to the load objects */
  typedef FEMPArray<Self> ArrayType;

  /**
   * Sets the pointer to solution vector. This function is automatically
   * called by the Solver class on every load object.
   *
   * Some types of external Loads may need access to previous values of
   * solution vector. If a derived class needs that, it should implement
   * this function, and store the passed pointer accordingly. If the result
   * vector is not required, the functionn should be left unimplemented,
   * so that only the dummy implementation in base class is called.
   *
   * \param ptr Pointer to the object of Solution class.
   */
  virtual void SetSolution(Solution::ConstPointer) {}
  virtual Solution::ConstPointer GetSolution( ) { return 0;}

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadBase_h
