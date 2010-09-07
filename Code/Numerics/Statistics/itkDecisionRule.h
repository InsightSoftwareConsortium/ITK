/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDecisionRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDecisionRule_h
#define __itkDecisionRule_h

#include <vector>
#include "vnl/vnl_matrix.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkArray.h"
#include "itkVariableLengthVector.h"

namespace itk
{
namespace Statistics
{
/** \class DecisionRule
 *  \brief Base class that allows the setting of usage of different
 *  decision rules used in classification
 *  This class has the pure virtual function, Evaluate(). Therefore,
 *  any subclass should implement the function to be instantiated.
 */

class ITK_EXPORT DecisionRule:public Object
{
public:
  /** Standard class typedefs */
  typedef DecisionRule               Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(DecisionRule, Object);

  /** Types for the arguments that are acceptable in the Evaluate() method */
  typedef std::vector< double > MembershipVectorType;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const MembershipVectorType & discriminantScores) const = 0;

protected:
  DecisionRule();
  virtual ~DecisionRule();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  DecisionRule(const Self &);   //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};                              // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
