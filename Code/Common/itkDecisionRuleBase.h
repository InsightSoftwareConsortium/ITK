/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDecisionRuleBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDecisionRuleBase_h
#define __itkDecisionRuleBase_h

#include <vector>
#include "vnl/vnl_matrix.h"
#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** \class DecisionRuleBase
 *  \brief Base class that allows the setting of usage of differnt
 *  decision rules used in classification
 *  This class has the pure virtual function, Evaluate(). Therefore,
 *  any subclass should implement the function to be instantiated.
 */
 
class ITKCommon_EXPORT DecisionRuleBase : public Object
{
public:
  /** Standard class typedefs */ 
  typedef DecisionRuleBase Self ;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Run-time type information (and related methods) */
  itkTypeMacro(DecisionRuleBase, Object);
  
  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(std::vector< double >
                                &discriminantScores) = 0 ;

protected:
  DecisionRuleBase();
  virtual ~DecisionRuleBase();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  
} ; // end of class

} // namespace itk

#endif







