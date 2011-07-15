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
#ifndef __itkDecisionRuleBase_h
#define __itkDecisionRuleBase_h

#include <vector>
#include "vnl/vnl_matrix.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkArray.h"
#include "itkVariableLengthVector.h"

namespace itk
{
/** \class DecisionRuleBase
 *  \brief Base class that allows the setting of usage of differnt
 *  decision rules used in classification
 *  This class has the pure virtual function, Evaluate(). Therefore,
 *  any subclass should implement the function to be instantiated.
 * \ingroup ITKStatistics
 */

class ITK_EXPORT DecisionRuleBase:public Object
{
public:
  /** Standard class typedefs */
  typedef DecisionRuleBase           Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(DecisionRuleBase, Object);

  /** Types for the arguments that are acceptable in the Evaluate() method */
  typedef std::vector< double >          VectorType;
  typedef Array< double >                ArrayType;
  typedef VariableLengthVector< double > VariableLengthVectorType;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const VectorType & discriminantScores) const = 0;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const ArrayType & discriminantScores) const = 0;

protected:
  DecisionRuleBase();
  virtual ~DecisionRuleBase();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  DecisionRuleBase(const Self &); //purposely not implemented
  void operator=(const Self &);   //purposely not implemented
};                                // end of class
} // namespace itk

#endif
