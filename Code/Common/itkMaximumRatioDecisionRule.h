/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumRatioDecisionRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MaximumRatioDecisionRule_h
#define __MaximumRatioDecisionRule_h

#include "itkWin32Header.h"

#include <vector>
#include "vnl/vnl_matrix.h"

#include "itkNumericTraits.h"
#include "itkDecisionRuleBase.h"

namespace itk {

/** \class MaximumRatioDecisionRule
 *  \brief this rule returns  \f$i\f$ if
 *   \f$\frac{f_{i}(\overrightarrow{x})}{f_{j}(\overrightarrow{x})} >
 *   \frac{K_{j}}{K_{i}}\f$ for all \f$j \not= i\f$,
 * where the \f$i\f$ is the index of a class which has 
 * membership function \f$f_{i}\f$ and its prior value 
 * (usually, the a priori probability or the size of a class) is \f$K_{i}\f$
 */
 
class ITKCommon_EXPORT MaximumRatioDecisionRule : 
    public DecisionRuleBase
{
public:
  /** Standard class typedefs */ 
  typedef MaximumRatioDecisionRule Self ;
  typedef DecisionRuleBase Superclass;
  typedef SmartPointer<Self> Pointer;
  
  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumRatioDecisionRule, DecisionRuleBase);
  
  /** Standard New() method support */
  itkNewMacro(Self) ;
  
  typedef float APrioriValueType ;
  typedef std::vector< APrioriValueType > APrioriVectorType ;

  unsigned int Evaluate(std::vector< double > &discriminantScores) ;

  void SetAPriori(APrioriVectorType& values) ;

protected:
  MaximumRatioDecisionRule() ;
  virtual ~MaximumRatioDecisionRule() {}
  
private:
  unsigned int m_NumberOfClasses ;
  vnl_matrix< double > m_APrioriRatioMatrix ;
} ; // end of class

inline unsigned int 
MaximumRatioDecisionRule::Evaluate(std::vector< double > 
                                   &discriminantScores)
{
  unsigned int i, j ;
  double temp ;

  for (i = 0 ; i < m_NumberOfClasses ; i++)
    {
    j = 0 ;
    while ( j < m_NumberOfClasses )
      {
      if ( j != i )
        {
        if ( discriminantScores[j] != 0.0 )
          {
          temp = discriminantScores[i] / discriminantScores[j] ;
          }
        else
          {
          temp = NumericTraits< double >::max() ;
          }

        if ( temp < m_APrioriRatioMatrix.get(i,j) )
          {
          break ;
          }
        }

      ++j ;

      if ( j == m_NumberOfClasses )
        {
        return i ;
        }
      }
    }

  return i ;
}

} // end of namespace
#endif







