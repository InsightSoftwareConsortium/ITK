/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumRatioDecisionRule.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMaximumRatioDecisionRule.h"

namespace itk {

MaximumRatioDecisionRule::MaximumRatioDecisionRule()
{
}


void MaximumRatioDecisionRule::SetAPriori(APrioriVectorType& values)
{
  m_NumberOfClasses = values.size() ;
  m_APrioriRatioMatrix.resize(values.size(), values.size()) ;
  unsigned int i, j ;
  double APrioriRatio ;
  for (i = 0 ; i < m_NumberOfClasses ; i++)
    {
    for (j = 0 ; j < m_NumberOfClasses ; j++)
      {
      if ( values[i] > 0 )
        {
        APrioriRatio = (double)values[j] / 
          (double)values[i] ;
        }
      else
        {
        APrioriRatio = NumericTraits< double >::max() ;
        }
        m_APrioriRatioMatrix.put(i, j, APrioriRatio) ;
      }
    }
}

} // end of namespace








