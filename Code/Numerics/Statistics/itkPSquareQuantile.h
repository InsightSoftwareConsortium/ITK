/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPSquareQuantile.h
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
#ifndef __itkPSquareQuantile_h
#define __itkPSquareQuantile_h

namespace itk{ 
  namespace Statistics{

/** \class PSquareQuantile
 *  \brief Raj Jain's P-Square algorithm implementation
 *
 * This algorithm is preferable for large data. 
 * Since this algorithm uses five markers and each marker needs one long and
 * three double variables, in terms of memory use, it's highly efficient. 
 * 
 * 
 * "Sequential Estimation of Quantiles"
 * (Technical Report TR-COSC 05/98)
 * by Jong-Suk R. Lee, Donald McNickle and Krzysztof Pawlikowski
 * 
 * "The P-Square Algorithm for Dynamic Calculation of Percentiles
 * and Histograms without Storing Observations"
 * Communications of the ACM, October 1985
 * by RAJ JAIN and IIMRICH CHLAMTAC
 * <http://www.cis.ohio-state.edu/~jain/papers/psqr.htm>
 */

template< class TData >
class PSquareQuantile
{
public:
  typedef TData DataContainerType ;
  typedef typename DataContainerType::Pointer DataContainerPointer ;
  typedef typename DataContainerType::FeatureElementType ValueType ;
  typedef typename DataContainerType::InstanceIdentifier InstanceIdentifier ;

  /**
   * returns 'p'-th quantile estimate
   */
  double operator() (DataContainerPointer data, long dimension, 
                     double p) ;

protected:
  /**
   * returns the 'id'-th element of the dimension
   */
  ValueType GetValue(InstanceIdentifier id) const
  {
    return m_Data->GetFeatureElement(m_Dimension, id) ;
  }
  
  /**
   * changes the 'id'-th element of the dimension to 'value'
   */
  void SetValue(InstanceIdentifier id, ValueType value) 
  {
    m_Data->SetFeatureElement(m_Dimension, id, value) ;
  }

  /**
   * returns parabolic estimate
   * The estimate is for markers that are off to the left of right
   * of its ideal location by more than one 
   */
  double ParabolicEstimate(InstanceIdentifier i, int direction) const
  {
    double estimate ;
    double p1 = double(direction) / double(n[i + 1] - n[i - 1]) ;
    double p2 = double(n[i] - n[i - 1] + direction) * double(q[i + 1] - q[i]) /
      double( n[i + 1] - n[i]) ;
    double p3 = double(n[i + 1] - n[i] - direction) * double(q[i] - q[i - 1]) /
      double(n[i] - n[i - 1]) ;

    estimate = q[i] + p1 * (p2 + p3) ;
    return estimate ;
  }

  /**
   * returns the linear estimate 
   */
  double LinearEstimate(InstanceIdentifier i, int direction)
  {
    return q[i] + direction *
      (q[i + direction] - q[i]) / (n[i + direction] - n[i]) ;
  }

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  DataContainerPointer m_Data ;
  long m_Dimension ;

  std::vector< double > q ; // marker heights
  std::vector< long > n ; // marker positions
  std::vector< double > np ; // desired marker position
  std::vector< double > dn ; // increment in desired marker position
} ;

  } // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPSquareQuantile.txx"
#endif

#endif
