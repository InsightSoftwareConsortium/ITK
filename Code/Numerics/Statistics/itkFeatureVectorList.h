/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFeatureVectorList.h
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
#ifndef __itkFeatureVectorList_h
#define __itkFeatureVectorList_h

#include "itkFeatureVectorContainer.h"

#include <vector>

namespace itk{ 
  namespace Statistics{

/** \class FeatureVectorList 
 *  \brief This class is the base class for containers that have a list
 * of feature vectors
 * 
 * FeatureVectorList allows duplicates of feature vectors. It's not sorted.
 * It doesn't allow users to set frequency. The GetFrequency(...) methods
 * returns 1 if a feature vector exists, else 0.
 *
 *\sa FeatureVectorContainer, Histogram
 */

template <class TFeatureElement = float, unsigned int VFeatureDimension = 1>
class ITK_EXPORT FeatureVectorList
: public FeatureVectorContainer<TFeatureElement, VFeatureDimension>
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef FeatureVectorList  Self;

 /**
  * Standard Superclass typedef
  */
  typedef FeatureVectorContainer<TFeatureElement, VFeatureDimension> 
  Superclass;

 /** 
  * Run-time type information (and related methods).
  */
  itkTypeMacro(FeatureVectorList, FeatureVectorContainer);

  /**
   * FeatureVector typedef from superclass
   */
  typedef typename Superclass::FeatureVectorType FeatureVectorType;
 
  /**
   * InstanceIdentifier typedef from superclass.
   */
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;
  
  /**
   * VFeatureDimension template argument alias
   */
  enum { FeatureDimension = VFeatureDimension } ;

  /**
   * returns 'p'-th percentile.
   *
   * If t = p * total frequency of the dimension,
   * i = integer part of t,
   * the percentile value =  
   * (i + 1 - t) * sorted data[i] of the dimension  + 
   * (t - i) * sorted data[i + 1] of the dimension ;
   *
   * If the "forceSortedDimensionDataGeneration" flag is true, 
   * the dimension of the sorted data is differ from the "dimension",
   * argument, or sorted data is not available, it generates sorted data.
   * for the dimension
   */
  double Quantile(unsigned int dimension, double p, 
                  bool forceSortedDimensionDataGeneration = false) ;

  /**
   * To save memery space, if the sorted data is no longer needed,
   * then call release memory 
   */
  void ClearSortedDimenensionData()
  {
    m_SortedDimensionData.clear();
    m_SortedDimension = FeatureDimension ;
  }

protected:
  FeatureVectorList() ;
  virtual ~FeatureVectorList() {};
 
  /**
   * generates sorted data for the 'dimension'
   */
  void GenerateSortedDimensionData(unsigned int dimension) ;

private:
  FeatureVectorList(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  typedef std::vector< FeatureElementType > SortedDimensionData ;
  SortedDimensionData m_SortedDimensionData ;
  unsigned int m_SortedDimension ;
};

  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFeatureVectorList.txx"
#endif

#endif
