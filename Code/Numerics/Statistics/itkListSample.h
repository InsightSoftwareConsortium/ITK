/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSample_h
#define __itkListSample_h

#include "itkSample.h"

#include <vector>

namespace itk{ 
  namespace Statistics{

/** \class ListSample 
 *  \brief This class is the base class for containers that have a list
 * of measurement vectors
 * 
 * ListSample allows duplicates of measurement vectors. It's not sorted.
 * It doesn't allow users to set frequency. The GetFrequency(...) methods
 * returns 1 if a measurement vector exists, else 0.
 *
 *\sa Sample, Histogram
 */

template <class TMeasurement = float, unsigned int VMeasurementVectorSize = 1>
class ITK_EXPORT ListSample
: public Sample<TMeasurement, VMeasurementVectorSize>
{
public:
  /** Standard class typedef. */
  typedef ListSample  Self;
  typedef Sample<TMeasurement, VMeasurementVectorSize> Superclass;

  /** Standard macros */
  itkTypeMacro(ListSample, Sample);

  /** Superclass typedefs for Measurement vector, measurement, Instance Identifier, 
   * frequency, size, size element value */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::MeasurementType MeasurementType;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::SizeType SizeType ;
  typedef typename Superclass::SizeValueType SizeValueType ;

  /** VMeasurementVectorSize template argument alias */
  enum { MeasurementVectorSize = VMeasurementVectorSize } ;

  /** returns 'p'-th percentile.
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
   * for the dimension */
  double Quantile(unsigned int dimension, double p, 
                  bool forceSortedDimensionDataGeneration = false) ;

  /** To save memery space, if the sorted data is no longer needed,
   * then call release memory */
  void ClearSortedDimenensionData()
  {
    m_SortedDimensionData.clear();
    m_SortedDimension = MeasurementVectorSize ;
  }

protected:
  ListSample() ;
  virtual ~ListSample() {};
  void PrintSelf(std::ostream& os, Indent indent) const; 
  
  /** generates sorted data for the 'dimension' */
  void GenerateSortedDimensionData(unsigned int dimension) ;

private:
  ListSample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  typedef std::vector< MeasurementType > SortedDimensionData ;
  SortedDimensionData m_SortedDimensionData ;
  unsigned int m_SortedDimension ;
};

  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkListSample.txx"
#endif

#endif
