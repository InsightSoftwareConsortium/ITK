/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedCenteroidKdTreeGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeightedCenteroidKdTreeGenerator_h
#define __itkWeightedCenteroidKdTreeGenerator_h

#include <vector>

#include "itkSample.h"
#include "itkSubsample.h"
#include "itkKdTree.h"
#include "itkKdTreeGenerator.h"
#include "itkStatisticsAlgorithm.h"

namespace itk{ 
namespace Statistics{

/** \class WeightedCenteroidKdTreeGenerator 
 *  \brief WeightedCenteroidKdTreeGenerator 
 */

template < class TSample >
class ITK_EXPORT WeightedCenteroidKdTreeGenerator : 
    public KdTreeGenerator< TSample >
{
public:
  /** Standard class typedefs */
  typedef WeightedCenteroidKdTreeGenerator Self ;
  typedef KdTreeGenerator< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(WeightedCenteroidKdTreeGenerator, KdTreeGenerator);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** typedef alias for the source data container */ 
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::SubsampleType SubsampleType ;
  typedef typename Superclass::SubsamplePointer SubsamplePointer ;
  typedef typename Superclass::KdTreeType KdTreeType ;
  typedef typename Superclass::KdTreeNodeType KdTreeNodeType ;
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

protected:
  WeightedCenteroidKdTreeGenerator() ;
  virtual ~WeightedCenteroidKdTreeGenerator() {}
  void PrintSelf(std::ostream& os, Indent indent) const ;

  virtual KdTreeNodeType* GenerateNonterminalNode(int beginIndex,
                                                  int endIndex,
                                                  MeasurementVectorType 
                                                  &lowerBound,
                                                  MeasurementVectorType 
                                                  &upperBound,
                                                  int level) ;

private:
  WeightedCenteroidKdTreeGenerator(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  MeasurementVectorType m_TempLowerBound ;
  MeasurementVectorType m_TempUpperBound ;
  MeasurementVectorType m_TempMean ;
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedCenteroidKdTreeGenerator.txx"
#endif

#endif




