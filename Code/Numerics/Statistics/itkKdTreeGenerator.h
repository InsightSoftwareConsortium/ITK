/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKdTreeGenerator_h
#define __itkKdTreeGenerator_h

#include <vector>
#include "itkMacro.h"
#include "itkPoint.h"
#include "itkSize.h"
#include "itkObject.h"

#include "itkSample.h"
#include "itkSubsample.h"
#include "itkKdTree.h"
#include "itkStatisticsAlgorithm.h"

namespace itk{ 
  namespace Statistics{

/** \class KdTreeGenerator 
 *  \brief KdTreeGenerator 
 */

template < class TSample >
class ITK_EXPORT KdTreeGenerator : public Object
{
public:
  /** Standard class typedefs */
  typedef KdTreeGenerator Self ;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(KdTreeGenerator, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** typedef alias for the source data container */ 
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TSample::MeasurementType MeasurementType ;
  
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  typedef KdTree< TSample > KdTreeType ;
  typedef KdTreeType OutputType ;
  typedef typename KdTreeType::Pointer OutputPointer ;
  typedef typename KdTreeType::KdTreeNodeType KdTreeNodeType ;

  typedef Subsample< TSample > SubsampleType ;
  typedef typename SubsampleType::Pointer SubsamplePointer ;

  void SetSample(TSample* sample) ;

  void SetBucketSize(int size) ;

  OutputPointer GetOutput()
  { return m_Tree ; }

  void GenerateData() ;

  unsigned int TotalInstance ;
protected:
  KdTreeGenerator() ;
  virtual ~KdTreeGenerator() {}
  void PrintSelf(std::ostream& os, Indent indent) const ;

  SubsamplePointer GetSubsample()
  { return m_Subsample ; }

  virtual KdTreeNodeType* GenerateNonterminalNode(int beginIndex,
                                                  int endIndex,
                                                  MeasurementVectorType 
                                                  &lowerBound,
                                                  MeasurementVectorType 
                                                  &upperBound,
                                                  int level) ;

  KdTreeNodeType* GenerateTreeLoop(int beginIndex, int endIndex, 
                                   MeasurementVectorType &lowerBound,
                                   MeasurementVectorType &upperBound,
                                   int level) ;

  void DumpVector(MeasurementVectorType &vec) ;

private:
  KdTreeGenerator(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  TSample* m_SourceSample ;
  SubsamplePointer m_Subsample ;
  int m_BucketSize ;
  OutputPointer m_Tree ;
  MeasurementVectorType m_TempLowerBound ;
  MeasurementVectorType m_TempUpperBound ;
  MeasurementVectorType m_TempMean ;
} ; // end of class

  } // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTreeGenerator.txx"
#endif

#endif




