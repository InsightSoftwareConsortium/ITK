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
 *  \brief This class generates a KdTree object with centeroid information.
 * 
 * The KdTree object stores measurment vectors in a k-d tree structure
 * that is a binary tree. The partition value is the median value of one
 * of the k dimension (partition dimension). The partition dimension is
 * determined by the spread of measurement values in each dimension. The
 * partition dimension is the dimension has the widest spread. Our
 * implementation of k-d tree doesn't have any construction or insertion
 * logic. Users should use this class or the KdTreeGenerator class.
 *
 * This class is derived from the KdTreeGenerator class. The only
 * difference between this class and the KdTreeGenerator class is that
 * the nonterminal node type of this class is
 * KdTreeWeightedCenteroidNonterminalNode and that of the
 * KdTreeGenerator is KdTreeNonterminalNode. Therefore, the public
 * interface is identical to each other. The nonterminal node generation
 * routines differ.
 *
 * To run this generator, users should provides the bucket size
 * (SetBucketSize method) and the input sample (SetSample method). The
 * Update method will run this generator. To get the resulting KdTree
 * object, call the GetOutput method.
 * 
 * \sa KdTree, KdTreeNode, KdTreeWeightedCenteroidNonterminalNode, 
 * KdTreeTerminalNode, KdTreeGenerator
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
  /** Constructor */
  WeightedCenteroidKdTreeGenerator() ;

  /** Destructor */
  virtual ~WeightedCenteroidKdTreeGenerator() {}

  void PrintSelf(std::ostream& os, Indent indent) const ;

  /** Nonterminal node generation routine */
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




