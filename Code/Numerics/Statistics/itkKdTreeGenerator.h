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
 *  \brief This class generates a KdTree object without centeroid information.
 * 
 * The KdTree object stores measurment vectors in a k-d tree structure
 * that is a binary tree. The partition value is the median value of one
 * of the k dimension (partition dimension). The partition dimension is
 * determined by the spread of measurement values in each dimension. The
 * partition dimension is the dimension has the widest spread. Our
 * implementation of k-d tree doesn't have any construction or insertion
 * logic. Users should use this class or the
 * WeightedCenteroidKdTreeGenerator class.
 *
 * The number of the measurement vectors in a terminal node is set by
 * the SetBucketSize method. If we use too small number for this, it
 * might cause computational overhead to calculate bound
 * conditions. However, too large number will cause more distance
 * calculation between the measurement vectors in a terminal node and
 * the query point.
 *
 * To run this generator, users should provides the bucket size
 * (SetBucketSize method) and the input sample (SetSample method). The
 * Update method will run this generator. To get the resulting KdTree
 * object, call the GetOutput method.
 * 
 * \sa KdTree, KdTreeNode, KdTreeNonterminalNode, KdTreeTerminalNode,
 * WeightedCenteroidKdTreeGenerator
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

  /** Typedef for the k-d tree */
  typedef KdTree< TSample > KdTreeType ;
  
  /** Type alias for the k-d tree type */
  typedef KdTreeType OutputType ;
  
  /** Typedef for the smart pointer to the k-d tree */
  typedef typename KdTreeType::Pointer OutputPointer ;

  /** Typedef for the k-d tree node type */
  typedef typename KdTreeType::KdTreeNodeType KdTreeNodeType ;

  /** Typedef for the internal Subsample */
  typedef Subsample< TSample > SubsampleType ;

  /** Typedef for the smart pointer to the Subsample */
  typedef typename SubsampleType::Pointer SubsamplePointer ;

  /** Sets the input sample that provides the measurement vectors. */
  void SetSample(TSample* sample) ;

  /** Sets the number of measurement vectors that can be stored in a
   * terminal node. */
  void SetBucketSize(int size) ;

  /** Returns the pointer to the generated k-d tree. */
  OutputType* GetOutput()
  { return m_Tree.GetPointer() ; }

  /** Runs this k-d tree construction algorithm. */
  void Update()
  { this->GenerateData() ; }

  /** Runs this k-d tree construction algorithm. */
  void GenerateData() ;

  /** The number of measurement vectors in an object of this class. */
  unsigned int TotalInstance ;

protected:
  /** Constructor */
  KdTreeGenerator() ;

  /** Destructor */
  virtual ~KdTreeGenerator() {}

  void PrintSelf(std::ostream& os, Indent indent) const ;

  /** Returns the smart pointer to the internal Subsample object. */
  SubsamplePointer GetSubsample()
  { return m_Subsample ; }

  /** Nonterminal node generation routine */ 
  virtual KdTreeNodeType* GenerateNonterminalNode(int beginIndex,
                                                  int endIndex,
                                                  MeasurementVectorType 
                                                  &lowerBound,
                                                  MeasurementVectorType 
                                                  &upperBound,
                                                  int level) ;

  /** Tree generation loop */
  KdTreeNodeType* GenerateTreeLoop(int beginIndex, int endIndex, 
                                   MeasurementVectorType &lowerBound,
                                   MeasurementVectorType &upperBound,
                                   int level) ;

private:
  KdTreeGenerator(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  /** Pointer to the input (source) sample */
  TSample* m_SourceSample ;

  /** Smart pointer to the internal Subsample object. This class needs
   * a Subsample object because the partitioning process involves sorting
   * and selection. */
  SubsamplePointer m_Subsample ;

  /** The number of measurement vectors that can be stored in a terminal
   * node. */
  int m_BucketSize ;
  
  /** Pointer to the resulting k-d tree. */
  OutputPointer m_Tree ;
  
  /** Temporary lower bound for the TreeGenerationLoop */
  MeasurementVectorType m_TempLowerBound ;

  /** Temporary upper bound for the TreeGenerationLoop */
  MeasurementVectorType m_TempUpperBound ;

  /** Temporary mean for the TreeGenerationLoop */
  MeasurementVectorType m_TempMean ;
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTreeGenerator.txx"
#endif

#endif




