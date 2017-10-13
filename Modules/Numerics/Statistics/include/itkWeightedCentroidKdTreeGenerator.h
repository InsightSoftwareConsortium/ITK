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
#ifndef itkWeightedCentroidKdTreeGenerator_h
#define itkWeightedCentroidKdTreeGenerator_h

#include <vector>

#include "itkSubsample.h"
#include "itkKdTreeGenerator.h"

namespace itk
{
namespace Statistics
{
/** \class WeightedCentroidKdTreeGenerator
 *  \brief This class generates a KdTree object with centroid information.
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
 * KdTreeWeightedCentroidNonterminalNode and that of the
 * KdTreeGenerator is KdTreeNonterminalNode. Therefore, the public
 * interface is identical to each other. The nonterminal node generation
 * routines differ.
 *
 * To run this generator, users should provides the bucket size
 * (SetBucketSize method) and the input sample (SetSample method). The
 * Update method will run this generator. To get the resulting KdTree
 * object, call the GetOutput method.
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained from the sample set
 * as input. You may query this length using the function GetMeasurementVectorSize().
 *
 * \sa KdTree, KdTreeNode, KdTreeWeightedCentroidNonterminalNode,
 * KdTreeTerminalNode, KdTreeGenerator
 * \ingroup ITKStatistics
 */

template< typename TSample >
class ITK_TEMPLATE_EXPORT WeightedCentroidKdTreeGenerator:
  public KdTreeGenerator< TSample >
{
public:
  /** Standard class typedefs */
  typedef WeightedCentroidKdTreeGenerator Self;
  typedef KdTreeGenerator< TSample >      Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(WeightedCentroidKdTreeGenerator, KdTreeGenerator);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** typedef alias for the source data container */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::MeasurementType       MeasurementType;
  typedef typename Superclass::SubsampleType         SubsampleType;
  typedef typename Superclass::SubsamplePointer      SubsamplePointer;
  typedef typename Superclass::KdTreeType            KdTreeType;
  typedef typename Superclass::KdTreeNodeType        KdTreeNodeType;

protected:
  /** Constructor */
  WeightedCentroidKdTreeGenerator();

  /** Destructor */
  virtual ~WeightedCentroidKdTreeGenerator() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Nonterminal node generation routine */
  virtual KdTreeNodeType * GenerateNonterminalNode(unsigned int beginIndex,
                                                   unsigned int endIndex,
                                                   MeasurementVectorType
                                                   & lowerBound,
                                                   MeasurementVectorType
                                                   & upperBound,
                                                   unsigned int level) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WeightedCentroidKdTreeGenerator);

  MeasurementVectorType m_TempLowerBound;
  MeasurementVectorType m_TempUpperBound;
  MeasurementVectorType m_TempMean;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedCentroidKdTreeGenerator.hxx"
#endif

#endif
