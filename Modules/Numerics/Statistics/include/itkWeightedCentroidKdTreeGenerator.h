/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/**
 *\class WeightedCentroidKdTreeGenerator
 *  \brief This class generates a KdTree object with centroid information.
 *
 * The KdTree object stores measurement vectors in a k-d tree structure
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

template <typename TSample>
class ITK_TEMPLATE_EXPORT WeightedCentroidKdTreeGenerator : public KdTreeGenerator<TSample>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(WeightedCentroidKdTreeGenerator);

  /** Standard class type aliases */
  using Self = WeightedCentroidKdTreeGenerator;
  using Superclass = KdTreeGenerator<TSample>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(WeightedCentroidKdTreeGenerator, KdTreeGenerator);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** type alias alias for the source data container */
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using MeasurementType = typename Superclass::MeasurementType;
  using SubsampleType = typename Superclass::SubsampleType;
  using SubsamplePointer = typename Superclass::SubsamplePointer;
  using KdTreeType = typename Superclass::KdTreeType;
  using KdTreeNodeType = typename Superclass::KdTreeNodeType;

protected:
  /** Constructor */
  WeightedCentroidKdTreeGenerator() = default;

  /** Destructor */
  ~WeightedCentroidKdTreeGenerator() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Nonterminal node generation routine */
  KdTreeNodeType *
  GenerateNonterminalNode(unsigned int            beginIndex,
                          unsigned int            endIndex,
                          MeasurementVectorType & lowerBound,
                          MeasurementVectorType & upperBound,
                          unsigned int            level) override;

private:
  MeasurementVectorType m_TempLowerBound;
  MeasurementVectorType m_TempUpperBound;
  MeasurementVectorType m_TempMean;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWeightedCentroidKdTreeGenerator.hxx"
#endif

#endif
