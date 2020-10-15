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
#ifndef itkKdTreeGenerator_h
#define itkKdTreeGenerator_h

#include <vector>

#include "itkKdTree.h"
#include "itkStatisticsAlgorithm.h"

namespace itk
{
namespace Statistics
{
/**
 *\class KdTreeGenerator
 *  \brief This class generates a KdTree object without centroid information.
 *
 * The KdTree object stores measurement vectors in a k-d tree structure
 * that is a binary tree. The partition value is the median value of one
 * of the k dimension (partition dimension). The partition dimension is
 * determined by the spread of measurement values in each dimension. The
 * partition dimension is the dimension has the widest spread. Our
 * implementation of k-d tree doesn't have any construction or insertion
 * logic. Users should use this class or the
 * WeightedCentroidKdTreeGenerator class.
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
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained from the sample set
 * as input. You may query this length using the function GetMeasurementVectorSize().
 *
 * \sa KdTree, KdTreeNode, KdTreeNonterminalNode, KdTreeTerminalNode,
 * WeightedCentroidKdTreeGenerator
 * \ingroup ITKStatistics
 *
 * \sphinx
 * \sphinxexample{Numerics/Statistics/SpatialSearch,Spatial Search}
 * \endsphinx
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT KdTreeGenerator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(KdTreeGenerator);

  /** Standard class type aliases */
  using Self = KdTreeGenerator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(KdTreeGenerator, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** type alias alias for the source data container */
  using MeasurementVectorType = typename TSample::MeasurementVectorType;
  using MeasurementType = typename TSample::MeasurementType;

  /** Typedef for the length of each measurement vector */
  using MeasurementVectorSizeType = unsigned int;

  /** Typedef for the k-d tree */
  using KdTreeType = KdTree<TSample>;

  /** Type alias for the k-d tree type */
  using OutputType = KdTreeType;

  /** Typedef for the smart pointer to the k-d tree */
  using OutputPointer = typename KdTreeType::Pointer;

  /** Typedef for the k-d tree node type */
  using KdTreeNodeType = typename KdTreeType::KdTreeNodeType;

  /** Typedef for the internal Subsample */
  using SubsampleType = Subsample<TSample>;

  /** Typedef for the smart pointer to the Subsample */
  using SubsamplePointer = typename SubsampleType::Pointer;

  /** Sets the input sample that provides the measurement vectors. */
  void
  SetSample(TSample * sample);

  /** Sets the number of measurement vectors that can be stored in a
   * terminal node. */
  void
  SetBucketSize(unsigned int size);

  /** Returns the pointer to the generated k-d tree. */
  OutputPointer
  GetOutput()
  {
    return m_Tree;
  }

  /** Runs this k-d tree construction algorithm. */
  void
  Update()
  {
    this->GenerateData();
  }

  /** Runs this k-d tree construction algorithm. */
  void
  GenerateData();

  /** Get macro to get the length of the measurement vectors that are being
   * held in the 'sample' that is passed to this class */
  itkGetConstMacro(MeasurementVectorSize, unsigned int);

protected:
  /** Constructor */
  KdTreeGenerator();

  /** Destructor */
  ~KdTreeGenerator() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Returns the smart pointer to the internal Subsample object. */
  SubsamplePointer
  GetSubsample()
  {
    return m_Subsample;
  }

  /** Nonterminal node generation routine */
  virtual KdTreeNodeType *
  GenerateNonterminalNode(unsigned int            beginIndex,
                          unsigned int            endIndex,
                          MeasurementVectorType & lowerBound,
                          MeasurementVectorType & upperBound,
                          unsigned int            level);

  /** Tree generation loop */
  KdTreeNodeType *
  GenerateTreeLoop(unsigned int            beginIndex,
                   unsigned int            endIndex,
                   MeasurementVectorType & lowerBound,
                   MeasurementVectorType & upperBound,
                   unsigned int            level);

private:
  /** Pointer to the input (source) sample */
  TSample * m_SourceSample;

  /** Smart pointer to the internal Subsample object. This class needs
   * a Subsample object because the partitioning process involves sorting
   * and selection. */
  SubsamplePointer m_Subsample;

  /** The number of measurement vectors that can be stored in a terminal
   * node. */
  unsigned int m_BucketSize;

  /** Pointer to the resulting k-d tree. */
  OutputPointer m_Tree;

  /** Temporary lower bound for the TreeGenerationLoop */
  MeasurementVectorType m_TempLowerBound;

  /** Temporary upper bound for the TreeGenerationLoop */
  MeasurementVectorType m_TempUpperBound;

  /** Temporary mean for the TreeGenerationLoop */
  MeasurementVectorType m_TempMean;

  /** Length of a measurement vector */
  MeasurementVectorSizeType m_MeasurementVectorSize;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkKdTreeGenerator.hxx"
#endif

#endif
