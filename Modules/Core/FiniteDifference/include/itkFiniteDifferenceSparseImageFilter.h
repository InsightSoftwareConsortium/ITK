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
#ifndef itkFiniteDifferenceSparseImageFilter_h
#define itkFiniteDifferenceSparseImageFilter_h

#include "itkFiniteDifferenceSparseImageFunction.h"
#include "itkFiniteDifferenceImageFilter.h"
#include "itkMultiThreaderBase.h"
#include "itkSparseImage.h"

namespace itk
{
/**
 * \class FiniteDifferenceSparseImageFilter
 *
 * \brief This class implements a multi-threaded base class for Image to
 * SparseImage finite difference processes.
 *
 * \par
 * This class implements a multi-threading mechanism for implementing finite
 * difference PDE's on sparse image types. The sparse image is a image of
 * pointers to node variables at valid pixel locations and null pointers at
 * others. The node variable type must have the following members: m_Index,
 * m_Data and m_Update.
 *
 * \par
 * This class also adds precomputing support to the finite difference image
 * filter scheme. This support can be used by certain filters to speed up the
 * processing. The m_PrecomputeFlag should be set to true to use this and the
 * Function object must provide a PrecomputeSparseUpdate method.
 *
 * \par INPUTS
 * The input to this filter is either a regular or sparse image. Subclasses
 * should provide a way of copying this information to the output sparse image
 * or initializing the output image nodes from the input image.
 *
 * \par OUTPUTS
 * The output is a sparse image. The output will be in the m_Data members of
 * the nodes of the sparse image.
 *
 * \par IMPORTANT
 * The output sparse image type must be templated with a node type that at
 * least has the following member variables: m_Index, m_Data and m_Update.
 * \ingroup ITKFiniteDifference
 */

template <typename TInputImageType, typename TSparseOutputImageType>
class ITK_TEMPLATE_EXPORT FiniteDifferenceSparseImageFilter
  : public FiniteDifferenceImageFilter<TInputImageType, TSparseOutputImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FiniteDifferenceSparseImageFilter);

  /** Standard class type alias */
  using Self = FiniteDifferenceSparseImageFilter;
  using Superclass = FiniteDifferenceImageFilter<TInputImageType, TSparseOutputImageType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(FiniteDifferenceSparseImageFilter, FiniteDifferenceImageFilter);

  /**Typedefs from the superclass */
  using InputImageType = typename Superclass::InputImageType;
  using SparseOutputImageType = typename Superclass::OutputImageType;
  using PixelType = typename Superclass::PixelType;
  using TimeStepType = typename Superclass::TimeStepType;
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;
  // the PixelType is from output image; therefore, it is a pointer

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Typedefs from the sparse output image type. */
  using IndexType = typename SparseOutputImageType::IndexType;
  using SizeType = typename SparseOutputImageType::SizeType;
  using OutputNodeType = typename SparseOutputImageType::NodeType;
  using NodeListType = typename SparseOutputImageType::NodeListType;

  /** The type for the data variable of OutputNodeType. */
  using NodeDataType = typename OutputNodeType::NodeDataType;

  /** The basic scalar variable type used in OutputNodeType. Expected to be
   * float or double. If NodeDataType is a scalar, then this is the same type as
   * that. */
  using NodeValueType = typename OutputNodeType::NodeValueType;

  /** The sparse image finite difference function type used in this class. */
  using SparseFunctionType = FiniteDifferenceSparseImageFunction<SparseOutputImageType>;

  /** Sets the function object that will be called for computing updates. */
  void
  SetSparseFunction(SparseFunctionType * sf);

  itkSetMacro(PrecomputeFlag, bool);
  itkGetConstMacro(PrecomputeFlag, bool);

protected:
  FiniteDifferenceSparseImageFilter();
  ~FiniteDifferenceSparseImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method splits the active pixels of the sparse image into equal size
   *  lists for multi-threading. These lists remain constant throughout the
   *  operation of this filter. */
  void
  Initialize() override;

  /** This class does not use AllocateUpdateBuffer to allocate memory for its
   *  narrow band. All memory is handled through the SparseImage class. */
  void
  AllocateUpdateBuffer() override
  {}

  /** This function can be used to implements constraints on the range of data
   * values. Default is no constraint. */
  virtual NodeDataType
  DataConstraint(const NodeDataType & data) const
  {
    return data;
  }

private:
  /** The type of region used in multithreading. */
  struct ThreadRegionType
  {
    // this is the first element
    typename NodeListType::Iterator first;
    // this is one past the last element
    typename NodeListType::Iterator last;
  };

protected:
  /** This function returns a single region for use in multi-threading. */
  ThreadIdType
  GetSplitRegion(ThreadIdType i, ThreadIdType num, ThreadRegionType & splitRegion);

  /** This function updates the m_Data variable in the output image nodes using
      the update values computed by CalculateChange. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

  /** Multi-threaded implementation of ApplyUpdate. */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  ApplyUpdateThreaderCallback(void * arg);

  virtual void
  ThreadedApplyUpdate(const TimeStepType & dt, const ThreadRegionType & regionToProcess, ThreadIdType threadId);

  /** This method computes changes to the output image using the
      ComputeSparseUpdate method in the Sparse Function object. */
  TimeStepType
  CalculateChange() override;

  /** Multuthreaded implementation of CalculateChange */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  CalculateChangeThreaderCallback(void * arg);

  virtual TimeStepType
  ThreadedCalculateChange(const ThreadRegionType & regionToProcess, ThreadIdType threadId);

  /** This method provides a means of performing a first pass for computing the
   *  change and storing intermediate values that will then be used by
   *  CalculateChange. This can be used to speed up certain update rules. */
  virtual void
  PrecalculateChange();

  /** Multi-threaded implementation of PrecalculateChange */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  PrecalculateChangeThreaderCallback(void * arg);

  virtual void
  ThreadedPrecalculateChange(const ThreadRegionType & regionToProcess, ThreadIdType threadId);

  /** Structure for passing information into static callback methods.
   *  Used in  the subclasses' threading mechanisms. */
  struct FDThreadStruct
  {
    FiniteDifferenceSparseImageFilter * Filter;
    TimeStepType                        TimeStep;
    std::vector<TimeStepType>           TimeStepList;
    std::vector<bool>                   ValidTimeStepList;
  };

private:
  /** Flag to let the class know whether or not to call PrecalculateChange. */
  bool m_PrecomputeFlag;

  /** The Sparse function type. */
  SparseFunctionType * m_SparseFunction;

  /** A list of subregions of the active set of pixels in the sparse image
      which are passed to each thread for parallel processing. */
  typename NodeListType::RegionListType m_RegionList;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFiniteDifferenceSparseImageFilter.hxx"
#endif

#endif
