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
#ifndef itkImplicitManifoldNormalVectorFilter_h
#define itkImplicitManifoldNormalVectorFilter_h

#include "itkConstNeighborhoodIterator.h"
#include "itkNormalVectorFunctionBase.h"
#include "itkFiniteDifferenceSparseImageFilter.h"
#include "itkVector.h"

namespace itk
{
/**
 * \class ImplicitManifoldNormalVectorFilter
 *
 * \brief This class implements the filter for computing the normal vectors
 * from a scalar implicit function (i.e. a levelset image) and processing them.
 *
 * \par
 * This is a ready-to-use class for filtering normal vectors of an implicit
 * manifold image. The normal vectors of the input image are computed and
 * processed in a band where the values of the input image fall in the region
 * [IsoLevelLow, IsoLevelHigh]. The processing is done by m_NormalFunction.
 * This class also defines a Halt method which uses the iteration counter and
 * the parameter m_MaxIteration to determine when to stop the processing.
 *
 * \par INPUTS
 * The input is a scalar image. Even though this can be any scalar image, this
 * filter class is intended to work with the image of an implicit function. One
 * such example is the output of the SparseFieldLevelSetImageFilter class.
 *
 * \par OUTPUTS
 * The output is a sparse image. The m_Data member variables of the nodes of
 * the sparse image will contain the filtered output normal vectors. The sparse
 * image has valid node pointers only in the band [IsoLevelLow, IsoLevelHigh].
 * Pixels of the sparse image outside this band will be null pointers.
 *
 * \par PARAMETERS
 * IsoLevelLow and IsoLevelHigh define the working band for this filter. Pixels
 * of the input scalar image whose values fall between these low and high
 * limits will be operated on. MaxIteration determines the number of iterations
 * this filter will perform. (Default is 25)
 * The MinVectorNorm parameter determines the minimum vector norm allowed
 * (to avoid divide by 0). The default for MinVectorNorm is 10^-6 which is designed
 * to work with an input image of floats. This value can be lowered to 10^-12
 * if the input image is doubles.
 *
 * \par IMPORTANT
 * The TSparseOutputImage template parameter must be a sparse image templated
 * over a NodeType which at least has the following members: m_Data,
 * m_InputData, m_Update and m_MAnifoldNormal.
 * Depending on the Function object being used it might need other
 * members. For instance, NormalVectorDiffusionFunction will also require that
 * the NodeType has the following additional members: m_Flux.
 * \ingroup ITKLevelSets
 */
template <typename TInputImage, typename TSparseOutputImage>
class ITK_TEMPLATE_EXPORT ImplicitManifoldNormalVectorFilter
  : public FiniteDifferenceSparseImageFilter<TInputImage, TSparseOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImplicitManifoldNormalVectorFilter);

  /** Standard class type alias */
  using Self = ImplicitManifoldNormalVectorFilter;
  using Superclass = FiniteDifferenceSparseImageFilter<TInputImage, TSparseOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(ImplicitManifoldNormalVectorFilter, FiniteDifferenceSparseImageFilter);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Standard get dimension macro. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Typedefs from the superclass */
  using InputImageType = typename Superclass::InputImageType;
  using IndexType = typename Superclass::IndexType;
  using NormalVectorType = typename Superclass::NodeDataType;
  using NodeValueType = typename Superclass::NodeValueType;
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;

  using SparseOutputImageType = typename Superclass::SparseOutputImageType;
  using NormalBandNodeType = typename Superclass::OutputNodeType;
  using NodeListType = typename Superclass::NodeListType;

  /** The iterator for the input image. */
  using InputImageIteratorType = ConstNeighborhoodIterator<InputImageType>;

  /** This is the finite difference function type for processing the normal
      vectors */
  using NormalFunctionType = NormalVectorFunctionBase<SparseOutputImageType>;

  /** This is the radius type for the image neighborhoods. */
  using RadiusType = typename FiniteDifferenceFunctionType::RadiusType;

  /** This method is used to set the finite difference function. */
  void
  SetNormalFunction(NormalFunctionType * nf);

  itkSetMacro(MaxIteration, unsigned int);
  itkGetConstMacro(MaxIteration, unsigned int);
  itkSetMacro(IsoLevelLow, NodeValueType);
  itkGetConstMacro(IsoLevelLow, NodeValueType);
  itkSetMacro(IsoLevelHigh, NodeValueType);
  itkGetConstMacro(IsoLevelHigh, NodeValueType);
  itkSetMacro(MinVectorNorm, NodeValueType);
  itkGetConstMacro(MinVectorNorm, NodeValueType);
  itkSetMacro(UnsharpMaskingFlag, bool);
  itkGetConstMacro(UnsharpMaskingFlag, bool);
  itkSetMacro(UnsharpMaskingWeight, NodeValueType);
  itkGetConstMacro(UnsharpMaskingWeight, NodeValueType);

protected:
  ImplicitManifoldNormalVectorFilter();
  ~ImplicitManifoldNormalVectorFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This calls SetNormalBand to create the band of normals to process. */
  void
  Initialize() override;

  /** This function sets the band for normal vector processing. */
  void
  SetNormalBand();

  /** This function precomputes information for normal vector
   * processing. */
  void
  InitializeNormalBandNode(NormalBandNodeType * node, const InputImageIteratorType & it);

  /** This function does nothing. The output initialization
      is handled by Initialize. */
  void
  CopyInputToOutput() override
  {}

  /** This is the stopping criterion function used in the iterative finite
      difference scheme. */
  bool
  Halt() override
  {
    if (this->GetElapsedIterations() == m_MaxIteration)
    {
      return true;
    }
    else
    {
      return false;
    }
  }

protected:
  /** This function implements the unit norm constraint for normal vectors. */
  NormalVectorType
  DataConstraint(const NormalVectorType & data) const override
  {
    return (data / (m_MinVectorNorm + data.GetNorm()));
  }

  /** This function implements unsharp masking which is turned ON/OFF by the
      UnsharpMaskingFlag and controlled by the UnsharpMaskingWeight
      parameters. */
  void
  PostProcessOutput() override;

private:
  /** The finite difference function. */
  NormalFunctionType * m_NormalFunction;

  /** The number of iterations this filter will execute. */
  unsigned int m_MaxIteration;

  /** The upper and lower limits of the band of the scalar image on which we
      operate. */
  NodeValueType m_IsoLevelLow, m_IsoLevelHigh;

  /** The minimum length a vector is allowed to have to avoid divide by zero. */
  NodeValueType m_MinVectorNorm;

  /** The ON/OFF switch for unsharp masking. Default is OFF. */
  bool m_UnsharpMaskingFlag;

  /** The weight determining the extent of enhancement if unsharp masking is
      turned on. */
  NodeValueType m_UnsharpMaskingWeight;

  /** Constants used in computations. */
  unsigned long m_Indicator[Self::ImageDimension];
  unsigned int  m_NumVertex;
  NodeValueType m_DimConst;
  NodeValueType m_DimConst2;
  RadiusType    m_ManifoldRadius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImplicitManifoldNormalVectorFilter.hxx"
#endif

#endif
