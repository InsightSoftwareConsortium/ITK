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
#ifndef itkSparseFieldFourthOrderLevelSetImageFilter_h
#define itkSparseFieldFourthOrderLevelSetImageFilter_h

#include "itkNormalVectorDiffusionFunction.h"
#include "itkImplicitManifoldNormalVectorFilter.h"
#include "itkLevelSetFunctionWithRefitTerm.h"
#include "itkSparseFieldLevelSetImageFilter.h"
#include <cmath>

namespace itk
{
/**
 * \class NormalBandNode
 *
 * \brief This is a data storage class that can is used as the node type for
 * the SparseImage class.
 *
 * \ingroup ITKLevelSets
 */
template <typename TImageType>
class ITK_TEMPLATE_EXPORT NormalBandNode
{
public:
  /** The scalar image type. */
  using LevelSetImageType = TImageType;

  /** The pixel type of the scalar image. Expected to be float or double. */
  using NodeValueType = typename LevelSetImageType::PixelType;

  /** The index type for the scalar image. */
  using IndexType = typename LevelSetImageType::IndexType;

  /** The definition for the normal vector type of the scalar image. */
  using NodeDataType = Vector<NodeValueType, TImageType::ImageDimension>;

  /** Container for output data (normal vectors). */
  NodeDataType m_Data;

  /** Container for a copy of normal vectors before processing. */
  NodeDataType m_InputData;

  /** Container for storing update vectors. */
  NodeDataType m_Update;

  /** Container for the manifold normal vector. These are computed once at
      initialization and later used for computing intrinsic derivatives. */
  NodeDataType m_ManifoldNormal[TImageType::ImageDimension];

  /** Intermediate flux computations used in computing the update. */
  NodeDataType m_Flux[TImageType::ImageDimension];

  /** Curvature computed from the output normal vectors. Used by
      LevelSetFunctionWithRefitTerm for its propagation term. */
  NodeValueType m_Curvature;

  /** This flag is true if the curvature value at this node is valid, false
      otherwise. */
  bool m_CurvatureFlag;

  /** The position of this node in the sparse image. */
  IndexType m_Index;

  /** Pointers to previous and next nodes in the list. */
  NormalBandNode * Next;
  NormalBandNode * Previous;
};

/**
 * \class SparseFieldFourthOrderLevelSetImageFilter
 *
 * \brief This class implements the fourth order level set PDE framework.
 *
 * \par
 * This class adds a ProcessNormals method to SparseFieldLevelSetImageFilter
 * class. The ProcessNormals method uses the
 * ImplicitManifoldNormalDiffusionFilter class to generate a SparseImage of
 * filtered normal vectors. We make a copy of the current state of the output
 * image (also referred to as level set image) for this class and pass it to
 * ImplicitManifoldNormalDiffusionFilter. That class computes the normal
 * vectors to the level set image and filters them. The output is in the form
 * of a sparse image templated with the NormalBandNode type. We then compute
 * curvatures from that output and store them in the SparseImage as well. This
 * SparseImage is passed onto the LevelSetFunctionWithRefitTerm filter class to
 * be used as a target in the propagation term.
 *
 * \par INPUT and OUTPUT
 * Same as SparseFieldLevelSetImageFilter
 *
 * \par PARAMETERS
 * MaxRefitIteration sets the maximum number of allowable iterations between
 * calls to ProcessNormals. The decision of when to call the ProcessNormals
 * method is made in InitializeIteration according to a few criteria one of
 * which is this maximum number of iterations.
 *
 * \par
 * MaxNormalIteration sets the maximum number of diffusion iterations on the
 * normals to be performed by the ImplicitManifoldNormalDiffusionFilter
 * class. Please read the documentation for that class.
 *
 * \par
 * CurvatureBandWidth determines the width of the band to be processed in
 * ImplicitManifoldNormalDiffusionFilter.
 *
 * \par
 * RMSChangeNormalProcessTrigger provides another mechanism in
 * InitializeIteration for calling the ProcessNormals method. Whenever the RMS
 * change reported by SparseFieldLevelSetImageFilter falls below this parameter
 * ProcessNormals is called regardless of whether MaxRefitIteration has been
 * reached. This parameter could be used to speed up the algorithm; however, it
 * can also effect the results. Use with caution. Default is 0 which does
 * nothing.
 *
 * \par IMPORTANT
 * Defaults for above parameters are set in the
 * constructor. Users should not change these unless they have a good
 * understanding of the algorithm.
 *
 * \par OTHER PARAMETERS
 * NormalProcessType tells ImplicitManifoldNormalVectorFilter whether to use
 * isotropic or anisotropic diffusion. A value of 0 means isotropic whereas a
 * value of 1 means anisotropic diffusion. If this parameter is set to 1,
 * NormalProcessConductance determines the level of detail preservation. Please
 * read the documentation for ImplicitManifoldNormalVectorFilter and
 * AnisotropicFourthOrderLevelSetImageFilter.
 *
 * \par
 * NormalProcessUnsharpFlag turns unsharp masking on/off. If this parameter is
 * turned on, then NormalProcessUnsharpWeight should be set. Please read the
 * documentation for ImplicitManifoldNormalVectorFilter.
 *
 * \par IMPORTANT
 * Users of this class must define the Halt function.
 * \ingroup ITKLevelSets
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT SparseFieldFourthOrderLevelSetImageFilter
  : public SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SparseFieldFourthOrderLevelSetImageFilter);

  /** Standard class type aliases */
  using Self = SparseFieldFourthOrderLevelSetImageFilter;
  using Superclass = SparseFieldLevelSetImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(SparseFieldFourthOrderLevelSetImageFilter, SparseFieldLevelSetImageFilter);

  /** Standard image dimension macro. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Typedefs derived from the superclass. */
  using OutputImageType = typename Superclass::OutputImageType;
  using ValueType = typename Superclass::ValueType;
  using IndexType = typename Superclass::IndexType;
  using LayerType = typename Superclass::LayerType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodScalesType = typename Superclass::NeighborhoodScalesType;

  /** The storage class used as the node type for the sparse normal vector
      image. */
  using NodeType = NormalBandNode<OutputImageType>;

  /** The sparse image type used for processing the normal vectors. */
  using SparseImageType = SparseImage<NodeType, Self::ImageDimension>;

  /** The normal vector type. */
  using NormalVectorType = typename NodeType::NodeDataType;

  /** The iterator type for the sparse image. */
  using SparseImageIteratorType = NeighborhoodIterator<SparseImageType>;

  /** The filter type for processing the normal vectors of the level set. */
  using NormalVectorFilterType = ImplicitManifoldNormalVectorFilter<OutputImageType, SparseImageType>;

  /** The function type for processing the normal vector neighborhood. */
  using NormalVectorFunctionType = NormalVectorDiffusionFunction<SparseImageType>;

  /** The radius type derived from the normal vector function. */
  // using RadiusType = typename NormalVectorFunctionType::RadiusType;

  /** The level set function with refitting term type. */
  using LevelSetFunctionType = LevelSetFunctionWithRefitTerm<OutputImageType, SparseImageType>;

  itkGetConstReferenceMacro(MaxRefitIteration, unsigned int);
  itkSetMacro(MaxRefitIteration, unsigned int);
  itkGetConstReferenceMacro(MaxNormalIteration, unsigned int);
  itkSetMacro(MaxNormalIteration, unsigned int);
  itkGetConstReferenceMacro(CurvatureBandWidth, ValueType);
  itkSetMacro(CurvatureBandWidth, ValueType);
  itkGetConstReferenceMacro(RMSChangeNormalProcessTrigger, ValueType);
  itkSetMacro(RMSChangeNormalProcessTrigger, ValueType);
  itkGetConstReferenceMacro(NormalProcessType, int);
  itkSetMacro(NormalProcessType, int);
  itkGetConstReferenceMacro(NormalProcessConductance, ValueType);
  itkSetMacro(NormalProcessConductance, ValueType);
  itkSetMacro(NormalProcessUnsharpFlag, bool);
  itkGetConstReferenceMacro(NormalProcessUnsharpFlag, bool);
  itkSetMacro(NormalProcessUnsharpWeight, ValueType);
  itkGetConstReferenceMacro(NormalProcessUnsharpWeight, ValueType);

  /** Set the level set function. Must LevelSetFunctionWithRefitTerm or a
      subclass. */
  void
  SetLevelSetFunction(LevelSetFunctionType * lsf);

  /** Compute the number of layers that must be used in
      SparseFieldLevelSetImageFilter to accommodate the desired normal
      processing band. */
  unsigned int
  GetMinimumNumberOfLayers() const
  {
    return (int)std::ceil(m_CurvatureBandWidth + Self::ImageDimension);
  }

  /** This overrides SparseFieldLevelSetImageFilter's SetNumberOfLayers to make
      sure we have enough layers to do what we need. */
  void
  SetNumberOfLayers(const unsigned int n) override
  {
    unsigned int nm = std::max(this->GetMinimumNumberOfLayers(), n);

    if (nm != this->GetNumberOfLayers())
    {
      Superclass::SetNumberOfLayers(nm);
      this->Modified();
    }
  }

  /** This method first calls the Superclass InitializeIteration method. Then
      it determines whether ProcessNormals should be called. */
  void
  InitializeIteration() override
  {
    Superclass::InitializeIteration();
    ValueType rmschange = this->GetRMSChange();

    if ((this->GetElapsedIterations() == 0) || (m_RefitIteration == m_MaxRefitIteration) ||
        (rmschange <= m_RMSChangeNormalProcessTrigger) || (this->ActiveLayerCheckBand()))
    {
      if ((this->GetElapsedIterations() != 0) && (rmschange <= m_RMSChangeNormalProcessTrigger) &&
          (m_RefitIteration <= 1))
      {
        m_ConvergenceFlag = true;
      }

      m_RefitIteration = 0;
      ProcessNormals();
    }

    m_RefitIteration++;
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<ValueType>));
  // End concept checking
#endif

protected:
  SparseFieldFourthOrderLevelSetImageFilter();
  ~SparseFieldFourthOrderLevelSetImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method computes curvature from normal vectors stored in a sparse
      image neighborhood. */
  ValueType
  ComputeCurvatureFromSparseImageNeighborhood(SparseImageIteratorType & it) const;

  /** This method computes curvature from the processed normal vectors over
   *  the region specified by the CurvatureBandWidth parameter. The
   *  curvatures are stored in the sparse image. */
  void
  ComputeCurvatureTarget(const OutputImageType * distanceImage, SparseImageType * sparseImage) const;

  /** The method for processing the normal vectors. */
  void
  ProcessNormals();

  /** This method checks whether the level set front is touching the edges of
   * the band where curvature from the processed normal vectors has been
   * computed. This is one of the conditions for triggering the ProcessNormals
   * method. */
  bool
  ActiveLayerCheckBand() const;

private:
  /** This is a iteration counter that gets reset to 0 every time
      ProcessNormals method is called. */
  unsigned int m_RefitIteration;

  /** This parameter determines the maximum number of
      SparseFieldLevelSetImageFilter iterations that will be executed between
      calls to ProcessNormals. */
  unsigned int m_MaxRefitIteration;

  /** This parameter is used to set the corresponding parameter in
      ImplicitManifoldNormalDiffusionfFilter. */
  unsigned int m_MaxNormalIteration;

  /** This is used to trigger a call to the ProcessNormals method
      before m_RefitIteration reaches m_MaxRefitIteration if the RMSChange falls
      below this parameter. */
  ValueType m_RMSChangeNormalProcessTrigger;

  /** This flag is set to true to signal final convergence. It can be used by
      subclasses that define a Halt method. */
  bool m_ConvergenceFlag;

  /** The level set function with the term for refitting the level set to the
      processed normal vectors. */
  LevelSetFunctionType * m_LevelSetFunction;

  /** This parameter determines the width of the band where we compute
   * curvature from the processed normals. The wider the band, the more level set
   * iterations that can be performed between calls to ProcessNormals. It is
   * qsuggested that this value is left at its default. */
  ValueType m_CurvatureBandWidth;

  /** The parameter that chooses between isotropic/anisotropic filtering of the
      normal vectors. */
  int m_NormalProcessType;

  /** The conductance parameter used if anisotropic filtering of the normal
      vectors is chosen. */
  ValueType m_NormalProcessConductance;

  /** The parameter that turns on/off the unsharp mask enhancement of the
      normals. */
  bool m_NormalProcessUnsharpFlag;

  /** The weight that controls the extent of enhancement if the
      NormalProcessUnsharpFlag is ON. */
  ValueType m_NormalProcessUnsharpWeight;

  /** Constants used in the computations. */
  static const SizeValueType m_NumVertex;
  static const ValueType     m_DimConst;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSparseFieldFourthOrderLevelSetImageFilter.hxx"
#endif

#endif
