/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkInvertDisplacementFieldImageFilter_h
#define itkInvertDisplacementFieldImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include <mutex>

namespace itk
{

/**
 * \class InvertDisplacementFieldImageFilter
 * \brief Iteratively estimates the inverse of a displacement field by fixed-point composition.
 *
 * \par Overview (implementation)
 * Given a forward displacement field \f$ \mathbf{u}(\mathbf{x}) \f$ (mapping points
 * \f$ \mathbf{x} \mapsto \mathbf{x} + \mathbf{u}(\mathbf{x}) \f$), the inverse field
 * \f$ \mathbf{v}(\mathbf{y}) \f$ satisfies
 * \f[
 *    \mathbf{x} + \mathbf{u}(\mathbf{x}) = \mathbf{y}, \quad
 *    \mathbf{y} + \mathbf{v}(\mathbf{y}) = \mathbf{x}.
 * \f]
 * Eliminating \f$\mathbf{x}\f$ yields the fixed-point condition
 * \f[
 *    \mathbf{v}(\mathbf{y}) \approx -\,\mathbf{u}\!\left(\mathbf{y} + \mathbf{v}(\mathbf{y})\right).
 * \f]
 * This filter solves that condition by iterative composition starting from an initial
 * inverse estimate (optionally supplied by the user). At each iteration, the forward
 * field is interpolated at warped locations \f$ \mathbf{y} + \mathbf{v}^{(k)}(\mathbf{y}) \f$
 * and the inverse is updated to reduce both the mean and max residual norms until
 * user-specified tolerances or an iteration cap is reached. The implementation supports
 * multithreading and vector-image interpolation; linear vector interpolation is used by
 * default.
 *
 * \par Algorithmic sketch
 * For output lattice point \f$\mathbf{y}\f$:
 *  - Initialize \f$\mathbf{v}^{(0)}(\mathbf{y})\f$ to the provided \c InverseFieldInitialEstimate
 *    (or zero if none).
 *  - Iterate \f$k = 0,1,\dots\f$ up to \c MaximumNumberOfIterations (default 20):
 *    \f[
 *      \mathbf{r}^{(k)}(\mathbf{y}) = \mathbf{u}\!\left(\mathbf{y} + \mathbf{v}^{(k)}(\mathbf{y})\right)
 *      + \mathbf{v}^{(k)}(\mathbf{y}),
 *    \f]
 *    \f[
 *      \mathbf{v}^{(k+1)}(\mathbf{y}) = \mathbf{v}^{(k)}(\mathbf{y}) - \mathbf{r}^{(k)}(\mathbf{y}).
 *    \f]
 *  - Stop when \f$\max_{\mathbf{y}}\|\mathbf{r}^{(k)}(\mathbf{y})\|\f$ and
 *    \f$\mathrm{mean}_{\mathbf{y}}\|\mathbf{r}^{(k)}(\mathbf{y})\|\f$ fall below
 *    \c MaxErrorToleranceThreshold and \c MeanErrorToleranceThreshold.
 *
 * \par Designed usage and assumptions
 *  - Best used inside iterative registration where forward updates are small and
 *    diffeomorphic at each step; supplying the previous iteration’s inverse as the
 *    initial estimate greatly accelerates convergence and improves robustness.
 *  - Not intended to recover a full inverse from identity in one step for large
 *    deformations; prefer multi-resolution schemes, incremental composition, or
 *    scaling-and-squaring to stay within the contraction regime of the fixed-point map.
 *  - The forward field should be (approximately) invertible in the region of interest
 *    (positive Jacobian determinant). Non-invertible folds will stall or diverge.
 *  - Boundary handling: when \c EnforceBoundaryCondition=true (default), the inverse is
 *    clamped to zero at the image boundary to avoid extrapolation artifacts.
 *
 * \par Complexity and performance
 * Each iteration performs one interpolation and vector update per voxel:
 * \f$O(N \times I)\f$ work for \f$N\f$ voxels and \f$I\f$ iterations. The filter
 * parallelizes across the output region and reuses an internal composed field to
 * minimize memory traffic.
 *
 * \par Relationship to Symmetric Normalization (SyN)
 * This filter is a core component of the Symmetric Normalization (SyN) registration
 * algorithm: at each iteration SyN updates forward and inverse velocity/displacement
 * estimates symmetrically and uses this routine to maintain an explicit inverse field,
 * preserving inverse-consistency during optimization \cite{Christensen2001MMBIA,christensen2001}.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \par References
 * - \cite{christensen2001} Christensen, G. E., & Johnson, H. J. (2001).
 *   Consistent image registration.
 *   *IEEE Transactions on Medical Imaging*, 20(7), 568–582.
 *
 * \ingroup ITKDisplacementField
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT InvertDisplacementFieldImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(InvertDisplacementFieldImageFilter);

  using Self = InvertDisplacementFieldImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(InvertDisplacementFieldImageFilter);

  /** Extract dimension from input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputFieldType = TInputImage;
  using OutputFieldType = TOutputImage;

  using DisplacementFieldType = InputFieldType;
  using InverseDisplacementFieldType = OutputFieldType;

  /** Image type alias support */
  using PixelType = typename OutputFieldType::PixelType;
  using VectorType = typename OutputFieldType::PixelType;
  using RegionType = typename OutputFieldType::RegionType;
  using OutputImageRegionType = RegionType;
  using IndexType = typename OutputFieldType::IndexType;

  using PointType = typename OutputFieldType::PointType;
  using SpacingType = typename OutputFieldType::SpacingType;
  using OriginType = typename OutputFieldType::PointType;
  using SizeType = typename OutputFieldType::SizeType;
  using DirectionType = typename OutputFieldType::DirectionType;

  /** Other type alias */
  using RealType = typename VectorType::ComponentType;
  using RealImageType = Image<RealType, ImageDimension>;
  using InterpolatorType = VectorInterpolateImageFunction<InputFieldType, RealType>;
  using DefaultInterpolatorType = VectorLinearInterpolateImageFunction<InputFieldType, RealType>;

  /** Get the interpolator. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the deformation field */
  void
  SetDisplacementField(const InputFieldType * field)
  {
    itkDebugMacro("setting deformation field to " << field);
    if (field != this->GetInput(0))
    {
      this->SetInput(0, field);
      this->Modified();
      if (!this->m_Interpolator.IsNull())
      {
        this->m_Interpolator->SetInputImage(field);
      }
    }
  }

  /**
   * Get the deformation field.
   */
  const InputFieldType *
  GetDisplacementField() const
  {
    return this->GetInput(0);
  }

  /** Set/get the initial estimate for the inverse field (optional). */
  /** @ITKStartGrouping */
  itkSetInputMacro(InverseFieldInitialEstimate, InverseDisplacementFieldType);
  itkGetInputMacro(InverseFieldInitialEstimate, InverseDisplacementFieldType);
  /** @ITKEndGrouping */

  /* Set the interpolator. */
  virtual void
  SetInterpolator(InterpolatorType * interpolator);

  /* Set/Get the number of iterations */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);
  itkGetConstMacro(MaximumNumberOfIterations, unsigned int);

  /* Set/Get the mean stopping criterion */
  itkSetMacro(MeanErrorToleranceThreshold, RealType);
  itkGetConstMacro(MeanErrorToleranceThreshold, RealType);

  /* Set/Get the max stopping criterion */
  itkSetMacro(MaxErrorToleranceThreshold, RealType);
  itkGetConstMacro(MaxErrorToleranceThreshold, RealType);

  /* Get the max norm */
  itkGetConstMacro(MaxErrorNorm, RealType);

  /* Get the mean norm */
  itkGetConstMacro(MeanErrorNorm, RealType);

  /* Should we force the boundary to have zero displacement? */
  itkSetMacro(EnforceBoundaryCondition, bool);
  itkGetMacro(EnforceBoundaryCondition, bool);
  itkBooleanMacro(EnforceBoundaryCondition);

protected:
  /** Constructor */
  InvertDisplacementFieldImageFilter();

  /** Deconstructor */
  ~InvertDisplacementFieldImageFilter() override = default;

  /** Standard print self function **/
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** preprocessing function */
  void
  GenerateData() override;

  /** Multithreaded function which generates the output field. */
  void
  DynamicThreadedGenerateData(const RegionType &) override;

private:
  /** The interpolator. */
  typename InterpolatorType::Pointer m_Interpolator{};

  unsigned int m_MaximumNumberOfIterations{ 20 };

  RealType m_MaxErrorToleranceThreshold{};
  RealType m_MeanErrorToleranceThreshold{};

  // internal ivars necessary for multithreading basic operations

  typename DisplacementFieldType::Pointer m_ComposedField{};
  typename RealImageType::Pointer         m_ScaledNormImage{};

  RealType    m_MaxErrorNorm{};
  RealType    m_MeanErrorNorm{};
  RealType    m_Epsilon{};
  SpacingType m_DisplacementFieldSpacing{};
  bool        m_DoThreadedEstimateInverse{ false };
  bool        m_EnforceBoundaryCondition{ true };
  std::mutex  m_Mutex{};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInvertDisplacementFieldImageFilter.hxx"
#endif

#endif
