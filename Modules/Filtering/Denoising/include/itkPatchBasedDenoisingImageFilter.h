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
#ifndef itkPatchBasedDenoisingImageFilter_h
#define itkPatchBasedDenoisingImageFilter_h

#include "itkPatchBasedDenoisingBaseImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkVector.h"
#include "itkVectorImage.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkDiffusionTensor3D.h"
#include "itkFixedArray.h"
#include "itkMatrix.h"
#include "itkRegionConstrainedSubsampler.h"
#include <type_traits>

#include <vector>
#include "ITKDenoisingExport.h"

namespace itk
{
/**
 *\class PatchBasedDenoisingImageFilter
 * \brief Derived class implementing a specific patch-based denoising algorithm, as detailed below.
 *
 * This class is derived from the base class PatchBasedDenoisingBaseImageFilter; please refer to the
 * documentation of the base class first. This class implements a denoising filter that uses
 * iterative non-local, or semi-local, weighted averaging of image patches for image denoising. The
 * intensity at each pixel 'p' gets updated as a weighted average of intensities of a chosen subset
 * of pixels from the image.
 *
 * This class implements the denoising algorithm using a Gaussian kernel function for nonparametric
 * density estimation. The class implements a scheme to automatically estimated the kernel bandwidth
 * parameter (namely, sigma) using leave-one-out cross validation. It implements schemes for random
 * sampling of patches non-locally (from the entire image) as well as semi-locally (from the spatial
 * proximity of the pixel being denoised at the specific point in time). It implements a specific
 * scheme for defining patch weights (mask) as described in Awate and Whitaker 2005 IEEE CVPR and
 * 2006 IEEE TPAMI.
 *
 * \ingroup Filtering
 * \ingroup ITKDenoising
 * \sa PatchBasedDenoisingBaseImageFilter
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT PatchBasedDenoisingImageFilter
  : public PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PatchBasedDenoisingImageFilter);

  /** Standard class type aliases. */
  using Self = PatchBasedDenoisingImageFilter;
  using Superclass = PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using OutputImagePointer = typename Superclass::OutputImagePointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PatchBasedDenoisingImageFilter, PatchBasedDenoisingBaseImageFilter);

  /** Type definition for the input image. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;

  /** Image dimension, assumed to be the same for input and output data*/
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Type definition for the input image region and size type. */
  using InputImageRegionType = typename InputImageType::RegionType;

  /** Type definition for the input image region iterator */
  using OutputImageRegionIteratorType = ImageRegionIterator<OutputImageType>;
  using InputImageRegionConstIteratorType = ImageRegionConstIterator<InputImageType>;

  /** Type definition for the input and output pixel types.
      Output pixel type will be used in computations. */
  using PixelType = typename Superclass::PixelType;
  using PixelValueType = typename Superclass::PixelValueType;

  using RealType = typename NumericTraits<PixelType>::RealType;
  using RealValueType = typename NumericTraits<PixelValueType>::RealType;
  using PixelArrayType = Array<PixelValueType>;
  using RealArrayType = Array<RealValueType>;
  using ShortArrayType = Array<unsigned short>;

  /** Type definition for patch weights type. */
  using ListAdaptorType = typename Superclass::ListAdaptorType;
  using PatchRadiusType = typename Superclass::PatchRadiusType;
  using InputImagePatchIterator = typename Superclass::InputImagePatchIterator;
  using PatchSampleType = ListAdaptorType;
  using PatchWeightsType = typename Superclass::PatchWeightsType;

  /** Type definitions for delegate classes. */
  using BaseSamplerType = itk::Statistics::RegionConstrainedSubsampler<PatchSampleType, InputImageRegionType>;
  using BaseSamplerPointer = typename BaseSamplerType::Pointer;
  using InstanceIdentifier = typename BaseSamplerType::InstanceIdentifier;

  /**
   * Type definitions for Riemannian LogMap Eigensystem.
   * Since the LogMap computations are only valid for DiffusionTensor3D
   * pixels right now which always have a dimension of 3x3.
   */
  using EigenValuesArrayType = FixedArray<PixelValueType, 3>;
  using EigenVectorsMatrixType = Matrix<PixelValueType, 3, 3>;
  using EigenValuesCacheType = std::vector<EigenValuesArrayType>;
  using EigenVectorsCacheType = std::vector<EigenVectorsMatrixType>;

  struct ThreadDataStruct
  {
    ShortArrayType        validDerivatives;
    RealArrayType         entropyFirstDerivative;
    RealArrayType         entropySecondDerivative;
    ShortArrayType        validNorms;
    RealArrayType         minNorm;
    RealArrayType         maxNorm;
    BaseSamplerPointer    sampler;
    EigenValuesCacheType  eigenValsCache;
    EigenVectorsCacheType eigenVecsCache;
  };

  /** Set/Get flag indicating whether smooth-disc patch weights should be used.
   *  If this flag is true, the smooth-disc patch weights will override any
   *  weights provided via the SetPatchWeights method.
   */
  itkSetMacro(UseSmoothDiscPatchWeights, bool);
  itkBooleanMacro(UseSmoothDiscPatchWeights);
  itkGetConstMacro(UseSmoothDiscPatchWeights, bool);

  /** Set/Get initial kernel bandwidth estimate.
   * To prevent the class from automatically modifying this estimate,
   * set KernelBandwidthEstimation to false in the base class.
   */
  void
  SetKernelBandwidthSigma(const RealArrayType & kernelSigma);
  itkGetConstMacro(KernelBandwidthSigma, RealArrayType);

  /** Set/Get the fraction of the image to use for kernel bandwidth sigma estimation.
   *  To reduce the computational burden for computing sigma,
   *  a small random fraction of the image pixels can be used.
   */
  itkSetClampMacro(KernelBandwidthFractionPixelsForEstimation, double, 0.01, 1.0);
  itkGetConstReferenceMacro(KernelBandwidthFractionPixelsForEstimation, double);

  /** Set/Get flag indicating whether conditional derivatives should be used
    estimating sigma. */
  itkSetMacro(ComputeConditionalDerivatives, bool);
  itkBooleanMacro(ComputeConditionalDerivatives);
  itkGetConstMacro(ComputeConditionalDerivatives, bool);

  /** Set/Get flag indicating whether the fast algorithm for tensor computations should be used.
   *
   *  Specifically, when this flag is true (default) or On, a faster implementation of the 3x3
   *  symmetric positive-definite eigensystem analysis will be used. See
   *  Hasan KM, Basser PJ, Parker DL, Alexander AL.
   *  Analytical computation of the eigenvalues and eigenvectors in DT-MRI.
   *  J Magn Reson 2001; 152: 41-47.
   *  This faster algorithm may be slightly less accurate and possibly less stable in the presence
   *  of noise.  So far in practice it has been shown to work well.
   *
   *  However, you may want to turn this option off if you have concerns about numerical
   *  performance.
   */
  itkSetMacro(UseFastTensorComputations, bool);
  itkBooleanMacro(UseFastTensorComputations);
  itkGetConstMacro(UseFastTensorComputations, bool);

  /** Maximum number of Newton-Raphson iterations for sigma update. */
  static constexpr unsigned int MaxSigmaUpdateIterations = 20;

  /** Set/Get the kernel bandwidth sigma multiplication factor used to modify the
   *  automatically-estimated kernel bandwidth sigma. At times, it may be desirable
   *  to modify the value of the automatically-estimated sigma.  Typically, this number
   *  isn't very far from 1.
   *  Note: This is used only when KernelBandwidthEstimation is True/On.
   */
  itkSetClampMacro(KernelBandwidthMultiplicationFactor, double, 0.01, 100);
  itkGetConstReferenceMacro(KernelBandwidthMultiplicationFactor, double);

  /** Set/Get the noise sigma.
   * Used by the noise model where appropriate, defaults to 5% of the image intensity range
   */
  void
  SetNoiseSigma(const RealType & sigma);

  itkGetConstMacro(NoiseSigma, RealType);

  /** Set/Get the class used for creating a subsample of patches. */
  itkSetObjectMacro(Sampler, BaseSamplerType);
  itkGetModifiableObjectMacro(Sampler, BaseSamplerType);

  /** Get the number of independent components of the input. */
  itkGetConstMacro(NumIndependentComponents, unsigned int);

protected:
  PatchBasedDenoisingImageFilter();
  ~PatchBasedDenoisingImageFilter() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Clean up Eigensystem caches */
  virtual void
  EmptyCaches();

  /** Allocate memory for a temporary update container in the subclass*/
  void
  AllocateUpdateBuffer() override;

  void
  CopyInputToOutput() override;

  void
  GenerateInputRequestedRegion() override;

  template <typename T, typename U = void>
  using DisableIfMultiComponent =
    typename std::enable_if<std::is_same<T, typename NumericTraits<T>::ValueType>::value, U>;

  template <typename T, typename U = void>
  using EnableIfMultiComponent =
    typename std::enable_if<!std::is_same<T, typename NumericTraits<T>::ValueType>::value, U>;


  /** \brief A method to generically get a component.
   *
   * The same function name can be used to generically access for
   * scalars and array-like types. For scalar types the idx parameter
   * is ignored.
   */
  template <typename T>
  typename DisableIfMultiComponent<T, T>::type
  GetComponent(const T pix, unsigned int itkNotUsed(idx)) const
  {
    // The enable if idiom is used to overload this method for both
    // scalars and multi-component types. By exploiting that
    // NumericTraits' ValueType type alias (defines the per-element type
    // for multi-component types ) is different then the parameterize
    // type, the bracket operator is used only for multi-component
    // types.
    return pix;
  }

  template <typename T>
  typename EnableIfMultiComponent<T, typename NumericTraits<T>::ValueType>::type
  GetComponent(const T & pix, unsigned int idx) const
  {
    return pix[idx];
  }

  /** \brief A method to generically set a component */
  template <typename T>
  void
  SetComponent(T &                                                      pix,
               unsigned int                                             itkNotUsed(idx),
               typename DisableIfMultiComponent<T, RealValueType>::type val) const
  {
    pix = val;
  }

  template <typename T>
  void
  SetComponent(T & pix, unsigned int idx, typename EnableIfMultiComponent<T, RealValueType>::type val) const
  {
    pix[idx] = val;
  }

  /** Compute the Minimum and Maximum pixel in the image for each independent
    component */

  void
  ComputeMinMax(const Image<DiffusionTensor3D<PixelValueType>, ImageDimension> * img)
  {
    if (this->GetComponentSpace() == Superclass::ComponentSpaceEnum::RIEMANNIAN)
    {
      DispatchedRiemannianMinMax(img);
    }
    else
    {
      DispatchedArrayMinMax(img);
    }
  }

  template <typename TImageType>
  typename DisableIfMultiComponent<typename TImageType::PixelType>::type
  ComputeMinMax(const TImageType * img)
  {
    DispatchedMinMax(img);
  }

  template <typename TImageType>
  typename EnableIfMultiComponent<typename TImageType::PixelType>::type
  ComputeMinMax(const TImageType * img)
  {
    DispatchedArrayMinMax(img);
  }

  /**
   * Compute the signed difference a-b and the weighted squared distance
   * between a and b.  Do the computation in either Euclidean or Riemannian space
   * depending on pixel type.
   * The cache is used when the first argument is repeatedly passed into
   * ComputeLogMap since the eigen analysis will already have been computed
   * for that pixel.
   */
  void
  ComputeDifferenceAndWeightedSquaredNorm(const DiffusionTensor3D<PixelValueType> & a,
                                          const DiffusionTensor3D<PixelValueType> & b,
                                          const RealArrayType &                     weight,
                                          bool                                      useCachedComputations,
                                          SizeValueType                             cacheIndex,
                                          EigenValuesCacheType &                    eigenValsCache,
                                          EigenVectorsCacheType &                   eigenVecsCache,
                                          RealType &                                diff,
                                          RealArrayType &                           norm)
  {
    if (this->GetComponentSpace() == Superclass::ComponentSpaceEnum::RIEMANNIAN)
    {
      ComputeLogMapAndWeightedSquaredGeodesicDifference(
        a, b, weight, useCachedComputations, cacheIndex, eigenValsCache, eigenVecsCache, diff, norm);
    }
    else
    {
      ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(
        a, b, weight, useCachedComputations, cacheIndex, eigenValsCache, eigenVecsCache, diff, norm);
    }
  }

  template <typename PixelT>
  void
  ComputeDifferenceAndWeightedSquaredNorm(const PixelT &          a,
                                          const PixelT &          b,
                                          const RealArrayType &   weight,
                                          bool                    useCachedComputations,
                                          SizeValueType           cacheIndex,
                                          EigenValuesCacheType &  eigenValsCache,
                                          EigenVectorsCacheType & eigenVecsCache,
                                          RealType &              diff,
                                          RealArrayType &         norm)
  {
    ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(
      a, b, weight, useCachedComputations, cacheIndex, eigenValsCache, eigenVecsCache, diff, norm);
  }

  /**
   * Update a by adding b. In Riemannian space, b is in the tangent space of a.
   */
  RealType
  AddUpdate(const DiffusionTensor3D<RealValueType> & a, const RealType & b)
  {
    if (this->GetComponentSpace() == Superclass::ComponentSpaceEnum::RIEMANNIAN)
    {
      return this->AddExponentialMapUpdate(a, b);
    }
    else
    {
      return this->AddEuclideanUpdate(a, b);
    }
  }

  template <typename RealT>
  RealType
  AddUpdate(const RealT & a, const RealType & b)
  {
    return this->AddEuclideanUpdate(a, b);
  }

  virtual void
  EnforceConstraints();

  void
  Initialize() override;

  virtual void
  InitializeKernelSigma();

  void
  InitializePatchWeights() override;

  virtual void
  InitializePatchWeightsSmoothDisc();

  void
  InitializeIteration() override;

  void
  ComputeKernelBandwidthUpdate() override; // derived from base class;

  // define here

  virtual ThreadDataStruct
  ThreadedComputeSigmaUpdate(const InputImageRegionType & regionToProcess,
                             const int                    itkNotUsed(threadId),
                             ThreadDataStruct             threadData);

  virtual RealArrayType
  ResolveSigmaUpdate();

  void
  ComputeImageUpdate() override;

  virtual ThreadDataStruct
  ThreadedComputeImageUpdate(const InputImageRegionType & regionToProcess,
                             const int                    threadId,
                             ThreadDataStruct             threadData);

  virtual RealType
  ComputeGradientJointEntropy(InstanceIdentifier                  id,
                              typename ListAdaptorType::Pointer & inList,
                              BaseSamplerPointer &                sampler,
                              ThreadDataStruct &                  threadData);

  void
  ApplyUpdate() override;

  virtual void
  ThreadedApplyUpdate(const InputImageRegionType & regionToProcess, const int itkNotUsed(threadId));

  void
  PostProcessOutput() override;

  virtual void
  SetThreadData(int threadId, const ThreadDataStruct & data);

  virtual ThreadDataStruct
  GetThreadData(int threadId);

private:
  /** This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ComputeSigma for processing. */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  ComputeSigmaUpdateThreaderCallback(void * arg);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ComputeImageUpdate for processing. */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  ComputeImageUpdateThreaderCallback(void * arg);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ThreadedApplyUpdate for processing. */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  ApplyUpdateThreaderCallback(void * arg);

  template <typename TInputImageType>
  void
  DispatchedMinMax(const TInputImageType * img);

  template <typename TInputImageType>
  void
  DispatchedArrayMinMax(const TInputImageType * img);

  template <typename TInputImageType>
  void
  DispatchedVectorMinMax(const TInputImageType * img);

  template <typename TInputImageType>
  void
  DispatchedRiemannianMinMax(const TInputImageType * img);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ThreadedRiemannianMinMax for processing. */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  RiemannianMinMaxThreaderCallback(void * arg);

  ThreadDataStruct
  ThreadedRiemannianMinMax(const InputImageRegionType & regionToProcess,
                           const int                    itkNotUsed(threadId),
                           const InputImageType *       img,
                           ThreadDataStruct             threadData);

  virtual void
  ResolveRiemannianMinMax();

  void
  ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(const PixelType &       a,
                                                         const PixelType &       b,
                                                         const RealArrayType &   weight,
                                                         bool                    useCachedComputations,
                                                         SizeValueType           cacheIndex,
                                                         EigenValuesCacheType &  eigenValsCache,
                                                         EigenVectorsCacheType & eigenVecsCache,
                                                         RealType &              diff,
                                                         RealArrayType &         norm);

  /** Returns the Log map in the tangent space of spdMatrixA. */
  void
  ComputeLogMapAndWeightedSquaredGeodesicDifference(const DiffusionTensor3D<PixelValueType> & spdMatrixA,
                                                    const DiffusionTensor3D<PixelValueType> & spdMatrixB,
                                                    const RealArrayType &                     weight,
                                                    bool                                      useCachedComputations,
                                                    SizeValueType                             cacheIndex,
                                                    EigenValuesCacheType &                    eigenValsCache,
                                                    EigenVectorsCacheType &                   eigenVecsCache,
                                                    RealType &                                symMatrixLogMap,
                                                    RealArrayType &                           geodesicDist);

  template <typename TensorValueT>
  void
  Compute3x3EigenAnalysis(const DiffusionTensor3D<TensorValueT> & spdMatrix,
                          FixedArray<TensorValueT, 3> &           eigenVals,
                          Matrix<TensorValueT, 3, 3> &            eigenVecs);

  RealType
  AddEuclideanUpdate(const RealType & a, const RealType & b);

  /** Returns the Exp map */
  RealType
  AddExponentialMapUpdate(const DiffusionTensor3D<RealValueType> & spdMatrix,
                          const DiffusionTensor3D<RealValueType> & symMatrix);

  struct ThreadFilterStruct
  {
    PatchBasedDenoisingImageFilter * Filter;
    InputImageType *                 Img;
  };

  std::vector<ThreadDataStruct> m_ThreadData;

  /** The buffer that holds the updates for an iteration of the algorithm. */
  typename OutputImageType::Pointer m_UpdateBuffer;

  unsigned int m_NumPixelComponents{ 0 };
  unsigned int m_NumIndependentComponents{ 0 };
  unsigned int m_TotalNumberPixels{ 0 };

  bool m_UseSmoothDiscPatchWeights{ true };

  bool m_UseFastTensorComputations{ true };

  RealArrayType  m_KernelBandwidthSigma;
  bool           m_KernelBandwidthSigmaIsSet{ false };
  RealArrayType  m_IntensityRescaleInvFactor;
  PixelType      m_ZeroPixel;
  PixelArrayType m_ImageMin;
  PixelArrayType m_ImageMax;
  double         m_KernelBandwidthFractionPixelsForEstimation{ 0.20 };
  bool           m_ComputeConditionalDerivatives{ false };
  double         m_MinSigma;
  double         m_MinProbability;
  unsigned int   m_SigmaUpdateDecimationFactor;
  double         m_SigmaUpdateConvergenceTolerance{ 0.01 };
  ShortArrayType m_SigmaConverged;
  double         m_KernelBandwidthMultiplicationFactor{ 1.0 };

  RealType m_NoiseSigma;
  RealType m_NoiseSigmaSquared;
  bool     m_NoiseSigmaIsSet{ false };

  BaseSamplerPointer                m_Sampler;
  typename ListAdaptorType::Pointer m_SearchSpaceList;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPatchBasedDenoisingImageFilter.hxx"
#endif

#endif
