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
#include "itkEnableIf.h"
#include "itkIsSame.h"

#include <vector>

namespace itk
{
/** \class PatchBasedDenoisingImageFilter
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
class ITK_TEMPLATE_EXPORT PatchBasedDenoisingImageFilter :
  public PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PatchBasedDenoisingImageFilter                                Self;
  typedef PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                                            Pointer;
  typedef SmartPointer<const Self>                                      ConstPointer;
  typedef typename Superclass::OutputImagePointer                       OutputImagePointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PatchBasedDenoisingImageFilter,
    PatchBasedDenoisingBaseImageFilter);

  /** Type definition for the input image. */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** Image dimension, assumed to be the same for input and output data*/
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Type definition for the input image region and size type. */
  typedef typename InputImageType::RegionType InputImageRegionType;

  /** Type definition for the input image region iterator */
  typedef ImageRegionIterator<OutputImageType>     OutputImageRegionIteratorType;
  typedef ImageRegionConstIterator<InputImageType> InputImageRegionConstIteratorType;

  /** Type definition for the input and output pixel types.
      Output pixel type will be used in computations. */
  typedef typename Superclass::PixelType      PixelType;
  typedef typename Superclass::PixelValueType PixelValueType;

  typedef typename NumericTraits< PixelType >::RealType      RealType;
  typedef typename NumericTraits< PixelValueType >::RealType RealValueType;
  typedef Array<PixelValueType>                              PixelArrayType;
  typedef Array<RealValueType>                               RealArrayType;
  typedef Array<unsigned short>                              ShortArrayType;

  /** Type definition for patch weights type. */
  typedef typename Superclass::ListAdaptorType         ListAdaptorType;
  typedef typename Superclass::PatchRadiusType         PatchRadiusType;
  typedef typename Superclass::InputImagePatchIterator InputImagePatchIterator;
  typedef ListAdaptorType                              PatchSampleType;
  typedef typename Superclass::PatchWeightsType        PatchWeightsType;

  /** Type definitions for delegate classes. */
  typedef itk::Statistics::RegionConstrainedSubsampler<
      PatchSampleType, InputImageRegionType >          BaseSamplerType;
  typedef typename BaseSamplerType::Pointer            BaseSamplerPointer;
  typedef typename BaseSamplerType::InstanceIdentifier InstanceIdentifier;

  /**
   * Type definitions for Riemannian LogMap Eigensystem.
   * Since the LogMap computations are only valid for DiffusionTensor3D
   * pixels right now which always have a dimension of 3x3.
   */
  typedef FixedArray< PixelValueType, 3 >      EigenValuesArrayType;
  typedef Matrix< PixelValueType, 3, 3 >       EigenVectorsMatrixType;
  typedef std::vector<EigenValuesArrayType>    EigenValuesCacheType;
  typedef std::vector<EigenVectorsMatrixType>  EigenVectorsCacheType;

  struct ThreadDataStruct
    {
    ShortArrayType validDerivatives;
    RealArrayType entropyFirstDerivative;
    RealArrayType entropySecondDerivative;
    ShortArrayType validNorms;
    RealArrayType minNorm;
    RealArrayType maxNorm;
    BaseSamplerPointer sampler;
    EigenValuesCacheType eigenValsCache;
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
  void SetKernelBandwidthSigma(const RealArrayType& kernelSigma);
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
   *  symmetric positive-definite eigensystem analysys will be used. See
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
  itkStaticConstMacro(MaxSigmaUpdateIterations, unsigned int,
                      20);

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
  void SetNoiseSigma(const RealType& sigma);

  itkGetConstMacro(NoiseSigma, RealType);

  /** Set/Get the class used for creating a subsample of patches. */
  itkSetObjectMacro(Sampler, BaseSamplerType);
  itkGetModifiableObjectMacro(Sampler, BaseSamplerType);

  /** Get the number of independent components of the input. */
  itkGetConstMacro(NumIndependentComponents, unsigned int);

protected:
  PatchBasedDenoisingImageFilter();
  ~PatchBasedDenoisingImageFilter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** Clean up Eigensystem caches */
  virtual void EmptyCaches();

  /** Allocate memory for a temporary update container in the subclass*/
  virtual void AllocateUpdateBuffer() ITK_OVERRIDE;

  virtual void CopyInputToOutput() ITK_OVERRIDE;

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** \brief A method to generically get a component.
   *
   * The same function name can be used to generically access for
   * scalars and array-like types. For scalar types the idx parameter
   * is ignored.
  */
  template< typename T>
  typename EnableIfC<
    IsSame<T, typename NumericTraits<T>::ValueType>::Value,
    T >::Type
  GetComponent(const T pix,
               unsigned int itkNotUsed( idx ) ) const
  {
    // The enable if idiom is used to overload this method for both
    // scalars and multi-component types. By exploiting that
    // NumericTraits' ValueType typedef (defines the per-element type
    // for multi-component types ) is different then the parameterize
    // type, the bracket operator is used only for multi-component
    // types.
    return pix;
  }

  template< typename T>
  typename DisableIfC<
    IsSame<T, typename NumericTraits<T>::ValueType>::Value,
    typename NumericTraits<T>::ValueType>::Type
  GetComponent(const T& pix,
               unsigned int idx ) const
  {
    return pix[idx];
  }

  /** \brief A method to generically set a component */
  template< typename T >
  void
  SetComponent( T &pix,
                unsigned int itkNotUsed( idx ),
                typename EnableIfC< IsSame<T,
                                           typename NumericTraits<T>::ValueType>::Value, RealValueType>::Type val) const
  {
    pix = val;
  }

  template< typename T >
  void
  SetComponent( T &pix,
                unsigned int idx,
                typename DisableIfC< IsSame<T,
                                            typename NumericTraits<T>::ValueType>::Value,
                                     RealValueType>::Type val) const
  {
    pix[idx] =  val;
  }

  /** Compute the Minimum and Maximum pixel in the image for each independent
    component */

  void ComputeMinMax(const Image< DiffusionTensor3D<PixelValueType> , ImageDimension>* img)
  {
    if( this->GetComponentSpace() == Superclass::RIEMANNIAN )
      {
      DispatchedRiemannianMinMax(img);
      }
    else
      {
      DispatchedArrayMinMax(img);
      }
  }

  template< typename TImageType>
  typename EnableIfC<
    IsSame<typename TImageType::PixelType, typename NumericTraits<typename TImageType::PixelType>::ValueType>::Value
    >::Type
  ComputeMinMax(const TImageType* img)
  {
    DispatchedMinMax(img);
  }

  template< typename TImageType>
  typename DisableIfC<
    IsSame<typename TImageType::PixelType, typename NumericTraits<typename TImageType::PixelType>::ValueType>::Value
    >::Type
  ComputeMinMax(const TImageType* img)
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
  void ComputeDifferenceAndWeightedSquaredNorm(const DiffusionTensor3D<PixelValueType>& a,
                                               const DiffusionTensor3D<PixelValueType>& b,
                                               const RealArrayType& weight,
                                               bool useCachedComputations,
                                               SizeValueType cacheIndex,
                                               EigenValuesCacheType& eigenValsCache,
                                               EigenVectorsCacheType& eigenVecsCache,
                                               RealType& diff, RealArrayType& norm)
  {
    if( this->GetComponentSpace() == Superclass::RIEMANNIAN )
      {
      ComputeLogMapAndWeightedSquaredGeodesicDifference(a, b, weight,
                                                        useCachedComputations, cacheIndex,
                                                        eigenValsCache, eigenVecsCache,
                                                        diff, norm);
      }
    else
      {
      ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(a, b, weight,
                                                             useCachedComputations, cacheIndex,
                                                             eigenValsCache, eigenVecsCache,
                                                             diff, norm);
      }
  }

  template <typename PixelT>
  void ComputeDifferenceAndWeightedSquaredNorm(const PixelT& a,
                                               const PixelT& b,
                                               const RealArrayType& weight,
                                               bool useCachedComputations,
                                               SizeValueType cacheIndex,
                                               EigenValuesCacheType& eigenValsCache,
                                               EigenVectorsCacheType& eigenVecsCache,
                                               RealType& diff, RealArrayType& norm)
  {
    ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(a, b, weight,
                                                           useCachedComputations, cacheIndex,
                                                           eigenValsCache, eigenVecsCache,
                                                           diff, norm);
  }

  /**
   * Update a by adding b. In Riemannian space, b is in the tangent space of a.
   */
  RealType AddUpdate(const DiffusionTensor3D<RealValueType>& a,
                     const RealType& b)
  {
    if( this->GetComponentSpace() == Superclass::RIEMANNIAN )
      {
      return this->AddExponentialMapUpdate(a, b);
      }
    else
      {
      return this->AddEuclideanUpdate(a, b);
      }
  }

  template <typename RealT>
  RealType AddUpdate(const RealT& a,
                     const RealType& b)
  {
    return this->AddEuclideanUpdate(a, b);
  }

  virtual void EnforceConstraints();

  virtual void Initialize() ITK_OVERRIDE;

  virtual void InitializeKernelSigma();

  virtual void InitializePatchWeights() ITK_OVERRIDE;

  virtual void InitializePatchWeightsSmoothDisc();

  virtual void InitializeIteration() ITK_OVERRIDE;

  virtual void ComputeKernelBandwidthUpdate() ITK_OVERRIDE; // derived from base class;

  // define here

  virtual ThreadDataStruct ThreadedComputeSigmaUpdate(const InputImageRegionType& regionToProcess,
                                                      const int itkNotUsed(threadId),
                                                      ThreadDataStruct threadData);

  virtual RealArrayType ResolveSigmaUpdate();

  virtual void ComputeImageUpdate() ITK_OVERRIDE;

  virtual ThreadDataStruct ThreadedComputeImageUpdate(const InputImageRegionType& regionToProcess,
                                                      const int threadId,
                                                      ThreadDataStruct threadData);

  virtual RealType ComputeGradientJointEntropy(InstanceIdentifier id,
                                               typename ListAdaptorType::Pointer& inList,
                                               BaseSamplerPointer& sampler,
                                               ThreadDataStruct& threadData);

  virtual void ApplyUpdate() ITK_OVERRIDE;

  virtual void ThreadedApplyUpdate(const InputImageRegionType& regionToProcess,
                                   const int itkNotUsed(threadId) );

  virtual void PostProcessOutput() ITK_OVERRIDE;

  virtual void SetThreadData(int threadId, const ThreadDataStruct& data);

  virtual ThreadDataStruct GetThreadData(int threadId);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PatchBasedDenoisingImageFilter);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ComputeSigma for processing. */
  static ITK_THREAD_RETURN_TYPE ComputeSigmaUpdateThreaderCallback( void *arg );

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ComputeImageUpdate for processing. */
  static ITK_THREAD_RETURN_TYPE ComputeImageUpdateThreaderCallback( void *arg );

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ThreadedApplyUpdate for processing. */
  static ITK_THREAD_RETURN_TYPE ApplyUpdateThreaderCallback( void *arg );

  template <typename TInputImageType>
  void DispatchedMinMax(const TInputImageType* img);

  template <typename TInputImageType>
  void DispatchedArrayMinMax(const TInputImageType* img);

  template <typename TInputImageType>
  void DispatchedVectorMinMax(const TInputImageType* img);

  template <typename TInputImageType>
  void DispatchedRiemannianMinMax(const TInputImageType* img);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ThreadedRiemannianMinMax for processing. */
  static ITK_THREAD_RETURN_TYPE RiemannianMinMaxThreaderCallback( void *arg );

  ThreadDataStruct ThreadedRiemannianMinMax(const InputImageRegionType& regionToProcess,
                                            const int itkNotUsed(threadId),
                                            const InputImageType* img,
                                            ThreadDataStruct threadData);

  virtual void ResolveRiemannianMinMax();

  void ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(const PixelType& a, const PixelType& b,
                                                              const RealArrayType& weight,
                                                              bool useCachedComputations,
                                                              SizeValueType cacheIndex,
                                                              EigenValuesCacheType& eigenValsCache,
                                                              EigenVectorsCacheType& eigenVecsCache,
                                                              RealType& diff, RealArrayType& norm);

  /** Returns the Log map in the tangent space of spdMatrixA. */
  void ComputeLogMapAndWeightedSquaredGeodesicDifference(const DiffusionTensor3D<PixelValueType>& spdMatrixA,
                                                         const DiffusionTensor3D<PixelValueType>& spdMatrixB,
                                                         const RealArrayType& weight,
                                                         bool useCachedComputations,
                                                         SizeValueType cacheIndex,
                                                         EigenValuesCacheType& eigenValsCache,
                                                         EigenVectorsCacheType& eigenVecsCache,
                                                         RealType& symMatrixLogMap, RealArrayType& geodesicDist);

  template <typename TensorValueT>
  void Compute3x3EigenAnalysis(const DiffusionTensor3D<TensorValueT>& spdMatrix,
                               FixedArray< TensorValueT, 3 >&  eigenVals,
                               Matrix< TensorValueT, 3, 3 >& eigenVecs);

  RealType AddEuclideanUpdate(const RealType& a, const RealType& b);

  /** Returns the Exp map */
  RealType AddExponentialMapUpdate(const DiffusionTensor3D<RealValueType>& spdMatrix,
                                   const DiffusionTensor3D<RealValueType>& symMatrix);

  struct ThreadFilterStruct
    {
    PatchBasedDenoisingImageFilter *Filter;
    InputImageType *Img;
    };

  std::vector<ThreadDataStruct> m_ThreadData;

  /** The buffer that holds the updates for an iteration of the algorithm. */
  typename OutputImageType::Pointer m_UpdateBuffer;

  unsigned int m_NumPixelComponents;
  unsigned int m_NumIndependentComponents;
  unsigned int m_TotalNumberPixels;

  bool m_UseSmoothDiscPatchWeights;

  bool m_UseFastTensorComputations;

  RealArrayType  m_KernelBandwidthSigma;
  bool           m_KernelBandwidthSigmaIsSet;
  RealArrayType  m_IntensityRescaleInvFactor;
  PixelType      m_ZeroPixel;
  PixelArrayType m_ImageMin;
  PixelArrayType m_ImageMax;
  double         m_KernelBandwidthFractionPixelsForEstimation;
  bool           m_ComputeConditionalDerivatives;
  double         m_MinSigma;
  double         m_MinProbability;
  unsigned int   m_SigmaUpdateDecimationFactor;
  double         m_SigmaUpdateConvergenceTolerance;
  ShortArrayType m_SigmaConverged;
  double         m_KernelBandwidthMultiplicationFactor;

  RealType m_NoiseSigma;
  RealType m_NoiseSigmaSquared;
  bool     m_NoiseSigmaIsSet;

  BaseSamplerPointer                m_Sampler;
  typename ListAdaptorType::Pointer m_SearchSpaceList;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPatchBasedDenoisingImageFilter.hxx"
#endif

#endif
