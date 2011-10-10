#ifndef __itkPatchBasedDenoisingImageFilter_h
#define __itkPatchBasedDenoisingImageFilter_h

#include "itkPatchBasedDenoisingBaseImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkVector.h"
#include "itkVectorImage.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkDiffusionTensor3D.h"
#include "itkRegionConstrainedSubsampler.h"
#ifdef ITK_HAS_STLTR1_TR1_TYPE_TRAITS
#  include <tr1/type_traits>
#elif defined ITK_HAS_STLTR1_TYPE_TRAITS
#  include <type_traits>
#else
#  include "itkIsSame.h"
#endif


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

template <class TInputImage, class TOutputImage>
class ITK_EXPORT PatchBasedDenoisingImageFilter :
public PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage>
{
 public:
#if defined(ITK_HAS_STLTR1_TR1_TYPE_TRAITS) || defined(ITK_HAS_STLTR1_TYPE_TRAITS)
  typedef std::tr1::true_type  TrueType;
  typedef std::tr1::false_type FalseType;
#else
  typedef itk::TrueType  TrueType;
  typedef itk::FalseType FalseType;
#endif

  /** Standard class typedefs. */
  typedef PatchBasedDenoisingImageFilter                                Self;
  typedef PatchBasedDenoisingBaseImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                                            Pointer;
  typedef SmartPointer<const Self>                                      ConstPointer;
  typedef typename Superclass::OutputImagePointer                       OutputImagePointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PatchBasedDenoisingImageFilter, ImageToImageFilter);

  /** Type definition for the input image. */
  typedef typename Superclass::InputImageType   InputImageType;
  typedef typename Superclass::OutputImageType  OutputImageType;

  /** Image dimension, assumed to be the same for input and output data*/
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Type definition for the input image region and size type. */
  typedef typename InputImageType::RegionType           InputImageRegionType;

  /** Type definition for the input image region iterator */
  typedef ImageRegionIterator<OutputImageType>          OutputImageRegionIteratorType;
  typedef ImageRegionConstIterator<InputImageType>      InputImageRegionConstIteratorType;

  /** Type definition for the input and output pixel types.
      Output pixel type will be used in computations. */
  typedef typename Superclass::PixelType      PixelType;
  typedef typename Superclass::PixelValueType PixelValueType;

  typedef typename NumericTraits< PixelType >::RealType RealType;
  typedef typename NumericTraits< PixelValueType >::RealType RealValueType;
  typedef Array<PixelValueType> PixelArrayType;
  typedef Array<RealValueType>  RealArrayType;
  typedef Array<unsigned short> ShortArrayType;

  /** Type definition for patch weights type. */
  typedef typename Superclass::ListAdaptorType ListAdaptorType;
  typedef typename Superclass::PatchRadiusType PatchRadiusType;
  typedef typename Superclass::InputImagePatchIterator InputImagePatchIterator;
  typedef ListAdaptorType PatchSampleType;

  typedef typename Superclass::PatchWeightsType PatchWeightsType;

  /** Type definitions for delegate classes. */
  typedef itk::Statistics::RegionConstrainedSubsampler<
    PatchSampleType, InputImageRegionType >  BaseSamplerType;
  typedef typename BaseSamplerType::Pointer  BaseSamplerPointer;
  typedef typename BaseSamplerType::InstanceIdentifier InstanceIdentifier;

  struct ThreadDataStruct
  {
    ShortArrayType     validDerivatives;
    RealArrayType      entropyFirstDerivative;
    RealArrayType      entropySecondDerivative;
    ShortArrayType     validNorms;
    RealArrayType      minNorm;
    RealArrayType      maxNorm;
    BaseSamplerPointer sampler;
  };


  /** Set/Get flag indicating whether smooth-disc patch weights should be used.
   *  If this flag is true, the smooth-disc patch weights will override any
   *  weights provided via the SetPatchWeights method.
   */
  itkSetMacro(UseSmoothDiscPatchWeights, bool);
  itkBooleanMacro(UseSmoothDiscPatchWeights);
  itkGetConstMacro(UseSmoothDiscPatchWeights, bool);



  /** Set/Get initial sigma estimate.
   * To prevent the class from automatically modifying this estimate,
   * set DoKernelBandwidthEstimation to false in the base class.
   */
  itkSetMacro(GaussianKernelSigma, RealArrayType);
  itkGetConstMacro(GaussianKernelSigma, RealArrayType);

  /** Set/Get the fraction of the image to use for sigma estimation.
   *  To reduce the computational burden for computing sigma,
   *  a small random fraction of the image pixels can be used.
   */
  itkSetClampMacro(FractionPixelsForSigmaUpdate, double, 0.01, 1.0);
  itkGetConstReferenceMacro(FractionPixelsForSigmaUpdate, double);

  /** Set/Get flag indicating whether conditional derivatives should be used estimating sigma. */
  itkSetMacro(ComputeConditionalDerivatives, bool);
  itkBooleanMacro(ComputeConditionalDerivatives);
  itkGetConstMacro(ComputeConditionalDerivatives, bool);

  /** Maximum number of Newton-Raphson iterations for sigma update. */
  itkStaticConstMacro(MaxSigmaUpdateIterations, unsigned int,
                      20);

  /** Set/Get the sigma multiplication factor used to modify the automatically-estimated sigma.
   *  At times, it may be desirable to modify the value of the automatically-estimated sigma.
   *  Typically, this number isn't very far from 1.
   *  Note: This is used only when DoKernelBandwidthEstimation is True/On.
   */
  itkSetClampMacro(SigmaMultiplicationFactor, double, 0.01, 100);
  itkGetConstReferenceMacro(SigmaMultiplicationFactor, double);

  /** Set/Get the noise sigma.
   * Used by the noise model where appropriate, defaults to 5% of the image intensity range
   */
  void SetNoiseSigma(const RealType& sigma);
  itkGetConstMacro(NoiseSigma, RealType);


  /** Set/Get the class used for creating a subsample of patches. */
  itkSetObjectMacro(Sampler, BaseSamplerType);
  itkGetObjectMacro(Sampler, BaseSamplerType);

 protected:
  PatchBasedDenoisingImageFilter();
  ~PatchBasedDenoisingImageFilter() { };
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Allocate memory for a temporary update container in the subclass*/
  virtual void AllocateUpdateBuffer();

  virtual void CopyInputToOutput();

  virtual void GenerateInputRequestedRegion();

  template <typename T>
  typename NumericTraits<T>::ValueType
  GetComponent(const T& pix, unsigned int idx) const
  {
    return DispatchedGetComponent(pix, idx
#if defined(ITK_HAS_STLTR1_TR1_TYPE_TRAITS) || defined(ITK_HAS_STLTR1_TYPE_TRAITS)
                                  , std::tr1::is_pod<PixelType>()
#endif
      );
  }

  template <typename T>
  void
  SetComponent(T& pix, unsigned int idx, typename NumericTraits<T>::ValueType val)
  {
    DispatchedSetComponent(pix, idx, val
#if defined(ITK_HAS_STLTR1_TR1_TYPE_TRAITS) || defined(ITK_HAS_STLTR1_TYPE_TRAITS)
                           , std::tr1::is_pod<PixelType>()
#endif
      );
  }

  /** Compute the Minimum and Maximum pixel in the image for each independent component */
  void ComputeMinMax(const VectorImage<PixelValueType, ImageDimension>* img)
  { DispatchedVectorMinMax(img); }
  void ComputeMinMax(const Image< DiffusionTensor3D<PixelValueType> , ImageDimension>* img)
  {
    if (this->m_ComponentSpace == Superclass::RIEMANNIAN)
    {
      DispatchedRiemannianMinMax(img);
    }
    else
    {
      DispatchedMinMax(img, FalseType() );
    }
  }
  template <class ImageT> void ComputeMinMax(const ImageT* img)
  {
    DispatchedMinMax(img
#if defined(ITK_HAS_STLTR1_TR1_TYPE_TRAITS) || defined(ITK_HAS_STLTR1_TYPE_TRAITS)
                     , std::tr1::is_pod<PixelType>()
#endif
      );
  }

  /**
   * Compute the signed difference a-b and the weighted squared distance
   * between a and b.  Do the computation in either Euclidean or Riemannian space
   * depending on pixel type.
   */
  void ComputeDifferenceAndWeightedSquaredNorm(const DiffusionTensor3D<PixelValueType>& a,
                                               const DiffusionTensor3D<PixelValueType>& b,
                                               const RealArrayType& weight,
                                               RealType& diff, RealArrayType& norm)
  {
    if (this->m_ComponentSpace == Superclass::RIEMANNIAN)
    {
      ComputeLogMapAndWeightedSquaredGeodesicDifference(a, b, weight, diff, norm);
    }
    else
    {
      ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(a, b, weight, diff, norm);
    }
  }
  template <class PixelT>
  void ComputeDifferenceAndWeightedSquaredNorm(const PixelT& a,
                                               const PixelT& b,
                                               const RealArrayType& weight,
                                               RealType& diff, RealArrayType& norm)
  {
    ComputeSignedEuclideanDifferenceAndWeightedSquaredNorm(a, b, weight, diff, norm);
  }

  /**
   * Update a by adding b.  In Riemannian space, b is in the tangent space of a.
   */
  RealType AddUpdate(const DiffusionTensor3D<RealValueType>& a,
                     const RealType& b)
  {
    if (this->m_ComponentSpace == Superclass::RIEMANNIAN)
    {
      return this->AddExponentialMapUpdate(a, b);
    }
    else
    {
      return this->AddEuclideanUpdate(a, b);
    }
  }
  template <class RealT>
  RealType AddUpdate(const RealT& a,
                     const RealType& b)
  {
    return this->AddEuclideanUpdate(a, b);
  }


  virtual void EnforceConstraints();

  virtual void Initialize();

  virtual void InitializeKernelSigma();

  virtual void InitializePatchWeights();
  virtual void InitializePatchWeightsSmoothDisc();

  virtual void InitializeIteration();

  virtual void ComputeKernelBandwidthUpdate(); // derived from base class; define here
  virtual ThreadDataStruct ThreadedComputeSigmaUpdate(const InputImageRegionType& regionToProcess,
                                                      const int itkNotUsed(threadId),
                                                      ThreadDataStruct threadData);
  virtual RealArrayType ResolveSigmaUpdate();


  virtual void ComputeImageUpdate();
  virtual ThreadDataStruct ThreadedComputeImageUpdate(const InputImageRegionType& regionToProcess,
                                                      const int threadId,
                                                      ThreadDataStruct threadData);
  virtual RealType ComputeGradientJointEntropy(InstanceIdentifier id,
                                               typename ListAdaptorType::Pointer& inList,
                                               BaseSamplerPointer& sampler);


  virtual void ApplyUpdate();
  virtual void ThreadedApplyUpdate(const InputImageRegionType& regionToProcess,
                                   const int itkNotUsed(threadId));


  virtual void PostProcessOutput();

  virtual void SetThreadData(int threadId, const ThreadDataStruct& data);
  virtual ThreadDataStruct GetThreadData(int threadId);

  std::vector<ThreadDataStruct> m_ThreadData;

  /** The buffer that holds the updates for an iteration of the algorithm. */
  typename OutputImageType::Pointer m_UpdateBuffer;

  unsigned int     m_NumPixelComponents;
  unsigned int     m_NumIndependentComponents;
  unsigned int     m_TotalNumberPixels;
  //
  bool             m_UseSmoothDiscPatchWeights;
  //
  RealArrayType    m_GaussianKernelSigma;
  RealArrayType    m_IntensityRescaleInvFactor;
  PixelType        m_ZeroPixel;
  PixelArrayType   m_ImageMin;
  PixelArrayType   m_ImageMax;
  double           m_FractionPixelsForSigmaUpdate;
  bool             m_ComputeConditionalDerivatives;
  double           m_MinSigma;
  double           m_MinProbability;
  unsigned int     m_SigmaUpdateDecimationFactor;
  double           m_SigmaUpdateConvergenceTolerance;
  ShortArrayType   m_SigmaConverged;
  double           m_SigmaMultiplicationFactor;
  //
  RealType         m_NoiseSigma;
  RealType         m_NoiseSigmaSquared;
  bool             m_NoiseSigmaIsSet;
  //
  BaseSamplerPointer m_Sampler;
  typename ListAdaptorType::Pointer m_SearchSpaceList;

 private:
  PatchBasedDenoisingImageFilter(const Self&); // purposely not implemented
  void operator=(const Self&); // purposely not implemented

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ComputeSigma for processing. */
  static ITK_THREAD_RETURN_TYPE ComputeSigmaUpdateThreaderCallback( void *arg );

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ComputeImageUpdate for processing. */
  static ITK_THREAD_RETURN_TYPE ComputeImageUpdateThreaderCallback( void *arg );

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire a
   * region which it then passes to ThreadedApplyUpdate for processing. */
  static ITK_THREAD_RETURN_TYPE ApplyUpdateThreaderCallback( void *arg );

  template <typename T>
  typename NumericTraits<T>::ValueType
  DispatchedGetComponent(const T& pix, unsigned int itkNotUsed(idx),
                         TrueType isPod) const;

  template <typename T>
  typename NumericTraits<T>::ValueType
  DispatchedGetComponent(const T& pix, unsigned int idx,
                         FalseType isPod = FalseType()) const;

  template <typename T>
  void
  DispatchedSetComponent(T& pix, unsigned int itkNotUsed(idx),
                         typename NumericTraits<T>::ValueType val, TrueType isPod);

  template <typename T>
  void
  DispatchedSetComponent(T& pix, unsigned int idx,
                         typename NumericTraits<T>::ValueType val, FalseType isPod = FalseType());

  void DispatchedMinMax(const InputImageType* img, TrueType isPod);

  void DispatchedMinMax(const InputImageType* img, FalseType isPod = FalseType());

  void DispatchedVectorMinMax(const InputImageType* img);

  void DispatchedRiemannianMinMax(const InputImageType* img);
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
                                                              RealType& diff, RealArrayType& norm);

  /** Returns the Log map in the tangent space of spdMatrixA. */
  void ComputeLogMapAndWeightedSquaredGeodesicDifference(const DiffusionTensor3D<PixelValueType>& spdMatrixA,
                                                         const DiffusionTensor3D<PixelValueType>& spdMatrixB,
                                                         const RealArrayType& weight,
                                                         RealType& symMatrixLogMap, RealArrayType& geodesicDist);

  RealType AddEuclideanUpdate(const RealType& a, const RealType& b);
  /** Returns the Exp map */
  RealType AddExponentialMapUpdate(const DiffusionTensor3D<RealValueType>& spdMatrix,
                                   const DiffusionTensor3D<RealValueType>& symMatrix);

  struct ThreadFilterStruct
  {
    PatchBasedDenoisingImageFilter *Filter;
    InputImageType                 *Img;
  };

 }; // end class PatchBasedDenoisingImageFilter

} // end namespace itk

// Define instantiation macro for this template
#define ITK_TEMPLATE_PatchBasedDenoisingImageFilter(_, EXPORT, x, y) namespace itk { \
    _(2(class EXPORT PatchBasedDenoisingImageFilter< ITK_TEMPLATE_2 x >)) | \
      namespace Templates { typedef PatchBasedDenoisingImageFilter< ITK_TEMPLATE_2 x > \
        PatchBasedDenoisingImageFilter##y; }                            \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkPatchBasedDenoisingImageFilter+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkPatchBasedDenoisingImageFilter.hxx"
#endif

#endif
