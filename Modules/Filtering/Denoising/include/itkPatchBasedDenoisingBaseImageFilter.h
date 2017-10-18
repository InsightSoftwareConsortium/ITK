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
#ifndef itkPatchBasedDenoisingBaseImageFilter_h
#define itkPatchBasedDenoisingBaseImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkArray.h"
#include "itkSample.h"
#include "itkNumericTraits.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkImageToNeighborhoodSampleAdaptor.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkDiffusionTensor3D.h"

namespace itk
{

/** \class PatchBasedDenoisingBaseImageFilter
 * \brief Base class for patch-based denoising algorithms.
 *
 * Implementation of a denoising filter that uses iterative non-local, or semi-local, weighted
 * averaging of image patches for image denoising. The intensity at each pixel 'p' gets updated as a
 * weighted average of intensities of a chosen subset of pixels from the image. The weights are
 * derived using a kernel function on distances between (i) the patch around pixel p and (ii) the
 * patches around the chosen subset of pixels in the image. This class of methods is motivated by
 * texture-based image models and relies on nonparametric statistical modeling in the
 * high-dimensional space of image patches. The choice of an appropriate kernel bandwidth parameter
 * underlying nonparametric modeling can be important and may be estimated using cross-validation
 * schemes.
 *
 * Engineering issues underlying this class of methods include the choice of the patch size, the
 * definition of a weighting mask on patches (e.g. to make patches more isotropic and less
 * rectangular), the number of iterations over the image, the scheme for sampling patches from the
 * image, and the weights balancing the regularization and data fidelity when the noise model is
 * known.
 *
 * This class of methods stems from the following two independent and simultaneous publications:
 *
 * Suyash P. Awate, Ross T. Whitaker.
 * Higher-Order Image Statistics for Unsupervised, Information-Theoretic, Adaptive, Image Filtering.
 * IEEE Int. Conf. Computer Vision and Pattern Recognition (CVPR) 2005; (2):44-51.
 *
 * Antoni Buades, Bartomeu Coll, Jean-Michel Morel.
 * A Non-Local Algorithm for Image Denoising.
 * IEEE Int. Conf. Computer Vision and Pattern Recognition (CVPR) 2005; (2):60-65.
 *
 * While the former work considers the denoising algorithm as performing entropy reduction using
 * nonparametric density estimation, the latter work treats it as nonparametric regression. Details
 * underlying this class of methods appear in:
 *
 * Suyash P. Awate, Ross T. Whitaker.
 * Unsupervised, Information-Theoretic, Adaptive Image Filtering for Image Restoration.
 * IEEE Transactions on Pattern Analysis and Machine Intelligence (TPAMI) 2006; 28(3):364-376.
 *
 * Antoni Buades, Bartomeu Coll, Jean-Michel Morel.
 * Nonlocal Image and Movie Denoising.
 * International Journal of Computer Vision (IJCV) 2008; 76(2):123-139.
 *
 * This class provides the base software framework for implementing patch-based denoising methods
 * for multi-dimensional and multi-channel (i.e. vector-valued pixels) images. This framework is
 * multithreaded on shared-memory architectures. Multithreading is incorporated in, both,
 * intensity-updates and bandwidth-estimation stages by subdividing the image domain and associating
 * each sub-domain to a single thread for processing.
 *
 * To prevent oversmoothing, this class provides the framework for including a data-fidelity term
 * based on the knowledge of the noise model. The intensity updates are then treated as the sum of
 * (1) the weighted smoothing updates (weighted by SmoothingWeight) and
 * (2) the weighted fidelity updates (weighted by NoiseModelFidelityWeight) that prevent large
 * deviations of the denoised image from the noisy data.
 *
 * \ingroup Filtering
 * \ingroup ITKDenoising
 * \sa PatchBasedDenoisingImageFilter
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT PatchBasedDenoisingBaseImageFilter :
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PatchBasedDenoisingBaseImageFilter            Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PatchBasedDenoisingBaseImageFilter, ImageToImageFilter);

  /** Input and output image types. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Image dimension, assumed to be the same for input and output data. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Type definition for the input and output pixel types.
   *  Output pixel type will be used in computations.
   */
  typedef typename InputImageType::PixelType           InputPixelType;
  typedef typename OutputImageType::PixelType          OutputPixelType;
  typedef OutputPixelType                              PixelType;
  typedef typename NumericTraits<PixelType>::ValueType PixelValueType;

  /** Type definition for selecting the noise model. */
  typedef enum { NOMODEL = 0, GAUSSIAN = 1, RICIAN = 2, POISSON = 3 } NoiseModelType;

  /** Type definition to determine which space to do calculations in. */
  /** TODO add comment about why no noise model can be used for RIEMANNIAN space
    */
  typedef enum { EUCLIDEAN = 0, RIEMANNIAN = 1 } ComponentSpaceType;

  /** State that the filter is in, i.e. UNINITIALIZED or INITIALIZED. */
  typedef enum { UNINITIALIZED = 0, INITIALIZED = 1 } FilterStateType;

  /** This data structure type is used to store the weights (mask) for pixels in a patch in order to
   *  make the patch more isotropic and less rectangular.
   */
  typedef Array<float> PatchWeightsType;
  /** This data structure type is used for efficiently accessing patch values
   *  from the image data structure.
   */
  typedef ZeroFluxNeumannBoundaryCondition<OutputImageType> BoundaryConditionType;
  typedef typename::itk::Statistics::ImageToNeighborhoodSampleAdaptor<
      OutputImageType, BoundaryConditionType >              ListAdaptorType;
  typedef typename ListAdaptorType::NeighborhoodRadiusType  PatchRadiusType;
  typedef ConstNeighborhoodIterator<
      InputImageType, BoundaryConditionType >               InputImagePatchIterator;

  /** Set/Get the patch radius specified in physical coordinates.
   * Patch radius is preferably set to an even number.
   * Currently, only isotropic patches in physical space are allowed;
   * patches can be anisotropic in voxel space.
   */
  itkSetMacro(PatchRadius, unsigned int);
  itkGetConstMacro(PatchRadius, unsigned int);

  PatchRadiusType GetPatchRadiusInVoxels() const;

  PatchRadiusType GetPatchDiameterInVoxels() const;

  typename PatchRadiusType::SizeValueType GetPatchLengthInVoxels() const;

  /** Set/Get the patch weights, or mask, that make the patch more isotropic (less rectangular).
   * This function allows the user to set arbitrary patch weights
   * by providing a 1-D array of weights.
   */
  void SetPatchWeights(const PatchWeightsType& weights);

  PatchWeightsType GetPatchWeights() const;

  /** Set/Get the noise model type.
   * Defaults to NOMODEL.
   * To use the noise model during denoising, NoiseModelFidelityWeight must be positive.
   */
  itkSetMacro(NoiseModel, NoiseModelType);
  itkGetConstMacro(NoiseModel, NoiseModelType);

  /** Set/Get the weight on the smoothing term.
   *  This option is used when a noise model is specified.
   *  This weight controls the balance between the smoothing and the closeness to the noisy data.
   *  Large stepsizes may cause instabilities.
   */
  itkSetClampMacro(SmoothingWeight, double, 0.0, 1.0);
  itkGetConstMacro(SmoothingWeight, double);

  /** Set/Get the weight on the fidelity term (penalizes deviations from the noisy data).
   *  This option is used when a noise model is specified.
   *  This weight controls the balance between the smoothing and the closeness to the noisy data.
   *  Use a positive weight to prevent oversmoothing.
   */
  itkSetClampMacro(NoiseModelFidelityWeight, double, 0.0, 1.0);
  itkGetConstMacro(NoiseModelFidelityWeight, double);

  /** Set/Get flag indicating whether kernel-bandwidth should be estimated
   *  automatically from the image data.
   *  Defaults to false.
   */
  itkSetMacro(KernelBandwidthEstimation, bool);
  itkBooleanMacro(KernelBandwidthEstimation);
  itkGetConstMacro(KernelBandwidthEstimation, bool);

  /** Set/Get the update frequency for the kernel bandwidth estimation.
   *  An optimal bandwidth will be re-estimated
   *  based on the denoised image after every 'n' iterations.
   *  Must be a positive integer.
   *  Defaults to 3, i.e. bandwidth updated after every 3 denoising iteration.
   */
  itkSetClampMacro(KernelBandwidthUpdateFrequency, unsigned int, 1, NumericTraits<unsigned int>::max() );
  itkGetConstMacro(KernelBandwidthUpdateFrequency, unsigned int);

  /** Set/Get the number of denoising iterations to perform.
   *  Must be a positive integer.
   *  Defaults to 1.
   */
  itkSetClampMacro(NumberOfIterations, unsigned int, 1, NumericTraits<unsigned int>::max() );
  itkGetConstReferenceMacro(NumberOfIterations, unsigned int);

  /** Get the number of elapsed iterations of the filter. */
  itkGetConstReferenceMacro(ElapsedIterations, unsigned int);

  /** Set/Get flag indicating whether all components should always be treated
   * as if they are in euclidean space regardless of pixel type.
   * Defaults to false.
   */
  itkSetMacro(AlwaysTreatComponentsAsEuclidean, bool);
  itkBooleanMacro(AlwaysTreatComponentsAsEuclidean);
  itkGetConstMacro(AlwaysTreatComponentsAsEuclidean, bool);

  /** Set the state of the filter to INITIALIZED. */
  virtual void SetStateToInitialized();

  /** Set the state of the filter to UNINITIALIZED. */
  virtual void SetStateToUninitialized();

  /** Set/Get the state of the filter. */
#if !defined(ITK_WRAPPING_PARSER)
  itkSetMacro(State, FilterStateType);
  itkGetConstReferenceMacro(State, FilterStateType);
#endif

  /** Indicates whether the filter automatically resets to UNINITIALIZED state
   * after completing, or whether filter must be manually reset.
   * Require the filter to be manually reinitialized (by calling
   * SetStateToUninitialized(). */
  itkSetMacro(ManualReinitialization, bool);
  itkGetConstReferenceMacro(ManualReinitialization, bool);
  itkBooleanMacro(ManualReinitialization);

protected:
  PatchBasedDenoisingBaseImageFilter();
  ~PatchBasedDenoisingBaseImageFilter() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  virtual void CopyInputToOutput() = 0;

  /** Allocate memory and initialize patch weights. */
  virtual void InitializePatchWeights();

  virtual void Initialize() { }

  /** Allocate memory for a temporary update container in the subclass. */
  virtual void AllocateUpdateBuffer() = 0;

  virtual void PreProcessInput() { }

  virtual void InitializeIteration() { }

  /** Automatically estimate kernel bandwidth from the image data. */
  virtual void ComputeKernelBandwidthUpdate() = 0;

  /** Perform one iteration of image denoising. */
  virtual void ComputeImageUpdate() = 0;

  virtual void ApplyUpdate() = 0;

  virtual void PostProcessOutput() { }

  /** Check and indicate whether to continue iterations or stop. */
  virtual bool Halt();

  virtual bool ThreadedHalt(void *itkNotUsed(threadInfo) )
  {
  return this->Halt();
  }

  itkSetMacro(ElapsedIterations, unsigned int);

  /** Determine the component space based on pixel type */
  ComponentSpaceType DetermineComponentSpace(const RGBPixel<PixelValueType>& itkNotUsed(p) )
  {
  return EUCLIDEAN;
  }

  ComponentSpaceType DetermineComponentSpace(const RGBAPixel<PixelValueType>& itkNotUsed(p) )
  {
  return EUCLIDEAN;
  }

  ComponentSpaceType DetermineComponentSpace(const DiffusionTensor3D<PixelValueType>& itkNotUsed(p) )
  {
  return RIEMANNIAN;
  }

  template <typename PixelT>
  ComponentSpaceType DetermineComponentSpace(const PixelT& itkNotUsed(p) )
  {
  return EUCLIDEAN;
  }

  /** Set/Get the component space type. */
  itkSetMacro(ComponentSpace, ComponentSpaceType);
  itkGetConstMacro(ComponentSpace, ComponentSpaceType);

  // Cache input and output pointer to get rid of thousands of calls
  // to GetInput and GetOutput.
  const InputImageType *m_InputImage;
  OutputImageType      *m_OutputImage;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PatchBasedDenoisingBaseImageFilter);

  /** Parameters that define patch size and patch weights (mask). */
  unsigned int     m_PatchRadius;
  PatchWeightsType m_PatchWeights;

  /** Parameters that define the strategy for kernel-bandwidth estimation. */
  bool         m_KernelBandwidthEstimation;
  unsigned int m_KernelBandwidthUpdateFrequency;

  /** Parameters that define the total number of denoising iterations to perform
   *  and those completed so far. */
  unsigned int m_NumberOfIterations;
  unsigned int m_ElapsedIterations;

  /** Parameters defining the usage of a specific noise model, if desired. */
  NoiseModelType m_NoiseModel;
  double         m_SmoothingWeight;
  double         m_NoiseModelFidelityWeight;

  /** Parameter indicating whether components should be treated as if they are in
      Euclidean space regardless of pixel type. */
  bool               m_AlwaysTreatComponentsAsEuclidean;
  ComponentSpaceType m_ComponentSpace;

  bool m_ManualReinitialization;

  FilterStateType m_State;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPatchBasedDenoisingBaseImageFilter.hxx"
#endif

#endif
