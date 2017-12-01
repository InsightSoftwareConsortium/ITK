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
#ifndef itkMRIBiasFieldCorrectionFilter_h
#define itkMRIBiasFieldCorrectionFilter_h

#include <ctime>

#include "itkImageToImageFilter.h"
#include "itkArray2D.h"
#include "itkMRASlabIdentifier.h"
#include "itkCompositeValleyFunction.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "itkNormalVariateGenerator.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/** \class MRIBiasEnergyFunction
 * \brief Represents a cost function for MRI bias field correction optimization.
 *
 * This is a wrapping class which provides interfaces between images,
 * the bias field, the internal energy function (CompositeValleyFunction),
 * and the Optimizer.
 *
 * This class is templated over the type of the input image (TImage),
 * the image mask (which tells which pixels in the input image should be
 * included for energy value calculation), and the bias field (TBiasField).
 * \ingroup ITKBiasCorrection
 */
template< typename TImage, typename TImageMask, typename TBiasField >
class ITK_TEMPLATE_EXPORT MRIBiasEnergyFunction : public SingleValuedCostFunction
{
public:
  /** Standard class typedefs. */
  typedef MRIBiasEnergyFunction      Self;
  typedef SingleValuedCostFunction   Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MRIBiasEnergyFunction, SingleValuedCostFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Image related type definitions. */
  typedef TImage                         ImageType;
  typedef typename ImageType::Pointer    ImagePointer;
  typedef typename ImageType::PixelType  ImageElementType;
  typedef typename ImageType::IndexType  ImageIndexType;
  typedef typename ImageType::RegionType ImageRegionType;
  typedef TImageMask                     MaskType;
  typedef typename MaskType::Pointer     MaskPointer;
  typedef typename MaskType::PixelType   MaskElementType;

  /** Bias field type definition. */
  typedef TBiasField BiasFieldType;

  /** Parameters type for optimizier (coefficients type for bias
   * field estimate). */
  typedef typename Superclass::ParametersType ParametersType;

  /** Not used, but expected by SingleValuedNonLinearOptimizer class. */
  typedef Superclass::DerivativeType DerivativeType;

  /** The cost value type. */
  typedef Superclass::MeasureType MeasureType;

  itkStaticConstMacro(SpaceDimension, unsigned int, 3);

  /** The type of the internal energy function. */
  typedef CompositeValleyFunction InternalEnergyFunction;

  /** The type of the sampling factors. */
  typedef unsigned int SamplingFactorType[SpaceDimension];

  /** Specify the input image. */
  itkSetObjectMacro(Image, ImageType);

  /** Specify the input mask image. */
  itkSetObjectMacro(Mask, MaskType);

  /** Set the image region which will be included for energy calculation. */
  itkSetMacro(Region, ImageRegionType);

  /** Sets the BiasField object. */
  void SetBiasField(BiasFieldType *bias)
  { m_BiasField = bias; }

  /** Sets the sampling factors of the energy function in each direction.
   *   Default is 1 in each dimension */
  void SetSamplingFactors(SamplingFactorType factor)
  {
    for ( unsigned int i = 0; i < SpaceDimension; i++ )
      {
      m_SamplingFactor[i] = factor[i];
      }
  }

  /** Get an energy value for the intensity difference between a pixel
   * and its corresponding bias. */
  double GetEnergy0(double diff)
  {
    return ( *m_InternalEnergyFunction )( diff );
  }

  /** Gets the total energy value of an image or a slice using the
   * given parameters. */
  virtual MeasureType GetValue(const ParametersType & parameters) const ITK_OVERRIDE;

  /** Dummy implementation to confirm to the SingleValuedCostFunction
   * interfaces. It is pure virtual in the superclass. */
  void GetDerivative( const ParametersType & itkNotUsed(parameters),
                      DerivativeType & itkNotUsed(derivative) ) const ITK_OVERRIDE
  {}

  /** Set Mean and Sigma for the normal distributions
   *  \warning This method MUST be called before any attempt to
   *   evaluate the Function because it instantiate the internal
   *   energy function. */
  void InitializeDistributions(Array< double > classMeans,
                               Array< double > classSigmas);

  virtual unsigned int GetNumberOfParameters(void) const ITK_OVERRIDE;

protected:
  /** Constructor. */
  MRIBiasEnergyFunction();

  /** Destructor. */
  virtual ~MRIBiasEnergyFunction() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MRIBiasEnergyFunction);

  /** Bias field object pointer. */
  BiasFieldType        *m_BiasField;

  /** Input image smart pointer. */
  ImagePointer m_Image;

  /** Input mask image smart pointer. */
  MaskPointer m_Mask;

  /** Region of interest. */
  ImageRegionType m_Region;

  /** Internal energy function object pointer. */
  InternalEnergyFunction *m_InternalEnergyFunction;

  /** Sampling factors */
  SamplingFactorType m_SamplingFactor;
};                                     // end of class

/** \class MRIBiasFieldCorrectionFilter
 * \brief Corrects 3D MRI bias field.
 *
 * This class is templated over the type of the input image (TInputImage)
 * and the type of the output image (TOutputImage).
 *
 * In MRI images, intensity inhomogenieties which are caused by
 * magnetic settings, patients' position, and other factors are not
 * unusual. The main purpose of this filter is to reduce such bias field.
 * To estimate the bias field, we use Legendre polynomials.
 * The 1+1 evolutionary optimizer searches for the best parameters of a
 * Legendre polynomial (bias field estimate) which minimizes the total
 * energy value of each image after bias field is eliminated.
 * The default Legendre polynomial degree is 3.
 *
 * The correction performs by default a multiplicative bias field correction
 * by first log-transforming the input image. This log transform only
 * works on images with grayscale values bigger than 0. The log-transform
 * can be disabled and the filter computes an additive bias field.
 *
 * There are three major processes in the whole bias correction scheme:
 * slab identification, inter-slice intensity correction, and
 * actual bias correction process.
 * Users can turn on and off each process within the whole bias
 * correction scheme using SetUsingSlabIdentification(bool, false by default),
 * SetUsingInterSliceIntensityCorrection(bool, true by default), and
 * SetUsingBiasFieldCorrection(bool, true by default) member function.
 *
 * Only the last process (the actual bias field correction) is implemented in a
 * multiresolution framework (without smoothing). Default is a standard level 2
 * multiresolution schedule (2 2 2 1 1 1).
 *
 * The bias field correction method was initially developed
 * and implemented by Martin Styner, Univ. of North Carolina at Chapel Hill,
 * and his colleagues.
 *
 * The multiresolution pyramid implementation is based on
 * itkMultiTesolutionPyramidImageFilter (without Gaussian smoothing)
 *
 * For more details. refer to the following articles.
 * "Parametric estimate of intensity inhomogeneities applied to MRI"
 * Martin Styner, Guido Gerig, Christian Brechbuehler, Gabor Szekely,
 * IEEE TRANSACTIONS ON MEDICAL IMAGING; 19(3), pp. 153-165, 2000,
 * (http://www.cs.unc.edu/~styner/docs/tmi00.pdf)
 *
 * "Evaluation of 2D/3D bias correction with 1+1ES-optimization"
 * Martin Styner, Prof. Dr. G. Gerig (IKT, BIWI, ETH Zuerich), TR-197
 * (http://www.cs.unc.edu/~styner/docs/StynerTR97.pdf)
 * \ingroup ITKBiasCorrection
 */
template< typename TInputImage, typename TOutputImage, typename TMaskImage >
class ITK_TEMPLATE_EXPORT MRIBiasFieldCorrectionFilter :
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef MRIBiasFieldCorrectionFilter                    Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(MRIBiasFieldCorrectionFilter, ImageToImageFilter);

  /** The dimension of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Input and output image related type definitions. */
  typedef TOutputImage                      OutputImageType;
  typedef typename TOutputImage::Pointer    OutputImagePointer;
  typedef typename TOutputImage::IndexType  OutputImageIndexType;
  typedef typename TOutputImage::PixelType  OutputImagePixelType;
  typedef typename TOutputImage::SizeType   OutputImageSizeType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::Pointer    InputImagePointer;
  typedef typename TInputImage::IndexType  InputImageIndexType;
  typedef typename TInputImage::PixelType  InputImagePixelType;
  typedef typename TInputImage::SizeType   InputImageSizeType;
  typedef typename TInputImage::RegionType InputImageRegionType;

  /** Mask image related type definitions. */
  typedef TMaskImage                         ImageMaskType;
  typedef typename ImageMaskType::Pointer    ImageMaskPointer;
  typedef typename ImageMaskType::RegionType ImageMaskRegionType;

  /** Internal (temporary) image related type definitions. */
  typedef Image< float, itkGetStaticConstMacro(ImageDimension) > InternalImageType;
  typedef typename InternalImageType::PixelType                  InternalImagePixelType;
  typedef typename InternalImageType::Pointer                    InternalImagePointer;
  typedef typename InternalImageType::RegionType                 InternalImageRegionType;

  /** Regions of the MRI slab identifier return. */
  typedef MRASlabIdentifier< InputImageType >                  MRASlabIdentifierType;
  typedef typename MRASlabIdentifierType::SlabRegionVectorType SlabRegionVectorType;
  typedef typename SlabRegionVectorType::iterator              SlabRegionVectorIteratorType;

  /** Bias field object type definition. */
  typedef MultivariateLegendrePolynomial BiasFieldType;

  /** Energy function type definition. */
  typedef MRIBiasEnergyFunction< InternalImageType,
                                 ImageMaskType,
                                 BiasFieldType >            EnergyFunctionType;
  typedef typename EnergyFunctionType::Pointer EnergyFunctionPointer;

  /** Normal variate Generator Type */
  typedef Statistics::NormalVariateGenerator NormalVariateGeneratorType;

  /** Optimizer type definition. */
  typedef OnePlusOneEvolutionaryOptimizer OptimizerType;

  /** ScheduleType typedef support. */
  typedef Array2D< unsigned int > ScheduleType;

  /** Set/Get the input mask image pointer.
   * Without this mask, this filter calculates the energy value using
   * all pixels in the input image. */
  void SetInputMask(ImageMaskType *inputMask);
  itkGetModifiableObjectMacro(InputMask, ImageMaskType);

  /** Get/Set the out mask image pointer.
   * Without this mask, this filter corrects every pixel in the input image. */
  void SetOutputMask(ImageMaskType *outputMask);
  itkGetModifiableObjectMacro(OutputMask, ImageMaskType);

#if defined ( ITK_FUTURE_LEGACY_REMOVE )
  /** If this value is true, the filter assumes the bias field is
   * multiplicative and internally uses log intensity values for
   * every calculation. */
  void IsBiasFieldMultiplicative(bool flag)
  { m_BiasFieldMultiplicative = flag; }
#endif // defined ( ITK_FUTURE_LEGACY_REMOVE )

  /** Set/Get the multiplicative nature of the filter's bias field: if
   * true, the filter assumes the bias field is multiplicative and
   * internally uses log intensity values for every calculation. */
  itkSetMacro(BiasFieldMultiplicative, bool);
  itkGetConstMacro(BiasFieldMultiplicative, bool);
  itkBooleanMacro(BiasFieldMultiplicative);

#if defined ( ITK_FUTURE_LEGACY_REMOVE )
  /** If the bias field is multiplicative, it returns true. */
  bool IsBiasFieldMultiplicative()
  { return m_BiasFieldMultiplicative; }
#endif // defined ( ITK_FUTURE_LEGACY_REMOVE )

  /** Set/Get the intensity correction flag. If the flag is true, inter-slice
   * intensity correction will be applied before bias field
   * correction (default value is true for 3D input images, and false for 2D
   * input images). */
  itkSetMacro(UsingInterSliceIntensityCorrection, bool);
  itkGetConstMacro(UsingInterSliceIntensityCorrection, bool);
  itkBooleanMacro(UsingInterSliceIntensityCorrection);

  /** Set/Get the slab correction flag. If the flag is true, inter-slice
   * intensity correction and bias field correction will be performed slab by
   * slab which is identified by the slab identifier (default value is false).
   * NOTE: if users want to slab identification, all the input image data
   * should be buffered. */
  itkSetMacro(UsingSlabIdentification, bool);
  itkGetConstMacro(UsingSlabIdentification, bool);
  itkBooleanMacro(UsingSlabIdentification);

  itkSetMacro(SlabBackgroundMinimumThreshold, InputImagePixelType);
  itkGetConstReferenceMacro(SlabBackgroundMinimumThreshold,
                            InputImagePixelType);

  itkSetMacro(SlabNumberOfSamples, unsigned int);
  itkGetConstReferenceMacro(SlabNumberOfSamples, unsigned int);

  itkSetMacro(SlabTolerance, double);
  itkGetConstReferenceMacro(SlabTolerance, double);

  /** Set/Get the bias correction flag. If the flag is true, bias field
   * correction runs.  This flag sounds odd. But if users want to use only
   * the inter-slice intensity correction without actual bias correction,
   * disabling bias field correction would be an useful option (default value
   * is true). */
  itkSetMacro(UsingBiasFieldCorrection, bool);
  itkGetConstMacro(UsingBiasFieldCorrection, bool);
  itkBooleanMacro(UsingBiasFieldCorrection);

  /** Set/Get the flag. If the flag is true, the output image (corrected image)
   * will be created when this filter is updated (default value is true). */
  itkSetMacro(GeneratingOutput, bool);
  itkGetConstMacro(GeneratingOutput, bool);
  itkBooleanMacro(GeneratingOutput);

  /** Sets the direction of slicing.
   * 0 - x axis, 1 - y axis, 2 - z axis */
  itkSetMacro(SlicingDirection, int);
  itkGetConstMacro(SlicingDirection, int);

  /** Set/Get the degree of the bias field estimate. */
  itkSetMacro(BiasFieldDegree, int);
  itkGetConstMacro(BiasFieldDegree, int);

  /** Sets the initial 3D bias field estimate coefficients that will be
   * used for correcting each slab. */
  void SetInitialBiasFieldCoefficients(const
                                       BiasFieldType::CoefficientArrayType
                                       & coefficients)
  { this->Modified(); m_BiasFieldCoefficients = coefficients; }

  /** Get the result bias field coefficients after the bias field
   * estimation (does not apply to the inter-slice intensity
   * correction). */
  BiasFieldType::CoefficientArrayType GetEstimatedBiasFieldCoefficients()
  { return m_EstimatedBiasFieldCoefficients; }

  /** Set the tissue class statistics for energy function initialization
   * If the numbers of elements in the means and the sigmas are not equal
   * it will throw an exception. */
  void SetTissueClassStatistics(const Array< double > & means,
                                const Array< double > & sigmas);

  /** Set/Get the maximum iteration termination condition parameter for the
   * bias field correction. */
  itkSetMacro(VolumeCorrectionMaximumIteration, int);
  itkGetConstMacro(VolumeCorrectionMaximumIteration, int);

  /** Set/Get the maximum iteration termination condition parameter for the
   * inter-slice intensity inhomogeneity correction. */
  itkSetMacro(InterSliceCorrectionMaximumIteration, int);
  itkGetConstMacro(InterSliceCorrectionMaximumIteration, int);

  /** Set/Get the initial search radius. */
  itkSetMacro(OptimizerInitialRadius, double);
  itkGetConstMacro(OptimizerInitialRadius, double);

  /** Set/Get the search radius grow factor. */
  itkSetMacro(OptimizerGrowthFactor, double);
  itkGetConstMacro(OptimizerGrowthFactor, double);

  /** Set/Get the search radius shrink factor. */

  itkSetMacro(OptimizerShrinkFactor, double);
  itkGetConstMacro(OptimizerShrinkFactor, double);

  /** Set the number of multi-resolution levels. The matrix containing the
   * schedule will be resized accordingly. The schedule is populated with
   * default values. At the coarsest (0) level, the shrink factors are set
   * 2^(nlevel - 1) for all dimension. These shrink factors are halved for
   * subsequent levels. The number of levels is clamped to a minimum value
   * of 1. All shrink factors are also clamped to a minimum value of 1. */
  void SetNumberOfLevels(unsigned int num);

  /** Get the number of multi-resolution levels. */
  itkGetConstMacro(NumberOfLevels, unsigned int);

  /** Set a multi-resolution schedule. The input schedule must have only
   * ImageDimension number of columns and NumberOfLevels number of rows. For
   * each dimension, the shrink factor must be non-increasing with respect to
   * subsequent levels. This function will clamp shrink factors to satisify
   * this condition. All shrink factors less than one will also be clamped
   * to the value of 1. */
  void SetSchedule(const ScheduleType & schedule);

  /** Get the multi-resolution schedule. */
  itkGetConstReferenceMacro(Schedule, ScheduleType);

  /** Set the starting shrink factor for the coarsest (0) resolution
   * level. The schedule is then populated with defaults values obtained by
   * halving the factors at the previous level. All shrink factors are
   * clamped to a minimum value of 1. */
  void SetStartingShrinkFactors(unsigned int factor);

  void SetStartingShrinkFactors(unsigned int *factors);

  /** Get the starting shrink factors. */
  const unsigned int * GetStartingShrinkFactors() const;

  /** Test if the schedule is downward divisible. This method returns true if
   * at every level, the shrink factors are divisble by the shrink factors at
   * the next level. */
  static bool IsScheduleDownwardDivisible(const ScheduleType & schedule);

  /** Initializes the energy function object and optimizer objects and
   * creates the internal image object copying the input image data to it.
   * Also, if the bias field is multiplicative, applies logarithm to pixel
   * intensity values, tissue classes' statistics values and the optimizer's
   * initial radius NOTE: If the tissue class statistics values (mean and
   * sigma values) then it will throw exception. */
  void Initialize();

  /** Optimizes the bias field only using the image data that are in
   * the specified region. */
  BiasFieldType EstimateBiasField(InputImageRegionType region,
                                  unsigned int degree,
                                  int maximumIteration);

  /** Correct the internal image using the bias field estimate
   * created by EstimateBiasField() member function and the internal image
   * data that are in the specified region. */
  void CorrectImage(BiasFieldType & bias,
                    InputImageRegionType region);

  /** Internally calls EstimateBiasField() and CorrectImage() member functions
   * for each slice to correct inter-slice intensity inhomogeneities. */
  void CorrectInterSliceIntensityInhomogeneity(InputImageRegionType region);

protected:
  MRIBiasFieldCorrectionFilter();
  virtual ~MRIBiasFieldCorrectionFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Checks if the mask image's dimensionality and size matches with
   * those of the input image. */
  bool CheckMaskImage(ImageMaskType *mask);

protected:
  /** Converts image data from source to target applying std::log(pixel + 1)
   * to all pixels. If the source pixel has negative value, it sets
   * the value of the corresponding pixel in the targe image as zero. */
  void Log1PImage(InternalImageType *source,
                  InternalImageType *target);

  /** Converts image data from source to target applying std::exp(pixel) - 1
   * to all pixels. */
  void ExpImage(InternalImageType *source,
                InternalImageType *target);

  /** Converts pixel type, and
   *  copies image data from source to target. */
  template< typename TSource, typename TTarget >
  void CopyAndConvertImage(const TSource *source,
                           TTarget *target,
                           typename TTarget::RegionType requestedRegion)
  {
    typedef ImageRegionConstIterator< TSource > SourceIterator;
    typedef ImageRegionIterator< TTarget >      TargetIterator;
    typedef typename TTarget::PixelType         TargetPixelType;

    SourceIterator s_iter(source, requestedRegion);
    TargetIterator t_iter(target, requestedRegion);

    s_iter.GoToBegin();
    t_iter.GoToBegin();
    while ( !s_iter.IsAtEnd() )
      {
      t_iter.Set( static_cast< TargetPixelType >( s_iter.Get() ) );
      ++s_iter;
      ++t_iter;
      }
  }

  /** Converts ImageRegion type (region) to DomainSize type (std::vector).
   * NOTE: if the size of the last dimension of the image region is one, then
   * the dimension of the resulting domain size will be one less than that of
   * he image region. */
  void GetBiasFieldSize(InputImageRegionType region,
                        BiasFieldType::DomainSizeType & domainSize);

  /** Find overlapping regions between the slab regions and the output image's
   * requested region. And then replace the original slab regions with
   * the resulting overlapping regions. */
  void AdjustSlabRegions(SlabRegionVectorType & slabs,
                         OutputImageRegionType requestedRegion);

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MRIBiasFieldCorrectionFilter);

  /** Energy function object pointer. */
  EnergyFunctionPointer m_EnergyFunction;

  /** Normal variate generator smart pointer. */
  NormalVariateGeneratorType::Pointer m_NormalVariateGenerator;

  /** Input mask image smart pointer. */
  ImageMaskPointer m_InputMask;

  /** Output mask image smart pointer. */
  ImageMaskPointer m_OutputMask;

  /** Internal image smart pointer. */
  InternalImagePointer m_InternalInput;

  /** Storage for the MRI slab identifier return. */
  SlabRegionVectorType m_Slabs;

  /** [0 - x, 1 - y, 2 - z]. */
  int m_SlicingDirection;

  /** Bias Field character if true, multiplicative.  if false, additive. */
  bool m_BiasFieldMultiplicative;

  /** operation selection flags. */
  bool m_UsingInterSliceIntensityCorrection;
  bool m_UsingSlabIdentification;
  bool m_UsingBiasFieldCorrection;
  bool m_GeneratingOutput;

  unsigned int        m_SlabNumberOfSamples;
  InputImagePixelType m_SlabBackgroundMinimumThreshold;
  double              m_SlabTolerance;

  /** The degree of the bias field estimate. */
  int m_BiasFieldDegree;

  /** The number of levels for the multires schedule. */
  unsigned int m_NumberOfLevels;

  /** The multires schedule */
  ScheduleType m_Schedule;

  /** Storage for the initial 3D bias field estimate coefficients that will be
   * used for correcting each slab. */
  BiasFieldType::CoefficientArrayType m_BiasFieldCoefficients;

  /** Storage for the resulting 3D bias field estimate coefficients
   * after optimization. */
  BiasFieldType::CoefficientArrayType m_EstimatedBiasFieldCoefficients;

  int m_VolumeCorrectionMaximumIteration;

  int m_InterSliceCorrectionMaximumIteration;

  /** Storage for the optimizer's initial search radius. */
  double m_OptimizerInitialRadius;

  /** Storage for the optimizer's search radius grow factor. */
  double m_OptimizerGrowthFactor;

  /** Storage for the optimizer's search radius shrink factor. */
  double m_OptimizerShrinkFactor;

  /** Storage for tissue classes' mean values. */
  Array< double > m_TissueClassMeans;

  /** Storage for tissue classes' variance values. */
  Array< double > m_TissueClassSigmas;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMRIBiasFieldCorrectionFilter.hxx"
#endif

#endif
