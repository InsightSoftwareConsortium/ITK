/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRIBiasFieldCorrectionFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMRIBiasFieldCorrectionFilter_h
#define __itkMRIBiasFieldCorrectionFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

#include "itkMRASlabIdentifier.h"
#include "itkCompositeValleyFunction.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "Statistics/itkNormalVariateGenerator.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkArray.h"
#include "itkImageRegionConstIterator.h"


namespace itk
{
/** \class MRIBiasEnergyFunction
 * \brief a cost function for optimization
 *
 * This is a wrapping class which provides interfaces between images,
 * the bias field, the internal energy function (CompositeValleyFunction), 
 * and the Optimizer.
 *
 * This class is templated over the type of the input image (TImage), 
 * the image mask (which tells which pixels in the input image should be
 * included for energy value calculation), and the bias field (TBiasField).  
 */
template<class TImage, class TImageMask, class TBiasField>
class MRIBiasEnergyFunction : public SingleValuedCostFunction
{
public:
  /** Standard class typedefs. */
  typedef MRIBiasEnergyFunction        Self;
  typedef SingleValuedCostFunction     Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( SingleValuedCostFunction, CostFunction );


  /** Image related type definitions. */
  typedef TImage ImageType ;
  typedef TImageMask MaskType ;
  typedef typename ImageType::Pointer ImagePointer ;
  typedef typename MaskType::Pointer MaskPointer ;
  typedef typename ImageType::PixelType ImageElementType ;
  typedef typename MaskType::PixelType MaskElementType ;
  typedef typename ImageType::IndexType ImageIndexType ;
  typedef typename ImageType::RegionType ImageRegionType ;

  /** Bias field type definition. */
  typedef TBiasField                        BiasFieldType;

  /** Parameters type for optimizier (coefficients type for bias
   * field estimate). */
  typedef typename Superclass::ParametersType    ParametersType ;

  /** Not used, but expected by SingleValuedNonLinearOptimizer class. */
  typedef Superclass::DerivativeType    DerivativeType;

  /** The cost value type. */
  typedef Superclass::MeasureType       MeasureType;

  /** Not used, but expected by SingleValuedNonLinearOptimizer class. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);

  /** The type of the internal energy function. */
  typedef CompositeValleyFunction InternalEnergyFunction ;

  /** Specify the input image. */
  itkSetObjectMacro( Image, ImageType );

  /** Specify the input mask image. */
  itkSetObjectMacro( Mask, MaskType );

  /** Set the image region which will be included for energy calculation. */
  itkSetMacro( Region, ImageRegionType );

  /** Sets the BiasField object. */
  itkSetObjectMacro( BiasField, BiasFieldType );

  /** Get an energy value for the intensity difference between a pixel
   * and its corresponding bias. */
  double GetEnergy0(double diff) 
  { return (*m_InternalEnergyFunction)(diff); } 

  /** Gets the total energy value of an image or a slice using the
   * given parameters. */
  MeasureType GetValue(const ParametersType & parameters ) const ;

  /** Dummy implementation to confirm to the SingleValuedCostFunction 
   * interfaces. It is pure virtual in the superclass */
  void GetDerivative( const ParametersType & itkNotUsed(parameters),
                      DerivativeType & itkNotUsed(derivative) ) const 
  {  }

  /** Set Mean and Sigma for the normal distributions 
   *  \warning This method MUST be called before any attemp to 
   *   evaluate the Function because it instantiate the internal
   *   energy function                                     */
  void InitializeDistributions( Array<double> classMeans, 
                                Array<double> classSigmas );

  unsigned int GetNumberOfParameters(void) const;

private:
  /** Bias field object pointer. */
  BiasFieldType        * m_BiasField ;

  /** Input image smart pointer. */
  ImagePointer           m_Image ;

  /** Input mask image smart pointer. */
  MaskPointer            m_Mask ;

  /** Region of interest. */
  ImageRegionType        m_Region ;

  /** Internal energy function object pointer. */
  InternalEnergyFunction* m_InternalEnergyFunction ;

protected:
  /** Constructor: */
  MRIBiasEnergyFunction();

  /** Destructor: */
  virtual ~MRIBiasEnergyFunction();


private:

  MRIBiasEnergyFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

} ; // end of class



/** \class MRIBiasFieldCorrectionFilter
 * \brief corrects 3D MRI bias field 
 *
 * This class is templated over the type of the input image (TInputImage) 
 * and the type of the output image (TOutputImage). 
 * 
 * In MRI images, intensity inhomogenieties which are caused by
 * magnetic settings, patients' postion, and other factors are not
 * unusual. The main purpose of this filter is to reduce such bias field.
 * To estimate the bias field, we use Legendre
 * polynomials. The 1+1 evolutionary optimizer searches for the best
 * paramters of a Legendre polynomial (bias field estimate) which
 * minimizes the total energy value of each image after bias field
 * is eleminated.
 *
 * There are three major processes in the whole bias correction scheme: 
 * slab identification, inter-slice intensity correction, and 
 * actual bias correction process.
 * Users can turn on and off each process within the whole bias
 * correction scheme using SetUsingSlabIdentification(bool),
 * SetUsingInterSliceIntensityCorrection(bool), and 
 * SetUsingBiasFieldCorrection(bool) member function.
 * 
 * The bias field correction method was initially developed 
 * and implemented by Martin Styner, Univ. of North Carolina at Chapel Hill,
 * and his colleagues.
 *
 * For more details. refer to the following articles.
 * "Parametric estimate of intensity inhomogeneities applied to MRI" 
 * Martin Styner, G. Gerig, Christian Brechbuehler, Gabor Szekely,  
 * IEEE TRANSACTIONS ON MEDICAL IMAGING; 19(3), pp. 153-165, 2000, 
 * (http://www.cs.unc.edu/~styner/docs/tmi00.pdf)
 * 
 * "Evaluation of 2D/3D bias correction with 1+1ES-optimization" 
 * Martin Styner, Prof. Dr. G. Gerig (IKT, BIWI, ETH Zuerich), TR-197
 * (http://www.cs.unc.edu/~styner/docs/StynerTR97.pdf)
 */
template <class TInputImage, class TOutputImage, class TMaskImage>
class ITK_EXPORT MRIBiasFieldCorrectionFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /** Standard class typedefs. */
  typedef MRIBiasFieldCorrectionFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(MRIBiasFieldCorrectionFilter, ImageToImageFilter);

  /** The dimension of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Input and output image related type definitions. */
  typedef TOutputImage OutputImageType ;
  typedef TInputImage InputImageType ;
  typedef typename TOutputImage::Pointer OutputImagePointer ;
  typedef typename TOutputImage::IndexType OutputImageIndexType ;
  typedef typename TOutputImage::PixelType OutputImagePixelType ;
  typedef typename TOutputImage::SizeType OutputImageSizeType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::Pointer InputImagePointer ;
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TInputImage::PixelType InputImagePixelType;
  typedef typename TInputImage::SizeType InputImageSizeType;
  typedef typename TInputImage::RegionType InputImageRegionType;

  /** Mask image related type definitions. */
  typedef TMaskImage ImageMaskType ;
  typedef typename ImageMaskType::Pointer ImageMaskPointer ;
  typedef typename ImageMaskType::RegionType ImageMaskRegionType ;

  /** Internal (temporary) image related type definitions. */
  typedef Image< float, itkGetStaticConstMacro(ImageDimension) > InternalImageType ;
  typedef typename InternalImageType::PixelType InternalImagePixelType ;
  typedef typename InternalImageType::Pointer InternalImagePointer ;
  typedef typename InternalImageType::RegionType InternalImageRegionType ;

  /** Regions of the MRI slab identifier return. */
  typedef MRASlabIdentifier<InputImageType>  MRASlabIdentifierType;
  typedef typename MRASlabIdentifierType::SlabRegionVectorType 
  SlabRegionVectorType;
  typedef typename SlabRegionVectorType::iterator SlabRegionVectorIteratorType;

  /** Bias field object type defintion. */
  typedef MultivariateLegendrePolynomial                  BiasFieldType;

  /** Energy function type defintion. */
  typedef MRIBiasEnergyFunction<InternalImageType, ImageMaskType, BiasFieldType> 
  EnergyFunctionType;
  typedef typename EnergyFunctionType::Pointer            EnergyFunctionPointer;

  /** Normal variate Generator Type */
  typedef Statistics::NormalVariateGenerator NormalVariateGeneratorType ;

  /** Optimizer type definition. */
  typedef OnePlusOneEvolutionaryOptimizer OptimizerType ;

  /** Set/Get the input mask image pointer
   * Without this mask, this filter calculates the energy value using
   * all pixels in the input image.  */
  void SetInputMask(ImageMaskPointer inputMask);
  itkGetObjectMacro( InputMask, ImageMaskType );

  /** Sets the out mask image pointer.
   * Without this mask, this filter corrects every pixel in the input image. */
  void SetOutputMask(ImageMaskPointer outputMask) ;

  /** Gets the output mask image pointer. */ 
  itkGetObjectMacro( OutputMask, ImageMaskType );

  /** If you set this true, this filter assumes the bias field is
   * multiplicative and internally uses log intensity values for
   * every calculation.  */
  void IsBiasFieldMultiplicative(bool flag) 
  { m_BiasMultiplicative = flag ; }

  /** If the bias field is multiplicative, it returns true. */
  bool IsBiasFieldMultiplicative() 
  { return m_BiasMultiplicative ; }

  /** Sets the intensity correction flag. if the flag is true, inter-slice
   * intensity correction will be applied before bias field
   * correction. default - true (3D input image), false (2D input image). */
  itkSetMacro( UsingInterSliceIntensityCorrection, bool );

  /** Sets the slab correction flag. If the flag is true, inter-slice
   * intensity correction and bias field correction will be performed slab by
   * slab which is identified by the slab identifier. default - false
   * NOTE: if users want to slab identification, all the input image data
   * should be buffered. */
  itkSetMacro( UsingSlabIdentification, bool );

  /** Set the bias correction flag. If the flag is true, bias field
   * correction runs.  This flag sounds odd. But if users want to use only
   * the inter-slice intensity correction without actual bias correction,
   * disabling bias field correction would be an useful option. default -
   * true. */
  itkSetMacro( UsingBiasFieldCorrection, bool );

  /** Sets the flag, If the flag is true, the output image (corrected image)
   * will be created when this filter is updated. default - true */
  itkSetMacro( GeneratingOutput, bool );

  /** Sets the direction of slicing.
   * 0 - x axis, 1 - y axis, 2 - z axis */
  itkSetMacro( SlicingDirection , int );

  /** Set/Get the degree of the bias field estimate. */
  itkSetMacro( BiasFieldDegree, int );
  itkGetMacro( BiasFieldDegree, int );

  /** Gets the number of the bias field coefficients. */
  itkGetMacro( NoOfBiasFieldCoefficients, int );

  /** Get the bias field domain size. */
  itkGetMacro( BiasFieldDomainSize, BiasFieldType::DomainSizeType );

  /** Sets the initial 3D bias field estimate coefficients that will be
   * used for correcting each slab. */
  void SetInitialBiasFieldCoefficients
  (const BiasFieldType::CoefficientArrayType &coefficients)
  { m_BiasFieldCoefficients = coefficients ; }

  /** Get the result bias field coefficients after the bias field
   * estimation (does not apply to the inter-slice intensity
   * correction) */
  itkGetMacro( EstimatedBiasFieldCoefficients, BiasFieldType::CoefficientArrayType );

  /** Set the tissue class statistics for energy function initialization
   * If the numbers of elements in the means and the sigmas are not equal
   * it will throw exception    */
  void SetTissueClassStatistics(const Array<double> & means, 
                                const Array<double> & sigmas) 
    throw (ExceptionObject) ;

  /** Set/Get the maximum iteration termination condition parameter. */
  itkSetMacro( VolumeCorrectionMaximumIteration, int );
  itkGetMacro( VolumeCorrectionMaximumIteration, int );
  itkSetMacro( InterSliceCorrectionMaximumIteration, int );
  itkGetMacro( InterSliceCorrectionMaximumIteration, int );

  /** Set/Get the initial search radius. */
  void SetOptimizerInitialRadius(double initRadius) 
  { m_OptimizerInitialRadius = initRadius ; }
  double GetOptimizerInitialRadius()
  { return m_OptimizerInitialRadius ; }

  /** Set/Get the search radius grow factor. */
  itkSetMacro( OptimizerGrowthFactor, double );
  itkGetMacro( OptimizerGrowthFactor, double );

  /** Set/Get the search radius shrink factor. */
  
  itkSetMacro( OptimizerShrinkFactor, double );
  itkGetMacro( OptimizerShrinkFactor, double );

  /** Initializes the energy function object and optimizer objects and
   * creates the internal image object copying the input image data to it.
   * Also, if the bias field is multiplicative, applies logarithm to pixel
   * intensity values, tissue classes' statistics values and the optimizer's
   * initial radius NOTE: If the tissue class statistics values (mean and
   * sigma values) then it will throw exception. */
  void Initialize() throw (ExceptionObject) ;

  /** Optimizes the bias field only using the image data that are in 
   * the specified region. */
  void EstimateBiasField(BiasFieldType* bias,
                         InputImageRegionType region) ;

  /** Correct the internal image using the bias field estimate 
   * created by EstimateBiasField() member function and the internal image
   * data that are in the specified region. */
  void CorrectImage(BiasFieldType* bias, 
                    InputImageRegionType region) ;

  /** Internally calls EstimateBiasField() and CorrectImage() member functions
   * for each slice to correct inter-slice intensity inhomogeneities. */
  void CorrectInterSliceIntensityInhomogeneity(InputImageRegionType region) ;

protected:
  MRIBiasFieldCorrectionFilter() ;
  virtual ~MRIBiasFieldCorrectionFilter() ;

  /** Checks if the mask image's dimensionality and size matches with
   * those of the input image */
  bool CheckMaskImage(ImageMaskPointer mask) ;

protected:
  /** Converts image data from source to target applying log(pixel + 1)
   * to all pixels. If the source pixel has negative value, it sets 
   * the value of the corresponding pixel in the targe image as zero.  */
  void Log1PImage(InternalImagePointer source, 
                  InternalImagePointer target) ;

  /** Converts image data from source to target applying exp(pixel) - 1
   * to all pixels.  */
  void ExpImage(InternalImagePointer source, 
                InternalImagePointer target) ;

  /** Converts ImageRegion type (region) to DomainSize type (std::vector)
   * NOTE: if the size of the last dimension of the image region is one, then
   * the dimension of the resulting domain size will be one less than that of
   * he image region */
  void GetBiasFieldSize(InputImageRegionType region,
                        BiasFieldType::DomainSizeType& domainSize) ;

  /** Find overlapping regions between the slab regions and the output image's
   * requested region. And then replace the original slab regions with
   * the resulting overlapping regions. */
  void AdjustSlabRegions(SlabRegionVectorType& slabs, 
                         OutputImageRegionType requestedRegion) ;

  void GenerateData() ;

private:
  MRIBiasFieldCorrectionFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Energy function object pointer. */
  EnergyFunctionPointer  m_EnergyFunction ;

  /** Normal variate generator smart pointer */
  NormalVariateGeneratorType::Pointer m_NormalVariateGenerator ;

  /** Optimizer object smart pointer.  */
  OptimizerType::Pointer m_Optimizer ;

  /** Input mask image smart pointer. */
  ImageMaskPointer m_InputMask ;

  /** Output mask image smart pointer. */
  ImageMaskPointer m_OutputMask ;

  /** Internal image smart pointer. */
  InternalImagePointer m_InternalInput ;

  /** Storage for the MRI slab identifier return. */
  SlabRegionVectorType m_Slabs ;

  /** [0 - x, 1 - y, 2 - z]. */
  int m_SlicingDirection ;

  /** Bias Field character if true, multiplicative.  if false, additive. */
  bool m_BiasMultiplicative ;

  /** operation selection flags. */
  bool m_UsingInterSliceIntensityCorrection ;
  bool m_UsingSlabIdentification ;
  bool m_UsingBiasFieldCorrection ;
  bool m_GeneratingOutput ;

  /** The degree of the bias field estimate. */
  int m_BiasFieldDegree ;

  /** The number of dimensions of the bias field estimate after optimization.*/
  int m_BiasFieldDimension ;

  /** The number of coefficients of the bias field after optimization. */
  int m_NoOfBiasFieldCoefficients ;

  /** Storage for the bias field domain size. */
  BiasFieldType::DomainSizeType m_BiasFieldDomainSize ;

  /** Storage for the initial 3D bias field estimate coefficients that will be
   * used for correcting each slab. */
  BiasFieldType::CoefficientArrayType m_BiasFieldCoefficients ;

  /** Storage for the resulting 3D bias field estimate coefficients 
   * after optimization. */
  BiasFieldType::CoefficientArrayType m_EstimatedBiasFieldCoefficients ;

  /** Storage for the optimizer's maximum iteration number. */
  int m_VolumeCorrectionMaximumIteration ;
  
  /** Storage for the optimizer's maximum iteration number. */
  int m_InterSliceCorrectionMaximumIteration ;

  /** Storage for the optimizer's initial search radius. */
  double m_OptimizerInitialRadius ;

  /** Storage for the optimizer's search radius grow factor. */
  double m_OptimizerGrowthFactor ;

  /** Storage for the optimizer's search radius shrink factor. */
  double m_OptimizerShrinkFactor ;

  /** Storage for tissue classes' mean values. */
  Array<double> m_TissueClassMeans ;

  /** Storage for tissue classes' variance values. */
  Array<double> m_TissueClassSigmas ;
};

  
// ==================================

/** Allocates memory to the target image object, converts pixel type, and
 *  copies image data from source to target. */
template<class TSource, class TTarget>
void CopyAndConvertImage(const TSource * sourceInp,
                         TTarget * targetInp,
                         typename TTarget::RegionType requestedRegion)
{
  typedef ImageRegionConstIterator<TSource> SourceIterator ;
  typedef ImageRegionIterator<TTarget> TargetIterator ;
  typedef typename TTarget::PixelType  TargetPixelType ;

  typename TSource::ConstPointer source( sourceInp );
  typename TTarget::Pointer      target( targetInp );

  target->SetRequestedRegion(requestedRegion) ;
  target->SetBufferedRegion(requestedRegion) ;
  target->Allocate() ;

  SourceIterator s_iter(source, requestedRegion) ;
  TargetIterator t_iter(target, requestedRegion) ;

  s_iter.GoToBegin() ;
  t_iter.GoToBegin() ;
  while (!s_iter.IsAtEnd())
    {
    t_iter.Set(static_cast<TargetPixelType>( s_iter.Get() ) ) ;
    ++s_iter ;
    ++t_iter ;
    }
}

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMRIBiasFieldCorrectionFilter.txx"
#endif

#endif
