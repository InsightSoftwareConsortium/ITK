/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRIBiasFieldCorrectionFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:


 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkMRIBiasFieldCorrectionFilter_h
#define __itkMRIBiasFieldCorrectionFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

#include "itkMRASlabIdentifier.h"
#include "itkCompositeValleyFunction.h"
#include "itkMultivariateLegendrePolynomial.h"
#include "Statistics/itkFastRandomUnitNormalVariateGenerator.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"

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
  class MRIBiasEnergyFunction
  {
  public:
    /**
     * Image related type definitions
     */
    typedef TImage ImageType ;
    typedef TImageMask MaskType ;
    typedef typename ImageType::Pointer ImagePointer ;
    typedef typename MaskType::Pointer MaskPointer ;
    typedef typename ImageType::PixelType ImageElementType ;
    typedef typename MaskType::PixelType MaskElementType ;
    typedef typename ImageType::IndexType ImageIndexType ;
    typedef typename ImageType::RegionType ImageRegionType ;

    /**
     * bias field type definition
     */
    typedef TBiasField BiasFieldType ;

    /**
     * parameters type for optimizier (coefficients type for bias
     * field estimate)
     */
    typedef typename BiasFieldType::ParametersType ParametersType ;

    /**
     * not used, but expected by SingleValuedNonLinearOptimizer class
     */
    typedef double DerivativeType;

    /**
     * cost value type
     */
    typedef double MeasureType ;

    /**
     * not used, but expected by SingleValuedNonLinearOptimizer class
     */
    enum { SpaceDimension = 3 };

    typedef CompositeValleyFunction InternalEnergyFunction ;

    /**
     * constructor:
     */
    MRIBiasEnergyFunction(std::vector<double> classMeans, 
                          std::vector<double> classSigmas) ;

    virtual ~MRIBiasEnergyFunction() ;

    /**
     * sets input image
     */
    void SetImage(ImagePointer image) ;
  
    /**
     * sets input mask image
     */
    void SetMask(MaskPointer mask) ;

    /**
     * set the image region which will be included for energy calculation
     */
    void SetRegion(ImageRegionType region) ;

    /**
     * sets BiasField object
     */
    void SetBiasField(BiasFieldType* biasField) ;

    /**
     * gets an energy value for the intensity difference between a pixel
     * and its corresponding bias
     */
    double GetEnergy0(double diff) 
    { 
      return (*m_InternalEnergyFunction)(diff) ; 
    } 

    /**
     * gets the total energy value of an image or a slice using the
     * given parameters.
     */
    MeasureType GetValue(ParametersType parameters, 
                         MeasureType& ret) ;

  protected:

  private:
    /**
     * bias field object pointer
     */
    BiasFieldType* m_BiasField ;

    /**
     * input image smart pointer
     */
    ImagePointer m_Image ;

    /**
     * input mask image smart pointer
     */
    MaskPointer m_Mask ;

    /**
     * interest region
     */
    ImageRegionType m_Region ;

    /**
     * internal energy function object pointer
     */
    InternalEnergyFunction* m_InternalEnergyFunction ;
  } ; // end of class

  /**
   * \class MRIBiasFieldCorrectionFilter
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
   *  slab identification, inter-slice intensity correction, and 
   *  actual bias correction process.
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

  template <class TInputImage, class TOutputImage>
  class ITK_EXPORT MRIBiasFieldCorrectionFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
  {
  public:
    /**
     * Standard "Self" & Superclass typedef.
     */
    typedef MRIBiasFieldCorrectionFilter Self;
    typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;


    enum { ImageDimension = TOutputImage::ImageDimension };

    /**
     * input and output image related type definitions
     */
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

    /**
     * mask image related type definitions
     */
    typedef Image<unsigned char, ImageDimension> ImageMaskType ;
    typedef typename ImageMaskType::Pointer ImageMaskPointer ;
    typedef typename ImageMaskType::RegionType ImageMaskRegionType ;

    /**
     * internal (temporary) image related type defintions
     */
    typedef Image< float, ImageDimension > InternalImageType ;
    typedef typename InternalImageType::PixelType InternalImagePixelType ;
    typedef typename InternalImageType::Pointer InternalImagePointer ;
    typedef typename InternalImageType::RegionType InternalImageRegionType ;

    /**
     * Regions of the MRI slab identifier return 
     */
    typedef MRASlabIdentifier<InputImageType>::SlabRegionVectorType 
    SlabRegionVectorType ;

    /** 
     * Smart pointer typedef support b
     */
    typedef SmartPointer<Self> Pointer;
    typedef SmartPointer<const Self>  ConstPointer;
  
    /**
     * Run-time type information (and related methods)
     */
    itkTypeMacro(MRIBiasFieldCorrectionFilter, ImageToImageFilter);
  
    /**
     * Method for creation through the object factory.
     */
    itkNewMacro(Self);


    /**
     * Bias field object type defintion
     */
    typedef MultivariateLegendrePolynomial BiasField ;

    /**
     * Energy function type defintion
     */
    typedef MRIBiasEnergyFunction<TInputImage, ImageMaskType, BiasField> 
    EnergyFunction ;

    /**
     * Optimizer type definition
     */
    typedef OnePlusOneEvolutionaryOptimizer<EnergyFunction, 
      FastRandomUnitNormalVariateGenerator> OptimizerType ;

    /**
     * Sets the input mask image pointer
     * Without this mask, this filter calculates the energy value using
     * all pixels in the input image. 
     */
    void SetInputMask(ImageMaskPointer inputMask) ;

    /**
     * Gets the input mask image pointer
     */
    ImageMaskPointer GetInputMask()
    { return m_InputMask ; }

    /**
     * Sets the out mask image pointer
     * Without this mask, this filter corrects every pixel in the input image.
     */
    void SetOutputMask(ImageMaskPointer outputMask) ;
    
    /**
     * Gets the output mask image pointer
     */ 
    ImageMaskPointer GetOutputMask()
    { return m_OutputMask ; }

    /**
     * If you set this true, this filter assumes the bias field is
     * multiplicative and internally uses log intensity values for
     * every calculation. 
     */
    void IsBiasFieldMultiplicative(bool flag) 
    { m_BiasMultiplicative = flag ; }

    /**
     * If the bias field is multiplicative, it returns true.
     */
    bool IsBiasFieldMultiplicative() 
    { return m_BiasMultiplicative ; }

    /**
     * Sets the flag. if the flag is true, inter-slice intensity
     * correction will be applied before bias field
     * correction. default - true (3D input image), false (2D input image).
     */
    void SetUsingInterSliceIntensityCorrection(bool flag)
    { m_UsingInterSliceIntensityCorrection = flag ; }

    /**
     * Sets the flag. If the flag is true, inter-slice intensity correction
     * and bias field correction will be performed slab by slab which
     * is identified by the slab identifier. default - false
     *
     * NOTE:
     * if users want to slab identification, all the input image data should
     * be buffered.
     */
    void SetUsingSlabIdentification(bool flag)
    { m_UsingSlabIdentification = flag ; }

    /**
     * Sets the flag. If the flag is true, bias field correction runs.
     * This flag sounds odd. But if users want to use only the inter-slice
     * intensity correction without actual bias correction, disabling
     * bias field correction would be an useful option. default - true.
     */
    void SetUsingBiasFieldCorrection(bool flag)
    { m_UsingBiasFieldCorrection = flag ; }


    /**
     * Sets the flag, If the flag is true, the output image (corrected image)
     * will be created when this filter is updated. default - true
     */
    void SetGeneratingOutput(bool flag)
    { m_GeneratingOutput = flag ; }

    // slab identifier realted members

    /**
     * Sets the direction of slicing
     * 0 - x axis, 1 - y axis, 2 - z axis
     */
    void SetSlicingDirection(int dimension)
    { m_SlicingDirection = dimension ; }
    
    // bias field related members

    /**
     * Sets the degree of the bias field estimate
     */
    void SetBiasFieldDegree(int degree) 
    { m_BiasFieldDegree = degree ; }

    /**
     * Gets the degree of the bias field estimate
     */
    int GetBiasFieldDegree() 
    { return m_BiasFieldDegree ; } 

    /**
     * Gets the number of dimensions of the bias field estimate
     */
    int GetBiasFieldDimension() 
    { return m_BiasFieldDimension ; } 

    /**
     * Gets the number of the bias field coefficients
     */
    int GetNoOfBiasFieldCoefficients()
    { return m_NoOfBiasFieldCoefficients ; }

    BiasField::DomainSizeType GetBiasFieldDomainSize()
    { return m_BiasFieldDomainSize ; }

    /**
     * Sets the initial 3D bias field estimate coefficients that will be
     * used for correcting each slab.
     */
    void SetInitialBiasFieldCoefficients(BiasField::CoefficientVector 
                                         coefficients)
    { m_BiasFieldCoefficients = coefficients ; }


    /**
     * Get the result bias field coefficients after the bias field
     * estimation (does not apply to the inter-slice intensity
     * correction)
     */
    BiasField::CoefficientVector GetEstimatedBiasFieldCoefficients() 
    { return m_EstimatedBiasFieldCoefficients ; }

    // ======== energy function related members ========

    /**
     * Set the tissue class statistics for energy function initialization
     * If the numbers of elements in the means and the sigmas are not equal
     * it will throw exception   
     */
    void SetTissueClassStatistics(std::vector<double> means, 
                                  std::vector<double> sigmas) 
      throw (ExceptionObject) ;

    // ======== optimizer related members ===============

    /**
     * Sets the maximum iteration termination condition parameter
     */
    void SetOptimizerMaximumIteration(int max) 
    { m_OptimizerMaximumIteration = max ; }

    int GetOptimizerMaximumIteration()
    { return m_OptimizerMaximumIteration ; } 

    /**
     * Sets the initial search radius 
     */
    void SetOptimizerInitialRadius(double initRadius) 
    { m_OptimizerInitialRadius = initRadius ; }

    double GetOptimizerInitialRadius()
    { return m_OptimizerInitialRadius ; }

    /**
     * Sets the search radius grow factor
     */
    void SetOptimizerGrowFactor(double grow) 
    { m_OptimizerGrowFactor = grow ; }

    double GetOptimizerGrowFactor()
    { return m_OptimizerGrowFactor ; }

    /**
     * Sets the search radius shrink factor
     */
    void SetOptimizerShrinkFactor(double shrink) 
    { m_OptimizerShrinkFactor = shrink ; }

    double GetOptimizerShrinkFactor()
    { return m_OptimizerShrinkFactor ; }

    /**
     * Initializes the energy function object and optimizer objects and 
     * creates the internal image object copying the input image data to it.
     * Also, if the bias field is multiplicative, 
     * applies logarithm to pixel intensity values, tissue classes' statistics
     * values and the optimizer's initial radius
     * NOTE:
     * If the tissue class statistics values (mean and sigma values) then it
     * will throw exception.
     */
    void Initialize() throw (ExceptionObject) ;

    /**
     * Optimizes the bias field only using the image data that are in 
     * the specified region  
     */
    void EstimateBiasField(BiasField* bias,
                           InputImageRegionType region) ;

    /**
     * Correct the internal image using the bias field estimate 
     * created by EstimateBiasField() member function and the internal image
     * data that are in the specified region  
     */
    void CorrectImage(BiasField* bias, 
                      InputImageRegionType region) ;

    /**
     * Internally calls EstimateBiasField() and CorrectImage() member functions
     * for each slice to correct inter-slice intensity inhomogeneities.
     */
    void CorrectInterSliceIntensityInhomogeneity(InputImageRegionType region) ;

    /**
     * Print out progess to the standard output
     */
    void SetVerboseMode(bool flag) 
    { m_VerboseMode = flag ; }

  protected:

    /**
     * Constructor
     */ 
    MRIBiasFieldCorrectionFilter() ;

    /**
     * Destructor: deletes the energy function object
     */
    virtual ~MRIBiasFieldCorrectionFilter() ;
    MRIBiasFieldCorrectionFilter(const Self&) {}
    void operator=(const Self&) {}

    /**
     * Checks if the mask image's dimensionality and size matches with
     * those of the input image
     */
    bool CheckMaskImage(ImageMaskPointer mask) ;

protected:
    /**
     * Converts image data from source to target applying log(pixel + 1)
     * to all pixels. If the source pixel has negative value, it sets 
     * the value of the corresponding pixel in the targe image as zero. 
     */
    void Log1PImage(InternalImagePointer source, 
                  InternalImagePointer target) ;

    /**
     * Converts image data from source to target applying exp(pixel) - 1
     * to all pixels. 
     */
    void ExpImage(InternalImagePointer source, 
                  InternalImagePointer target) ;

    /**
     * Converts ImageRegion type (region) to 
     * DomainSize type (std::vector)
     * NOTE:
     * if the size of the last dimension of the image region is one,
     * then the dimension of the resulting domain size will be one less than
     * that of he image region 
     */
    void GetBiasFieldSize(InputImageRegionType region,
                          BiasField::DomainSizeType& domainSize) ;
    
    
    /**
     * Find overlapping regions between the slab regions and the output image's
     * requested region. And then replace the original slab regions with
     * the resulting overlapping regions.
     */
    void AdjustSlabRegions(SlabRegionVectorType& slabs, 
                           OutputImageRegionType requestedRegion) ;

    void GenerateData() ;

  private:
    
    /**
     * Engery function object pointer
     */
    EnergyFunction* m_EnergyFunction ;

    /**
     * Optimizer object smart pointer 
     */
    OptimizerType::Pointer m_Optimizer ;
    
    /**
     * Input mask image smart pointer
     */
    ImageMaskPointer m_InputMask ;

    /**
     * Output mask image smart pointer 
     */
    ImageMaskPointer m_OutputMask ;

    /**
     * Internal image smart pointer
     */
    InternalImagePointer m_InternalInput ;

    /**
     * Storage for the MRI slab identifier return 
     */
    SlabRegionVectorType m_Slabs ;
    
    /**
     * [0 - x, 1 - y, 2 - z]
     */
    int m_SlicingDirection ;

    /**
     * Bias Field character
     * if true, multiplicative.
     * if false, additive.
     */
    bool m_BiasMultiplicative ;

    /**
     * operation selection flags
     */
    bool m_UsingInterSliceIntensityCorrection ;
    bool m_UsingSlabIdentification ;
    bool m_UsingBiasFieldCorrection ;
    bool m_GeneratingOutput ;

    /**
     * The degree of the bias field estimate
     */
    int m_BiasFieldDegree ;

    /**
     * The number of dimensions of the bias field estimate after optimization
     */
    int m_BiasFieldDimension ;

    /**
     * The number of coefficients of the bias field after optimization
     */
    int m_NoOfBiasFieldCoefficients ;

    /**
     * Storage for the bias field domain size
     */
    BiasField::DomainSizeType m_BiasFieldDomainSize ;

    /**
     * Storage for the initial 3D bias field estimate coefficients that will be
     * used for correcting each slab.
     */
    BiasField::CoefficientVector m_BiasFieldCoefficients ;

    /**
     * Storage for the resulting 3D bias field estimate coefficients 
     * after optimization
     */
    BiasField::CoefficientVector m_EstimatedBiasFieldCoefficients ;

    /**
     * Storage for the optimizer's maximum iteration number
     */
    int m_OptimizerMaximumIteration ;

    /**
     * Storage for the optimizer's initial search radius
     */
    double m_OptimizerInitialRadius ;

    /**
     * Storage for the optimizer's search radius grow factor
     */
    double m_OptimizerGrowFactor ;

    /**
     * Storage for the optimizer's search radius shrink factor
     */
    double m_OptimizerShrinkFactor ;

    /**
     * Storage for tissue classes' mean values
     */
    std::vector<double> m_TissueClassMeans ;

    /**
     * Storage for tissue classes' variance values
     */
    std::vector<double> m_TissueClassSigmas ;

    /**
     * if true, print out progress to the standard output. default - false
     */
    bool m_VerboseMode ;
  };

  
  // ==================================

  /**
   * Allocates memory to the target image object, converts pixel type
   * , and copies image data from source to target.
   */
  template<class TSource, class TTarget>
  void CopyAndConvertImage(typename TSource::Pointer source,
                           typename TTarget::Pointer target,
                           typename TTarget::RegionType requestedRegion)
  {
    typedef ImageRegionIterator<TSource> SourceIterator ;
    typedef ImageRegionIterator<TTarget> TargetIterator ;
    typedef typename TTarget::PixelType TargetPixelType ;

    target->SetRequestedRegion(requestedRegion) ;
    target->SetBufferedRegion(requestedRegion) ;
    target->Allocate() ;

    SourceIterator s_iter(source, requestedRegion) ;
    TargetIterator t_iter(target, requestedRegion) ;
    
    while (!s_iter.IsAtEnd())
      {
        t_iter.Set((TargetPixelType) s_iter.Get()) ;
        ++s_iter ;
        ++t_iter ;
      }
  }

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMRIBiasFieldCorrectionFilter.txx"
#endif

#endif
