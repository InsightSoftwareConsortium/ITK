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
#ifndef itkSTAPLEImageFilter_h
#define itkSTAPLEImageFilter_h

#include "itkImageToImageFilter.h"
#include <vector>

namespace itk
{
/** \class STAPLEImageFilter
 *
 * \brief The STAPLE filter implements the Simultaneous Truth and Performance
 * Level Estimation algorithm for generating ground truth volumes from a set of
 * binary expert segmentations.
 *
 * The STAPLE algorithm treats segmentation as a pixelwise classification,
 * which leads to an averaging scheme that accounts for systematic biases in
 * the behavior of experts in order to generate a fuzzy ground truth volume and
 * simultaneous accuracy assessment of each expert. The ground truth volumes
 * produced by this filter are floating point volumes of values between zero
 * and one that indicate probability of each pixel being in the object targeted
 * by the segmentation.
 *
 * The STAPLE algorithm is described in
 *
 * S. Warfield, K. Zou, W. Wells, "Validation of image segmentation and expert
 * quality with an expectation-maximization algorithm" in MICCAI 2002: Fifth
 * International Conference on Medical Image Computing and Computer-Assisted
 * Intervention, Springer-Verlag, Heidelberg, Germany, 2002, pp. 298-306
 *
 * \par INPUTS
 * Input volumes to the STAPLE filter must be binary segmentations of an image,
 * that is, there must be a single foreground value that represents positively
 * classified pixels (pixels that are considered to belong inside the
 * segmentation).  Any number of background pixel values may be present in the
 * input images.  You can, for example, input volumes with many different
 * labels as long as the structure you are interested in creating ground truth
 * for is consistently labeled among all input volumes.  Pixel type of the
 * input volumes does not matter.  Specify the label value for positively
 * classified pixels using SetForegroundValue.  All other labels will be
 * considered to be negatively classified pixels (background).
 *
 * Input volumes must all contain the same size RequestedRegions.
 *
 * \par OUTPUTS
 * The STAPLE filter produces a single output volume with a range of floating
 * point values from zero to one. IT IS VERY IMPORTANT TO INSTANTIATE THIS
 * FILTER WITH A FLOATING POINT OUTPUT TYPE (floats or doubles).  You may
 * threshold the output above some probability threshold if you wish to produce
 * a binary ground truth.
 *
 * \par PARAMETERS
 * The STAPLE algorithm requires a number of inputs.  You may specify any
 * number of input volumes using the SetInput(i, p_i) method, where i ranges
 * from zero to N-1, N is the total number of input segmentations, and p_i is
 * the SmartPointer to the i-th segmentation.
 *
 * The SetConfidenceWeight parameter is a modifier for the prior probability
 * that any pixel would be classified as inside the target object.  This
 * implementation of the STAPLE algorithm automatically calculates prior
 * positive classification probability as the average fraction of the image
 * volume filled by the target object in each input segmentation.  The
 * ConfidenceWeight parameter allows for scaling the of this default prior
 * probability: if g_t is the prior probability that a pixel would be
 * classified inside the target object, then g_t is set to g_t *
 * ConfidenceWeight before iterating on the solution.  In general
 * ConfidenceWeight should be left to the default of 1.0.
 *
 * You must provide a foreground value using SetForegroundValue that the STAPLE
 * algorithm will use to identify positively classified pixels in the the input
 * images.  All other values in the image will be treated as background values.
 * For example, if your input segmentations consist of 1's everywhere inside
 * the segmented region, then use SetForegroundValue(1).
 *
 * The STAPLE algorithm is an iterative E-M algorithm and will converge on a
 * solution after some number of iterations that cannot be known a priori.
 * After updating the filter, the total elapsed iterations taken to converge on
 * the solution can be queried through GetElapsedIterations().  You may also
 * specify a MaximumNumberOfIterations, after which the algorithm will stop
 * iterating regardless of whether or not it has converged.  This
 * implementation of the STAPLE algorithm will find the solution to within
 * seven digits of precision unless it is stopped early.
 *
 * Once updated, the Sensitivity (true positive fraction, q) and Specificity
 * (true negative fraction, q) for each expert input volume can be queried
 * using GetSensitivity(i) and GetSpecificity(i), where i is the i-th input
 * volume.
 *
 * \par REQUIRED PARAMETERS
 * The only required parameters for this filter are the ForegroundValue and the
 * input volumes.  All other parameters may be safely left to their default
 * values. Please see the paper cited above for more information on the STAPLE
 * algorithm and its parameters.  A proper understanding of the algorithm is
 * important for interpreting the results that it produces.
 *
 * \par EVENTS
 * This filter invokes IterationEvent() at each iteration of the E-M
 * algorithm. Setting the AbortGenerateData() flag will cause the algorithm to
 * halt after the current iteration and produce results just as if it had
 * converged. The algorithm makes no attempt to report its progress since the
 * number of iterations needed cannot be known in advance.
 * \ingroup ITKImageCompare
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT STAPLEImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef STAPLEImageFilter                               Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(STAPLEImageFilter, ImageToImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType                   OutputPixelType;
  typedef typename TInputImage::PixelType                    InputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType RealType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Image typedef support */
  typedef TInputImage                       InputImageType;
  typedef typename InputImageType::Pointer  InputImagePointer;
  typedef TOutputImage                      OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Set get the binary ON value of the input image. */
  itkSetMacro(ForegroundValue, InputPixelType);
  itkGetConstMacro(ForegroundValue, InputPixelType);

  /** After the filter is updated, this method returns a std::vector<double> of
   *  all Specificity (true negative fraction, q) values for the expert
   *  input volumes. */
  const std::vector< double > & GetSpecificity() const
  {
    return m_Specificity;
  }

  /** After the filter is updated, this method returns a std::vector<double> of
   * all Sensitivity (true positive fraction, p) values for the expert input
   * volumes. */
  const std::vector< double > & GetSensitivity() const
  {
    return m_Sensitivity;
  }

  /** After the filter is updated, this method returns the Sensitivity (true
   * positive fraction, p) value for the i-th expert input volume. */
  double GetSensitivity(unsigned int i)
  {
    if ( i > this->GetNumberOfIndexedInputs() )
      {
      itkExceptionMacro(<< "Array reference out of bounds.");
      }
    return m_Sensitivity[i];
  }

  /** After the filter is updated, this method returns the Specificity (true
   * negative fraction, q) value for the i-th expert input volume. */
  double GetSpecificity(unsigned int i)
  {
    if ( i > this->GetNumberOfIndexedInputs() )
      {
      itkExceptionMacro(<< "Array reference out of bounds.");
      }
    return m_Specificity[i];
  }

  /** Set/Get the maximum number of iterations after which the STAPLE algorithm
   *  will be considered to have converged.  In general this SHOULD NOT be set and
   *  the algorithm should be allowed to converge on its own. */
  itkSetMacro(MaximumIterations, unsigned int);
  itkGetConstMacro(MaximumIterations, unsigned int);

  /** Scales the estimated prior probability that a pixel will be inside the
   *  targeted object of segmentation.  The default prior probability g_t is
   *  calculated automatically as the average fraction of positively classified
   *  pixels to the total size of the volume (across all input volumes).
   *  ConfidenceWeight will scale this default value as g_t = g_t *
   *  ConfidenceWeight.  In general, ConfidenceWeight should be left to the
   *  default of 1.0. */
  itkSetMacro(ConfidenceWeight, double);
  itkGetConstMacro(ConfidenceWeight, double);

  /** Get the number of elapsed iterations of the iterative E-M algorithm. */
  itkGetConstMacro(ElapsedIterations, unsigned int);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  STAPLEImageFilter()
  {
    m_ForegroundValue = NumericTraits< InputPixelType >::OneValue();
    m_MaximumIterations = NumericTraits< unsigned int >::max();
    m_ElapsedIterations = 0;
    m_ConfidenceWeight = 1.0;
  }

  virtual ~STAPLEImageFilter() ITK_OVERRIDE {}
  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream &, Indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(STAPLEImageFilter);

  InputPixelType m_ForegroundValue;
  unsigned int   m_ElapsedIterations;
  unsigned int   m_MaximumIterations;

  double m_ConfidenceWeight;

  std::vector< double > m_Sensitivity;
  std::vector< double > m_Specificity;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSTAPLEImageFilter.hxx"
#endif

#endif
