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
#ifndef itkRecursiveGaussianImageFilter_h
#define itkRecursiveGaussianImageFilter_h

#include "itkRecursiveSeparableImageFilter.h"

namespace itk
{
/** \class RecursiveGaussianImageFilter
 * \brief Base class for computing IIR convolution with an approximation of a  Gaussian kernel.
 *
 *    \f[
 *      \frac{ 1 }{ \sigma \sqrt{ 2 \pi } } \exp{ \left( - \frac{x^2}{ 2 \sigma^2 } \right) }
 *    \f]
 *
 * RecursiveGaussianImageFilter is the base class for recursive filters that
 * approximate convolution with the Gaussian kernel.
 * This class implements the recursive filtering
 * method proposed by R.Deriche in IEEE-PAMI
 * Vol.12, No.1, January 1990, pp 78-87,
 * "Fast Algorithms for Low-Level Vision"
 *
 * Details of the implementation are described in the technical report:
 * R. Deriche, "Recursively Implementing The Gaussian and Its Derivatives",
 * INRIA, 1993, ftp://ftp.inria.fr/INRIA/tech-reports/RR/RR-1893.ps.gz
 *
 * Further improvements of the algorithm are described in:
 * G. Farneback & C.-F. Westin, "On Implementation of Recursive Gaussian
 * Filters", so far unpublished.
 *
 * As compared to itk::DiscreteGaussianImageFilter, this filter tends
 * to be faster for large kernels, and it can take the derivative
 * of the blurred image in one step.  Also, note that we have
 * itk::RecursiveGaussianImageFilter::SetSigma(), but
 * itk::DiscreteGaussianImageFilter::SetVariance().
 *
 * \ingroup ImageEnhancement SingleThreaded
 * \see DiscreteGaussianImageFilter
 * \ingroup ITKSmoothing
 *
 * \wiki
 * \wikiexample{EdgesAndGradients/RecursiveGaussianImageFilter,Find higher derivatives of an image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT RecursiveGaussianImageFilter:
  public RecursiveSeparableImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef RecursiveGaussianImageFilter                               Self;
  typedef RecursiveSeparableImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                       Pointer;
  typedef SmartPointer< const Self >                                 ConstPointer;

  typedef typename Superclass::RealType       RealType;
  typedef typename Superclass::ScalarRealType ScalarRealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Type macro that defines a name for this class */
  itkTypeMacro(RecursiveGaussianImageFilter, RecursiveSeparableImageFilter);

  /** Set/Get the Sigma, measured in world coordinates, of the Gaussian
   * kernel.  The default is 1.0. An exception will be generated if
   * the Sigma value is less than or equal to zero.
   */
  itkGetConstMacro(Sigma, ScalarRealType);
  itkSetMacro(Sigma, ScalarRealType);

  /** Enum type that indicates if the filter applies the equivalent operation
      of convolving with a gaussian, first derivative of a gaussian or the
      second derivative of a gaussian.  */
  typedef  enum { ZeroOrder, FirstOrder, SecondOrder } OrderEnumType;

  /** Type of the output image */
  typedef TOutputImage OutputImageType;

  /** Set/Get the flag for normalizing the gaussian over scale-space.

      This flag enables the analysis of the differential shape of
      features independent of their size ( both pixels and physical
      size ). Following the notation of Tony Lindeberg:

      Let \f[ L(x; t) = g(x; t) \ast f(x) \f] be the scale-space
      representation of image \f[ f(x) \f]
      where \f[ g(x; t) = \frac{1}{ \sqrt{ 2 \pi t}  }  \exp{ \left( -\frac{x^2}{ 2 t } \right) } \f] is
      the Gaussian function and \f[\ast\f] denotes convolution. This
      is a change from above with \f[ t = \sigma^2 \f].

      Then the normalized derivative operator for normalized
      coordinates across scale is:

      \f[
            \partial_\xi = \sqrt{t} \partial_x
      \f]

      The resulting scaling factor is
      \f[
            \sigma^N
      \f]
      where N is the order of the derivative.


      When this flag is ON the filter will be normalized in such a way
      that the values of derivatives are not biased by the size of the
      object. That is to say the maximum value a feature reaches across
      scale is independent of the scale of the object.

      For analyzing an image across scale-space you want to enable
      this flag.  It is disabled by default.

      \note Not all scale space axioms are satisfied by this filter,
      some are only approximated. Particularly, at fine scales ( say
      less than 1 pixel ) other methods such as a discrete Gaussian
      kernel should be considered.
     */
  itkSetMacro(NormalizeAcrossScale, bool);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  /** Set/Get the Order of the Gaussian to convolve with.
      \li ZeroOrder is equivalent to convolving with a Gaussian.  This
      is the default.
      \li FirstOrder is equivalent to convolving with the first derivative of a Gaussian.
      \li SecondOrder is equivalent to convolving with the second derivative of a Gaussian.
    */
  itkSetMacro(Order, OrderEnumType);
  itkGetConstMacro(Order, OrderEnumType);

  /** Explicitly set a zeroth order derivative. */
  void SetZeroOrder();

  /** Explicitly set a first order derivative. */
  void SetFirstOrder();

  /** Explicitly set a second order derivative. */
  void SetSecondOrder();

protected:
  RecursiveGaussianImageFilter();
  virtual ~RecursiveGaussianImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Set up the coefficients of the filter to approximate a specific kernel.
   * Here it is used to approximate a Gaussian or one of its
   * derivatives. Parameter is the spacing along the dimension to
   * filter. */
  virtual void SetUp(ScalarRealType spacing) ITK_OVERRIDE;

  /* See superclass for doxygen. This method adds the additional check
   * that sigma is greater than zero. */
  virtual void VerifyPreconditions() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RecursiveGaussianImageFilter);

  /** Compute the N coefficients in the recursive filter. */
  void ComputeNCoefficients(ScalarRealType sigmad,
                            ScalarRealType A1, ScalarRealType B1, ScalarRealType W1, ScalarRealType L1,
                            ScalarRealType A2, ScalarRealType B2, ScalarRealType W2, ScalarRealType L2,
                            ScalarRealType & N0, ScalarRealType & N1,
                            ScalarRealType & N2, ScalarRealType & N3,
                            ScalarRealType & SN, ScalarRealType & DN, ScalarRealType & EN);

  /** Compute the D coefficients in the recursive filter. */
  void ComputeDCoefficients(ScalarRealType sigmad,
                            ScalarRealType W1, ScalarRealType L1, ScalarRealType W2, ScalarRealType L2,
                            ScalarRealType & SD, ScalarRealType & DD, ScalarRealType & ED);

  /** Compute the M coefficients and the boundary coefficients in the
   * recursive filter. */
  void ComputeRemainingCoefficients(bool symmetric);

  /** Sigma of the gaussian kernel. */
  ScalarRealType m_Sigma;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  OrderEnumType m_Order;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveGaussianImageFilter.hxx"
#endif

#endif
