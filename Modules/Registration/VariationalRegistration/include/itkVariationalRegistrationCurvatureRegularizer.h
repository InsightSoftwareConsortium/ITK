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
#ifndef itkVariationalRegistrationCurvatureRegularizer_h
#define itkVariationalRegistrationCurvatureRegularizer_h

#include "itkVariationalRegistrationRegularizer.h"
#include "itkMultiThreaderBase.h"

#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)

// other includes:
#  include "itkFFTWCommon.h"

namespace itk
{

/** \class itk::VariationalRegistrationCurvatureRegularizer
 *
 *  \brief This class performs linear elastic regularization of a vector field.
 *
 *  This class implements curvature regularization as described in
 *  <em>Modersitzki. "Numerical methods for image registration". OUP Oxford, 2003.</em>.
 *  and
 *  <em> Fischer and Modersitzki. "A unified approach to fast image registration and a
 *  new curvature based registration technique."
 *  Linear Algebra and its applications 380 (2004): 107-124.</em>
 *
 *  Some parts of the code are according to CurvatureRegistrationFilter by T. Rohlfing.
 *
 *  We compute \f$u^{out}=(Id - A)^{-1}[u^{in}]\f$ with
 *  \f$A[u]=\alpha\Delta^2 u \f$ using an FFT based method.
 *  Please note that for given weight \f$\alpha'\f$ you have to set
 *  \f$\alpha=\tau\alpha'\f$ (see Eq.(2) in VariationalRegistrationFilter).
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationRegularizer
 *
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TDisplacementField>
class VariationalRegistrationCurvatureRegularizer : public VariationalRegistrationRegularizer<TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalRegistrationCurvatureRegularizer);

  /** Standard class type alias */
  using Self = VariationalRegistrationCurvatureRegularizer;
  using Superclass = VariationalRegistrationRegularizer<TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalRegistrationCurvatureRegularizer, VariationalRegistrationRegularizer);

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TDisplacementField::ImageDimension;

  /** Deformation field types, inherited from Superclass. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;
  using DisplacementFieldConstPointer = typename Superclass::DisplacementFieldConstPointer;
  using PixelType = typename Superclass::PixelType;
  using ValueType = typename Superclass::ValueType;
  typedef typename DisplacementFieldType::SizeType::SizeValueType OffsetValueType;

  /** Types for FFTW proxy */

#  if defined(ITK_USE_FFTWD)
  // Prefer to use double precision
  using RealTypeFFT = double;
#  else
#    if defined(ITK_USE_FFTWF)
// Allow to use single precision
#      warning "Using single precision for FFT computations!"
  using RealTypeFFT = float;
#    endif
#  endif

  using FFTWProxyType = typename fftw::Proxy<RealTypeFFT>;

  /** Set the regularization weight alpha */
  itkSetMacro(Alpha, ValueType);

  /** Get the regularization weight alpha */
  itkGetConstMacro(Alpha, ValueType);

protected:
  VariationalRegistrationCurvatureRegularizer();
  ~VariationalRegistrationCurvatureRegularizer() override;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Execute regularization. This method is multi-threaded but does not
   * use ThreadedGenerateData(). */
  void
  GenerateData() override;

  /** Method for initialization. Buffer images are allocated and the matrices
   * calculated in this method. */
  void
  Initialize() override;

  /** Initialize FFTW plans and multi-threading, allocate arrays for FFT */
  virtual bool
  InitializeCurvatureFFTPlans();

  /** Precompute sine and cosine values for solving the LES */
  virtual bool
  InitializeCurvatureDiagonalMatrix();

  /** Regularize the deformation field. This is called by GenerateData(). */
  virtual void
  Regularize();

  /** solve the LES after forward FFTs (before backward FFTs) */
  virtual void
  SolveCurvatureLES(unsigned int currentDimension);

  /** solve the LES after forward FFTs (and before backward FFTs). Multithreaded method. */
  virtual void
  ThreadedSolveCurvatureLES(unsigned int currentDimension, OffsetValueType from, OffsetValueType to);

  /** Calculate the index in the complex image for a given offset. */
  typename DisplacementFieldType::IndexType
  CalculateImageIndex(OffsetValueType offset);

private:
  /** Weight of the regularization term. */
  ValueType m_Alpha;

  /** The spacing of the displacement field. */
  typename DisplacementFieldType::SpacingType m_Spacing;

  /** The size of the displacement field. */
  typename DisplacementFieldType::SizeType m_Size;

  /** Number of pixels of the displacement field. */
  OffsetValueType m_TotalSize;

  /** offset table needed to compute image index from array index */
  OffsetValueType m_OffsetTable[ImageDimension];

  /** diagonal matrix for solving LES after FFT */
  RealTypeFFT * m_DiagonalMatrix[ImageDimension];

  /** FFT plans and buffers */
  typename FFTWProxyType::PlanType m_PlanForward;  /** FFT forward plan  */
  typename FFTWProxyType::PlanType m_PlanBackward; /** FFT backward plan */
  typename FFTWProxyType::PixelType *
    m_VectorFieldComponentBuffer; /** FFT memory space for input/output spatial data */
  typename FFTWProxyType::PixelType *
    m_DCTVectorFieldComponentBuffer; /** FFT memory space for output/input frequency data */

  struct CurvatureFFTThreadStruct
  {
    VariationalRegistrationCurvatureRegularizer * Filter;
    OffsetValueType                               totalSize;
    unsigned int                                  currentDimension;
  };

  static ITK_THREAD_RETURN_TYPE
  SolveCurvatureLESThreaderCallback(void * vargs);
};

} // namespace itk

#  ifndef ITK_MANUAL_INSTANTIATION
#    include "itkVariationalRegistrationCurvatureRegularizer.hxx"
#  endif

#endif
#endif
