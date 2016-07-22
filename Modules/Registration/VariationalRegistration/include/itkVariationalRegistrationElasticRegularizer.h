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
#ifndef __itkVariationalRegistrationElasticRegularizer_h
#define __itkVariationalRegistrationElasticRegularizer_h

#include "itkVariationalRegistrationRegularizer.h"
#include "itkMultiThreader.h"

#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)

// other includes:
#  include "itkFFTWCommon.h"

namespace itk
{

/** \class itk::VariationalRegistrationElasticRegularizer
 *
 *  \brief This class performs linear elastic regularization of a vector field.
 *
 *  This class implements linear elastic regularization as described in
 *  <em>Modersitzki. "Numerical methods for image registration". OUP Oxford, 2003.</em>.
 *
 *  We compute \f$u^{out}=(Id - A)^{-1}[u^{in}]\f$ with
 *  \f$A[u]=\mu\Delta u + (\mu+\lambda)\nabla(\nabla\cdot u)\f$ using an FFT based method.
 *  Please note that for given Lame constants \f$\mu'\f$ and \f$\lambda'\f$ you have to set
 *  \f$\mu=\tau\mu'\f$ and \f$\lambda=\tau\lambda'\f$ (see Eq.(2)
 *  in VariationalRegistrationFilter).
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationRegularizer
 *
 *  \ingroup VariationalRegistration
 *
 *  \warning This class is only implemented for image dimension 2 or 3.
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <class TDisplacementField>
class VariationalRegistrationElasticRegularizer : public VariationalRegistrationRegularizer<TDisplacementField>
{
public:
  /** Standard class typedefs */
  typedef VariationalRegistrationElasticRegularizer              Self;
  typedef VariationalRegistrationRegularizer<TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                     Pointer;
  typedef SmartPointer<const Self>                               ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalRegistrationElasticRegularizer, VariationalRegistrationRegularizer);

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int, TDisplacementField::ImageDimension);

  /** Deformation field types, inherited from Superclass. */
  typedef typename Superclass::DisplacementFieldType              DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer           DisplacementFieldPointer;
  typedef typename Superclass::DisplacementFieldConstPointer      DisplacementFieldConstPointer;
  typedef typename Superclass::PixelType                          PixelType;
  typedef typename Superclass::ValueType                          ValueType;
  typedef typename DisplacementFieldType::SizeType::SizeValueType OffsetValueType;

  /** Types for FFTW proxy */

#  if defined(ITK_USE_FFTWD)
  // Prefer to use double precision
  typedef double RealTypeFFT;
#  else
#    if defined(ITK_USE_FFTWF)
// Allow to use single precision
#      warning "Using single precision for FFT computations!"
  typedef float RealTypeFFT;
#    endif
#  endif

  typedef typename fftw::Proxy<RealTypeFFT> FFTWProxyType;

  /** Set the regularization weight lambda. */
  itkSetMacro(Lambda, ValueType);

  /** Get the regularization weight lambda. */
  itkGetConstMacro(Lambda, ValueType);

  /** Set the regularization weight mu. */
  itkSetMacro(Mu, ValueType);

  /** Get the regularization weight mu. */
  itkGetConstMacro(Mu, ValueType);

protected:
  VariationalRegistrationElasticRegularizer();
  ~VariationalRegistrationElasticRegularizer() {}

  /** Print information about the filter. */
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** Execute regularization. This method is multi-threaded but does not
   * use ThreadedGenerateData(). */
  virtual void
  GenerateData();

  /** Method for initialization. Buffer images are allocated and the matrices
   * calculated in this method. */
  virtual void
  Initialize();

  /** Initialize FFTW plans and multi-threading, allocate arrays for FFT */
  virtual bool
  InitializeElasticFFTPlans();

  /** Precompute sine and cosine values for solving the LES */
  virtual bool
  InitializeElasticMatrix();

  /** Delete all data allocated during Initialize() */
  virtual void
  FreeData();

  /** Regularize the deformation field. This is called by GenerateData(). */
  virtual void
  Regularize();

  /** solve the LES after forward FFTs (before backward FFTs) */
  virtual void
  SolveElasticLES();

  /** solve the LES after forward FFTs (and before backward FFTs). Multithreaded method. */
  virtual void
  ThreadedSolveElasticLES(OffsetValueType from, OffsetValueType to);

  /** Calculate the index in the complex image for a given offset. */
  typename DisplacementFieldType::IndexType
  CalculateComplexImageIndex(OffsetValueType offset);

private:
  VariationalRegistrationElasticRegularizer(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** Weight of the regularization term. */
  ValueType m_Lambda;

  /** Weight of the regularization term. */
  ValueType m_Mu;

  /** The spacing of the displacement field. */
  typename DisplacementFieldType::SpacingType m_Spacing;

  /** The size of the displacement field. */
  typename DisplacementFieldType::SizeType m_Size;

  /** Number of pixels of the displacement field. */
  OffsetValueType m_TotalSize;

  /** The size of the complex buffer. */
  typename DisplacementFieldType::SizeType m_ComplexSize;

  /** Number of pixels of the complex buffer. */
  OffsetValueType m_TotalComplexSize;

  OffsetValueType m_ComplexOffsetTable[ImageDimension];

  /** FFT matrix */
  double * m_MatrixCos[ImageDimension];
  double * m_MatrixSin[ImageDimension];

  /** FFT plans and buffers */
  typename FFTWProxyType::PlanType m_PlanForward[ImageDimension];  /** FFT forward plan  */
  typename FFTWProxyType::PlanType m_PlanBackward[ImageDimension]; /** FFT backward plan */
  typename FFTWProxyType::ComplexType *
    m_ComplexBuffer[ImageDimension];                  /** memory space for output of forward and input of backward FFT*/
  typename FFTWProxyType::PixelType * m_InputBuffer;  /** FFT memory space for input data */
  typename FFTWProxyType::PixelType * m_OutputBuffer; /** FFT memory space for output data */

  struct ElasticFFTThreadStruct
  {
    VariationalRegistrationElasticRegularizer * Filter;
    OffsetValueType                             totalComplexSize;
  };

  static ITK_THREAD_RETURN_TYPE
  SolveElasticLESThreaderCallback(void * vargs);
};

} // namespace itk

#  ifndef ITK_MANUAL_INSTANTIATION
#    include "itkVariationalRegistrationElasticRegularizer.hxx"
#  endif

#endif
#endif
