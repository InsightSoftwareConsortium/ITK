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
#ifndef itkIterativeDeconvolutionImageFilter_h
#define itkIterativeDeconvolutionImageFilter_h

#include "itkFFTConvolutionImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
/** \class IterativeDeconvolutionImageFilter
 * \brief Abstract base class for filters that implement iterative
 * deconvolution algorithms.
 *
 * This filter implements methods common to iterative deconvolution
 * algorithms. The input blurred image is set with the usual
 * SetInput() method and the blurring kernel is set with the
 * SetKernelImage() method.
 *
 * The number of iterations can be set with the method
 * SetNumberOfIterations(). This class emits an IterationEvent at the
 * start of each iteration. Iteration can be terminated early by
 * calling SetStopIteration( bool ) with the argument set to true. To
 * resume iterating, you must call SetStopIteration( bool ) with the
 * argument set to false before calling Update() a second time.
 *
 * This code was adapted from the Insight Journal contribution:
 *
 * "Deconvolution: infrastructure and reference algorithms"
 * by Gaetan Lehmann
 * https://hdl.handle.net/10380/3207
 *
 * \ingroup ITKDeconvolution
 */
template< typename TInputImage, typename TKernelImage=TInputImage, typename TOutputImage=TInputImage, typename TInternalPrecision=double >
class ITK_TEMPLATE_EXPORT IterativeDeconvolutionImageFilter :
    public FFTConvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
{
public:
  /** Standard typedefs. */
  typedef IterativeDeconvolutionImageFilter               Self;
  typedef FFTConvolutionImageFilter< TInputImage,
                                     TKernelImage,
                                     TOutputImage,
                                     TInternalPrecision > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Other useful typedefs. */
  typedef TInputImage  InputImageType;
  typedef TKernelImage KernelImageType;
  typedef TOutputImage OutputImageType;

  /** Internal types used by the FFT filters. */
  typedef typename Superclass::InternalImageType               InternalImageType;
  typedef typename Superclass::InternalImagePointerType        InternalImagePointerType;
  typedef typename Superclass::InternalComplexType             InternalComplexType;
  typedef typename Superclass::InternalComplexImageType        InternalComplexImageType;
  typedef typename Superclass::InternalComplexImagePointerType InternalComplexImagePointerType;

  /** Runtime information support. */
  itkTypeMacro(IterativeDeconvolutionImageFilter, ConvolutionImageFilterBase);

  /** Set/get the number of iterations. */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetMacro(NumberOfIterations, unsigned int);

  /** Set/get the current estimate. */
  itkGetModifiableObjectMacro(CurrentEstimate, InternalImageType);

  /** Stop iteration. When this method is called, the iteration will
   * halt after the current iteration has completed. */
  void SetStopIteration(bool stop)
  {
    // Don't call Modified() to avoid triggering a new update.
    m_StopIteration = stop;
  }
  itkGetConstMacro(StopIteration, bool);

  /** Get the current iteration. */
  itkGetConstMacro(Iteration, unsigned int);

protected:
  IterativeDeconvolutionImageFilter();
  virtual ~IterativeDeconvolutionImageFilter() ITK_OVERRIDE;

  /** Runs before iterating . */
  virtual void Initialize(ProgressAccumulator * progress,
                          float progressWeight,
                          float iterationProgressWeight);

  /** Performs an iteration. */
  virtual void Iteration(ProgressAccumulator * itkNotUsed(progress),
                         float itkNotUsed(iterationProgressWeight)) = 0;

  /** Runs after iterating. */
  virtual void Finish(ProgressAccumulator * progress,
                      float progressWeight);

  /** This filter needs the entire image kernel, which in general is
   * going to be a different size then the output requested region. As
   * such, this filter needs to provide an implementation for
   * GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Generate the output image data. Uses a minipipeline, so
   * ThreadedGenerateData is not overridden. */
  virtual void GenerateData() ITK_OVERRIDE;

  /** Discrete Fourier transform of the padded kernel. */
  InternalComplexImagePointerType m_TransferFunction;

  /** Intermediate results. Protected for easy access by subclasses. */
  InternalImagePointerType m_CurrentEstimate;

  typedef typename Superclass::FFTFilterType  FFTFilterType;
  typedef typename Superclass::IFFTFilterType IFFTFilterType;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IterativeDeconvolutionImageFilter);

  /** Number of iterations to run. */
  unsigned int m_NumberOfIterations;

  /** The current iteration. */
  unsigned int m_Iteration;

  /** Flag indicating whether iteration should be stopped. */
  bool m_StopIteration;

  /** Modified times for the input and kernel. */
  ModifiedTimeType m_InputMTime;
  ModifiedTimeType m_KernelMTime;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIterativeDeconvolutionImageFilter.hxx"
#endif

#endif
