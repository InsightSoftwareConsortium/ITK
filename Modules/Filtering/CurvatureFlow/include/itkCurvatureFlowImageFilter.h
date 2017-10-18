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
#ifndef itkCurvatureFlowImageFilter_h
#define itkCurvatureFlowImageFilter_h

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkCurvatureFlowFunction.h"

namespace itk
{
/** \class CurvatureFlowImageFilter
  * \brief Denoise an image using curvature driven flow.
  *
  * CurvatureFlowImageFilter implements a curvature driven image denoising
  * algorithm. Iso-brightness contours in the grayscale input image are viewed
  * as a level set. The level set is then evolved using a curvature-based speed
  * function:
  *
  * \f[  I_t = \kappa |\nabla I| \f]
  * where \f$ \kappa \f$ is the curvature.
  *
  * The advantage of this approach is that sharp boundaries are preserved
  * with smoothing occurring only within a region. However, it should be
  * noted that continuous application of this scheme will result in the
  * eventual removal of all information as each contour shrinks to zero and
  * disappear.
  *
  * Note that unlike level set segmentation algorithms,
  * the image to be denoised is already the level set and can be set
  * directly as the input using the SetInput() method.
  *
  * This filter has two parameters: the number of update iterations to
  * be performed and the timestep between each update.
  *
  * The timestep should be "small enough" to ensure numerical stability.
  * Stability is guarantee when the timestep meets the CFL
  * (Courant-Friedrichs-Levy) condition. Broadly speaking, this condition
  * ensures that each contour does not move more than one grid position
  * at each timestep. In the literature, the timestep is typically user
  * specified and have to manually tuned to the application.
  *
  * This filter make use of the multi-threaded finite difference solver
  * hierarchy.  Updates are computed using a CurvatureFlowFunction object. A
  * zero flux Neumann boundary condition when computing derivatives near the
  * data boundary.
  *
  * This filter may be streamed. To support streaming this filter produces a
  * padded output which takes into account edge effects. The size of the
  * padding is m_NumberOfIterations on each edge. Users of this filter should
  * only make use of the center valid central region.
  *
  * \warning This filter assumes that the input and output types have the
  * same dimensions. This filter also requires that the output image pixels
  * are of a floating point type. This filter works for any dimensional images.
  *
  * Reference:
  * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  * Cambridge Press, Chapter 16, Second edition, 1999.
  *
  * \sa DenseFiniteDifferenceImageFilter
  * \sa CurvatureFlowFunction
  * \sa MinMaxCurvatureFlowImageFilter
  * \sa BinaryMinMaxCurvatureFlowImageFilter
  *
  * \ingroup ImageEnhancement
  * \ingroup MultiThreaded
  * \ingroup Streamed
  *
  * Input/Output Restrictions:
  *  TInputImage and TOutputImage must have the same dimension.
  *  TOutputImage's pixel type must be a real number type.
  * \ingroup ITKCurvatureFlow
  */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT CurvatureFlowImageFilter:
  public DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef CurvatureFlowImageFilter                                      Self;
  typedef DenseFiniteDifferenceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                          Pointer;
  typedef SmartPointer< const Self >                                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CurvatureFlowImageFilter,
               DenseFiniteDifferenceImageFilter);

  /** InputImage type. */
  typedef typename Superclass::InputImageType InputImageType;

  /** OutputImage type. */
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;

  /** FiniteDifferenceFunction type. */
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  /** CurvatureFlowFunction type. */
  typedef CurvatureFlowFunction< OutputImageType >
  CurvatureFlowFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename Superclass::PixelType PixelType;

  /** The time step type. Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Set the timestep parameter. */
  itkSetMacro(TimeStep, TimeStepType);

  /** Get the timestep parameter. */
  itkGetConstMacro(TimeStep, TimeStepType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, PixelType > ) );
  itkConceptMacro( OutputConvertibleToDoubleCheck,
                   ( Concept::Convertible< PixelType, double > ) );
  itkConceptMacro( OutputDivisionOperatorsCheck,
                   ( Concept::DivisionOperators< PixelType > ) );
  itkConceptMacro( DoubleOutputMultiplyOperatorCheck,
                   ( Concept::MultiplyOperator< double, PixelType, PixelType > ) );
  itkConceptMacro( IntOutputMultiplyOperatorCheck,
                   ( Concept::MultiplyOperator< int, PixelType, PixelType > ) );
  itkConceptMacro( OutputLessThanDoubleCheck,
                   ( Concept::LessThanComparable< PixelType, double > ) );
  itkConceptMacro( OutputDoubleAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< PixelType, double > ) );
  // End concept checking
#endif

protected:
  CurvatureFlowImageFilter();
  ~CurvatureFlowImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Supplies the halting criteria for this class of filters.  The
   * algorithm will stop after a user-specified number of iterations. */
  virtual bool Halt() ITK_OVERRIDE
  {
    if ( this->GetElapsedIterations() == this->GetNumberOfIterations() )
      {
      return true;
      }
    else
      {
      return false;
      }
  }

  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration() ITK_OVERRIDE;

  /** To support streaming, this filter produces a output which is
   * larger than the original requested region. The output is padding
   * by m_NumberOfIterations pixels on edge. */
  virtual void EnlargeOutputRequestedRegion(DataObject *) ITK_OVERRIDE;

  /** Edge effects are taken care of by padding the output requested
   * region. As such, the input requested region needs to at
   * minimum the same size as the output requested region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CurvatureFlowImageFilter);

  TimeStepType m_TimeStep;
};
} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureFlowImageFilter.hxx"
#endif

#endif
