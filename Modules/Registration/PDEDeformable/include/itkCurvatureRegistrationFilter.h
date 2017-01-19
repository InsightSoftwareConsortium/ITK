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
#ifndef itkCurvatureRegistrationFilter_h
#define itkCurvatureRegistrationFilter_h

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkMeanSquareRegistrationFunction.h"

#if defined( ITK_USE_FFTWF ) || defined( ITK_USE_FFTWD )
#include "fftw3.h"

namespace itk
{
/** \class CurvatureRegistrationFilter
 * \brief Deformably register two images using the fast curvature algorithm.
 *
 * CurvatureRegistrationFilter implements the fast (i.e., O(n log n) )
 * registration method described in B. Fischer and J. Modersitzki,
 * "A unified approach to fast image registration and a new curvature
 * based registration technique," Linear Algebra and its Applications,
 * vol. 380, pp. 107-124, 2004.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the fixed image type, moving image type,
 * the deformation field type, and the images forces function. Fundamentally,
 * any image force function in the PDE framework should work (e.g., mean
 * squares, demons, correlation, mutual information, etc.).
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial deformation field maybe set via
 * SetInitialDisplacementField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDisplacementField.
 *
 * This class makes use of the finite difference solver hierarchy. Update
 * for each iteration is computed in the function class defined by the
 * template parameter TImageForceFunction.
 *
 * The algorithm has three parameters: the number of iteration to be performed,
 * the time step, and the weight of the curvature regularization term in
 * the overall cost function.
 *
 * \warning The algorithm parameters (time step and constraint weight) as
 * given by F&M (dt=100; alpha=0.01) do not seem to work with this implementation.
 * In fact, they seem to be off by orders of magnitude. This may be an
 * effect of different normalization of the image forces, or this implementation
 * may be incorrect. Be aware.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * \warning There is something sketchy going on with the DCT (see FFTW
 * documentation) regarding numerical stability of the "ordinary" DCT.
 * Also, F&M are slightly ambiguous in their use of the DCT operator. So
 * it is quite possible that the implementation of their algorithm is
 * not quite correct. Ultimately, they are saying that the curvature
 * smoother is equivalent to a special low-pass filter that is applied
 * to the DCT coefficients.
 *
 * \note This class was developed with funding from:
 *
 * "CNS Deficits: Interaction of Age and Alcoholism"
 * NIAAA AA05965, PI: A. Pfefferbaum
 *
 * "INIA: Imaging Core"
 * NIAAA AA13521, PI: A. Pfefferbaum
 *
 *
 * \sa CurvatureRegistrationFunction
 * \ingroup DeformableImageRegistration MultiThreaded
 *
 * \author Torsten Rohlfing, SRI International, Neuroscience Program
 * \ingroup ITKPDEDeformableRegistration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField,
          typename TImageForceFunction =
            MeanSquareRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField > >
class ITK_TEMPLATE_EXPORT CurvatureRegistrationFilter:
  public PDEDeformableRegistrationFilter< TFixedImage, TMovingImage,
                                          TDisplacementField >
{
public:
  /** Standard class typedefs. */
  typedef CurvatureRegistrationFilter                                                     Self;
  typedef PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField > Superclass;
  typedef SmartPointer< Self >                                                            Pointer;
  typedef SmartPointer< const Self >                                                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CurvatureRegistrationFilter,
               PDEDeformableRegistrationFilter);

  /** Inherit types from superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** FixedImage image type. */
  typedef typename Superclass::FixedImageType    FixedImageType;
  typedef typename Superclass::FixedImagePointer FixedImagePointer;
  itkStaticConstMacro(ImageDimension, unsigned int, FixedImageType::ImageDimension);

  /** MovingImage image type. */
  typedef typename Superclass::MovingImageType    MovingImageType;
  typedef typename Superclass::MovingImagePointer MovingImagePointer;

  /** Deformation field type. */
  typedef typename Superclass::DisplacementFieldType
  DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer
  DisplacementFieldPointer;

  typedef typename TDisplacementField::PixelType         DisplacementFieldPixelType;
  typedef typename DisplacementFieldPixelType::ValueType DisplacementFieldComponentType;
  itkStaticConstMacro(DeformationVectorDimension, unsigned int, DisplacementFieldPixelType::Dimension);

  #if defined( ITK_USE_FFTWD )
  //Prefer to use double precision
  typedef double RealTypeDFT;
  #else
    #if defined( ITK_USE_FFTWF )
  //Allow to use single precision
      #warning "Using single precision for FFT computations!"
  typedef double RealTypeDFT;
    #endif
  #endif

  typedef Image< RealTypeDFT, TDisplacementField::ImageDimension > DisplacementFieldComponentImageType;
  typedef typename DisplacementFieldComponentImageType::Pointer    DisplacementFieldComponentImagePointer;

  /** FiniteDifferenceFunction type. */
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  /** CurvatureRegistrationFilterFunction type. */
  typedef TImageForceFunction RegistrationFunctionType;

  /** Set the constraint vs. image forces weight. */
  void SetConstraintWeight(const float w) { m_ConstraintWeight = w; }

  /** Set the time step. */
  void SetTimeStep(const TimeStepType ts) { m_TimeStep = ts; }

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This is value is only available for the previous iteration and
   * NOT the current iteration. */
  virtual double GetMetric() const;

protected:
  CurvatureRegistrationFilter();
  ~CurvatureRegistrationFilter();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Initialize the state of filter before starting first iteration. */
  virtual void Initialize();

  /** Apply update. */
  virtual void ApplyUpdate(const TimeStepType& dt);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CurvatureRegistrationFilter);

  unsigned int m_FixedImageDimensions[ImageDimension];

  RealTypeDFT *m_DisplacementFieldComponentImage;
  RealTypeDFT *m_DisplacementFieldComponentImageDCT;

  float m_ConstraintWeight;

  fftw_plan m_PlanForwardDCT;
  fftw_plan m_PlanBackwardDCT;

  TimeStepType m_TimeStep;

  RealTypeDFT *m_DiagonalElements[ImageDimension];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureRegistrationFilter.hxx"
#endif

#endif //defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)

#endif
