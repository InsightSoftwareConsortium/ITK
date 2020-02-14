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
#ifndef itkHessianToObjectnessMeasureImageFilter_h
#define itkHessianToObjectnessMeasureImageFilter_h

#include "itkSymmetricSecondRankTensor.h"
#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class HessianToObjectnessMeasureImageFilter
 * \brief A filter to enhance M-dimensional objects in N-dimensional images
 *
 * The objectness measure is a generalization of Frangi's vesselness measure,
 * which is based on the analysis of the the Hessian eigen system. The filter
 * can enhance blob-like structures (M=0), vessel-like structures (M=1), 2D
 * plate-like structures (M=2), hyper-plate-like structures (M=3) in N-dimensional
 * images, with M<N.
 * The filter takes an image of a Hessian pixels ( SymmetricSecondRankTensor pixels
 * pixels ) and produces an enhanced image. The Hessian input image can be produced
 * using itk::HessianRecursiveGaussianImageFilter.
 *
 *
 * \par References
 * Frangi, AF, Niessen, WJ, Vincken, KL, & Viergever, MA (1998). Multiscale Vessel
 * Enhancement Filtering. In Wells, WM, Colchester, A, & Delp, S, Editors, MICCAI '98
 * Medical Image Computing and Computer-Assisted Intervention, Lecture Notes in Computer
 * Science, pages 130-137, Springer Verlag, 1998.
 *
 * Additional information can be from in the Insight Journal:
 * https://hdl.handle.net/1926/576
 *
 * \author Luca Antiga Ph.D.  Medical Imaging Unit,
 *                            Bioengineering Department, Mario Negri Institute, Italy.
 *
 * \sa MultiScaleHessianBasedMeasureImageFilter
 * \sa Hessian3DToVesselnessMeasureImageFilter
 * \sa HessianRecursiveGaussianImageFilter
 * \sa SymmetricEigenAnalysisImageFilter
 * \sa SymmetricSecondRankTensor
 *
 * \ingroup ITKImageFeature
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT HessianToObjectnessMeasureImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(HessianToObjectnessMeasureImageFilter);

  /** Standard class type aliases. */
  using Self = HessianToObjectnessMeasureImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Image dimension */
  static constexpr unsigned int ImageDimension = InputImageType ::ImageDimension;

  using EigenValueType = double;
  using EigenValueArrayType = itk::FixedArray<EigenValueType, Self::ImageDimension>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(HessianToObjectnessMeasureImageFilter, ImageToImageFilter);

  /** Set/Get Alpha, the weight corresponding to R_A
   * (the ratio of the smallest eigenvalue that has to be large to the larger ones).
   * Smaller values lead to increased sensitivity to the object dimensionality. */
  itkSetMacro(Alpha, double);
  itkGetConstMacro(Alpha, double);

  /** Set/Get Beta, the weight corresponding to R_B
   * (the ratio of the largest eigenvalue that has to be small to the larger ones).
   * Smaller values lead to increased sensitivity to the object dimensionality. */
  itkSetMacro(Beta, double);
  itkGetConstMacro(Beta, double);

  /** Set/Get Gamma, the weight corresponding to S
   * (the Frobenius norm of the Hessian matrix, or second-order structureness) */
  itkSetMacro(Gamma, double);
  itkGetConstMacro(Gamma, double);

  /** Toggle scaling the objectness measure with the magnitude of the largest
    absolute eigenvalue */
  itkSetMacro(ScaleObjectnessMeasure, bool);
  itkGetConstMacro(ScaleObjectnessMeasure, bool);
  itkBooleanMacro(ScaleObjectnessMeasure);

  /** Set/Get the dimensionality of the object (0: points (blobs),
   * 1: lines (vessels), 2: planes (plate-like structures), 3: hyper-planes.
   * ObjectDimension must be smaller than ImageDimension. */
  itkSetMacro(ObjectDimension, unsigned int);
  itkGetConstMacro(ObjectDimension, unsigned int);

  /** Enhance bright structures on a dark background if true, the opposite if
    false. Default is "On" (equivalent to vesselness). */
  itkSetMacro(BrightObject, bool);
  itkGetConstMacro(BrightObject, bool);
  itkBooleanMacro(BrightObject);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, OutputPixelType>));
  // End concept checking
#endif

protected:
  HessianToObjectnessMeasureImageFilter();
  ~HessianToObjectnessMeasureImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  VerifyPreconditions() ITKv5_CONST override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


private:
  // functor used to sort the eigenvalues are to be sorted
  // |e1|<=|e2|<=...<=|eN|
  //
  // Returns ( abs(a) < abs(b) )
  struct AbsLessCompare
  {
    bool
    operator()(EigenValueType a, EigenValueType b)
    {
      return itk::Math::abs(a) < itk::Math::abs(b);
    }
  };

  double       m_Alpha{ 0.5 };
  double       m_Beta{ 0.5 };
  double       m_Gamma{ 5.0 };
  unsigned int m_ObjectDimension{ 1 };
  bool         m_BrightObject{ true };
  bool         m_ScaleObjectnessMeasure{ true };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHessianToObjectnessMeasureImageFilter.hxx"
#endif

#endif
