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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBSplineDecompositionImageFilter_h
#define itkBSplineDecompositionImageFilter_h

#include <vector>

#include "itkImageLinearIteratorWithIndex.h"
#include "vnl/vnl_matrix.h"

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class BSplineDecompositionImageFilter
 * \brief Calculates the B-Spline coefficients of an image. Spline order may be from 0 to 5.
 *
 * This class defines N-Dimension B-Spline transformation.
 * It is based on:
 *    [1] M. Unser,
 *       "Splines: A Perfect Fit for Signal and Image Processing,"
 *        IEEE Signal Processing Magazine, vol. 16, no. 6, pp. 22-38,
 *        November 1999.
 *    [2] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part I--Theory,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 821-832,
 *        February 1993.
 *    [3] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part II--Efficient Design and Applications,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 834-848,
 *        February 1993.
 * And code obtained from bigwww.epfl.ch by Philippe Thevenaz
 *
 * Limitations:  Spline order must be between 0 and 5.
 *               Spline order must be set before setting the image.
 *               Uses mirror boundary conditions.
 *               Requires the same order of Spline for each dimension.
 *               Can only process LargestPossibleRegion
 *
 * \sa BSplineResampleImageFunction
 *
 * \ingroup ImageFilters
 * \ingroup SingleThreaded
 * \ingroup CannotBeStreamed
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BSplineDecompositionImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineDecompositionImageFilter);

  /** Standard class type aliases. */
  using Self = BSplineDecompositionImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDecompositionImageFilter, ImageToImageFilter);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Inherit input and output image types from Superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using OutputImagePointer = typename Superclass::OutputImagePointer;

  using CoeffType = typename itk::NumericTraits<typename TOutputImage::PixelType>::RealType;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Iterator type alias support */
  using OutputLinearIterator = ImageLinearIteratorWithIndex<TOutputImage>;

  using SplinePolesVectorType = std::vector<double>;

  /** Get/Sets the Spline Order, supports 0th - 5th order splines. The default
   *  is a 3rd order spline. */
  void
  SetSplineOrder(unsigned int SplineOrder);

  itkGetConstMacro(SplineOrder, int);

  /** Get the poles calculated for a given spline order. */
  itkGetConstMacro(SplinePoles, SplinePolesVectorType);

  /** Get the number of poles. */
  itkGetConstMacro(NumberOfPoles, int);


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(DimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  BSplineDecompositionImageFilter();
  ~BSplineDecompositionImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  /** This filter requires all of the input image. */
  void
  GenerateInputRequestedRegion() override;

  /** This filter must produce all of its output at once. */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  using CoefficientsVectorType = std::vector<CoeffType>;

  /** Determines the poles given the Spline Order. */
  virtual void
  SetPoles();

  /** Converts a vector of data to a vector of Spline coefficients. */
  virtual bool
  DataToCoefficients1D();

  /** Converts an N-dimension image of data to an equivalent sized image
   *    of spline coefficients. */
  void
  DataToCoefficientsND();

  /** Determines the first coefficient for the causal filtering of the data. */
  virtual void
  SetInitialCausalCoefficient(double z);

  /** Determines the first coefficient for the anti-causal filtering of the
    data. */
  virtual void
  SetInitialAntiCausalCoefficient(double z);

  /** Copy the input image into the output image.
   *  Used to initialize the Coefficients image before calculation. */
  void
  CopyImageToImage();

  /** Copies a vector of data from the Coefficients image (one line of the
   *  output image) to the scratch. */
  void
  CopyCoefficientsToScratch(OutputLinearIterator &);

  /** Copies a vector of data from the scratch to the Coefficients image
   *  (one line of the output image). */
  void
  CopyScratchToCoefficients(OutputLinearIterator &);

  // Variables needed by the smoothing spline routine.

  /** Temporary storage for processing of Coefficients. */
  CoefficientsVectorType m_Scratch;

  /** Image size. */
  typename TInputImage::SizeType m_DataLength;

  /** User specified spline order (3rd or cubic is the default). */
  unsigned int m_SplineOrder{ 0 };

  SplinePolesVectorType m_SplinePoles;

  int m_NumberOfPoles;

  /** Tolerance used for determining initial causal coefficient. Default is 1e-10.*/
  double m_Tolerance{ 1e-10 };

  /** Direction for iterator incrementing. Default is 0. */
  unsigned int m_IteratorDirection{ 0 };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineDecompositionImageFilter.hxx"
#endif

#endif
