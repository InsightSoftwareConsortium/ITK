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
#ifndef itkN4BiasFieldCorrectionImageFilter_h
#define itkN4BiasFieldCorrectionImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkArray.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkPointSet.h"
#include "itkVector.h"

#include "vnl/vnl_vector.h"

namespace itk
{

/**
 * \class N4BiasFieldCorrectionImageFilter
 * \brief Implementation of the N4  bias field correction algorithm.
 *
 * The nonparametric nonuniform intensity normalization (N3) algorithm, as
 * introduced by Sled et al. in 1998 is a method for correcting nonuniformity
 * associated with MR images. The algorithm assumes a simple parametric model
 * (Gaussian) for the bias field and does not require tissue class segmentation.
 * In addition, there are only a couple of parameters to tune with the default
 * values performing quite well. N3 has been publicly available as a set of
 * perl scripts (http://www.bic.mni.mcgill.ca/ServicesSoftwareAdvancedImageProcessingTools/HomePage)
 *
 * The N4 algorithm, encapsulated with this class, is a variation of the original
 * N3 algorithm with the additional benefits of an improved B-spline fitting
 * routine which allows for multiple resolutions to be used during the correction
 * process.  We also modify the iterative update component of algorithm such
 * that the residual bias field is continually updated
 *
 * Notes for the user:
 *  1. Since much of the image manipulation is done in the log space of the
 *     intensities, input images with negative and small values (< 1) can
 *     produce poor results.
 *  2. The original authors recommend performing the bias field correction
 *      on a downsampled version of the original image.
 *  3. A binary mask or a weighted image can be supplied.  If a binary mask
 *     is specified, those voxels in the input image which correspond to the
 *     voxels in the mask image are used to estimate the bias field. If a
 *     UseMaskLabel value is set to false (the default), all non-zero
 *     voxels in the  MaskImage will be masked; otherwise only voxels
 *     in the MaskImage that match  the MaskLabel will be used. If a confidence image is specified, the
 *     input voxels are weighted in the b-spline fitting routine according
 *     to the confidence voxel values.
 *  4. The filter returns the corrected image.  If the bias field is wanted, one
 *     can reconstruct it using the class itkBSplineControlPointImageFilter.
 *     See the IJ article and the test file for an example.
 *  5. The 'Z' parameter in Sled's 1998 paper is the square root
 *     of the class variable 'm_WienerFilterNoise'.
 *
 * The basic algorithm iterates between sharpening the intensity histogram of
 * the corrected input image and spatially smoothing those results with a
 * B-spline scalar field estimate of the bias field.
 *
 * \author Nicholas J. Tustison
 *
 * Contributed by Nicholas J. Tustison, James C. Gee in the Insight Journal
 * paper: https://www.insight-journal.org/browse/publication/640
 *
 * \par REFERENCE
 *
 * J.G. Sled, A.P. Zijdenbos and A.C. Evans.  "A Nonparametric Method for
 * Automatic Correction of Intensity Nonuniformity in  Data"
 * IEEE Transactions on Medical Imaging, Vol 17, No 1. Feb 1998.
 *
 * N.J. Tustison, B.B. Avants, P.A. Cook, Y. Zheng, A. Egan, P.A. Yushkevich,
 * and J.C. Gee. "N4ITK:  Improved N3 Bias Correction"
 * IEEE Transactions on Medical Imaging, 29(6):1310-1320, June 2010.
 *
 * \ingroup ITKBiasCorrection
 */

template <typename TInputImage,
          typename TMaskImage = Image<unsigned char, TInputImage::ImageDimension>,
          class TOutputImage = TInputImage>
class N4BiasFieldCorrectionImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(N4BiasFieldCorrectionImageFilter);

  /** Standard class type aliases. */
  using Self = N4BiasFieldCorrectionImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Runtime information support. */
  itkTypeMacro(N4BiasFieldCorrectionImageFilter, ImageToImageFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using MaskImageType = TMaskImage;
  using MaskPixelType = typename MaskImageType::PixelType;

  using RealType = float;
  using RealImageType = Image<RealType, ImageDimension>;
  using RealImagePointer = typename RealImageType::Pointer;
  using VariableSizeArrayType = Array<unsigned int>;

  /** B-spline smoothing filter argument type alias */
  using ScalarType = Vector<RealType, 1>;
  using PointSetType = PointSet<ScalarType, Self::ImageDimension>;
  using ScalarImageType = Image<ScalarType, Self::ImageDimension>;
  using PointSetPointer = typename PointSetType::Pointer;
  using PointType = typename PointSetType::PointType;

  /** B-sline filter type alias */
  using BSplineFilterType = BSplineScatteredDataPointSetToImageFilter<PointSetType, ScalarImageType>;
  using BiasFieldControlPointLatticeType = typename BSplineFilterType::PointDataImageType;
  using ArrayType = typename BSplineFilterType::ArrayType;

  /** Ensures that this filter can compute the entire output at once.  */
  void
  EnlargeOutputRequestedRegion(DataObject *) override;

  /**
   * The image expected for input for bias correction.
   */
  void
  SetInput1(const InputImageType * image)
  {
    this->SetInput(image);
  }

  /**
   * Set mask image function.  If a binary mask image is specified, only
   * those input image voxels inside the mask image values are used in
   * estimating the bias field.
   */
  itkSetInputMacro(MaskImage, MaskImageType);
  void
  SetInput2(const MaskImageType * mask)
  {
    this->SetMaskImage(mask);
  }

  /**
   * Get mask image function.  If a binary mask image is specified, only
   * those input image voxels inside the mask image values are used in
   * estimating the bias field.
   */
  itkGetInputMacro(MaskImage, MaskImageType);

  /**
   * Set/Get mask label value. If a binary mask image is specified and if
   * UseMaskValue is true, only those input image voxels corresponding
   * with mask image values equal to MaskLabel are used in estimating the
   * bias field. If a MaskImage is specified and UseMaskLabel is false, all
   * input image voxels corresponding to non-zero voxels in the MaskImage
   * are used in estimating the bias field. Default = 1.
   */
  itkSetMacro(MaskLabel, MaskPixelType);
  itkGetConstMacro(MaskLabel, MaskPixelType);

  /**
   * Use a mask label for identifying mask functionality. See SetMaskLabel.
   * Defaults to false. */
  itkSetMacro(UseMaskLabel, bool);
  itkGetConstMacro(UseMaskLabel, bool);
  itkBooleanMacro(UseMaskLabel);

  /**
   * Set confidence image function.  If a confidence image is specified,
   * estimation of the bias field weights the contribution of each voxel
   * according the value of the corresponding voxel in the confidence image.
   * For example, when estimating the bias field using brain , one can use
   * a soft segmentation of the white matter as the confidence image instead of
   * using a hard segmentation of the white matter as the mask image (as has
   * been done in the literature) as an alternative strategy to estimating the
   * bias field.
   */
  itkSetInputMacro(ConfidenceImage, RealImageType);
  void
  SetInput3(const RealImageType * image)
  {
    this->SetConfidenceImage(image);
  }

  /**
   * Get confidence image function.  If a confidence image is specified,
   * estimation of the bias field weights the contribution of each voxel
   * according the value of the corresponding voxel in the confidence image.
   * For example, when estimating the bias field using brain , one can use
   * a soft segmentation of the white matter as the confidence image instead of
   * using a hard segmentation of the white matter as the mask image (as has
   * been done in the literature) as an alternative strategy to estimating the
   * bias field.
   */
  itkGetInputMacro(ConfidenceImage, RealImageType);

  // Sharpen histogram parameters: in estimating the bias field, the
  // first step is to sharpen the intensity histogram by Wiener deconvolution
  // with a 1-D Gaussian.  The following parameters define this operation.
  // These default values in N4 match the default values in N3.

  /**
   * Set number of bins defining the log input intensity histogram.
   * Default = 200.
   */
  itkSetMacro(NumberOfHistogramBins, unsigned int);

  /**
   * Get number of bins defining the log input intensity histogram.
   * Default = 200.
   */
  itkGetConstMacro(NumberOfHistogramBins, unsigned int);

  /**
   * Set the noise estimate defining the Wiener filter.  Default = 0.01.
   */
  itkSetMacro(WienerFilterNoise, RealType);

  /**
   * Get the noise estimate defining the Wiener filter.  Default = 0.01.
   */
  itkGetConstMacro(WienerFilterNoise, RealType);

  /**
   * Set the full width at half maximum parameter characterizing the width of
   * the Gaussian deconvolution.  Default = 0.15.
   */
  itkSetMacro(BiasFieldFullWidthAtHalfMaximum, RealType);

  /**
   * Get the full width at half maximum parameter characterizing the width of
   * the Gaussian deconvolution.  Default = 0.15.
   */
  itkGetConstMacro(BiasFieldFullWidthAtHalfMaximum, RealType);

  // B-spline parameters governing the fitting routine

  /**
   * Set the spline order defining the bias field estimate.  Default = 3.
   */
  itkSetMacro(SplineOrder, unsigned int);

  /**
   * Get the spline order defining the bias field estimate.  Default = 3.
   */
  itkGetConstMacro(SplineOrder, unsigned int);

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro(NumberOfControlPoints, ArrayType);

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro(NumberOfControlPoints, ArrayType);

  /**
   * Set the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 1 level.
   */
  itkSetMacro(NumberOfFittingLevels, ArrayType);

  /**
   * Set the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 1 level.
   */
  void
  SetNumberOfFittingLevels(unsigned int n)
  {
    ArrayType nlevels;

    nlevels.Fill(n);
    this->SetNumberOfFittingLevels(nlevels);
  }

  /**
   * Get the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 1 level.
   */
  itkGetConstMacro(NumberOfFittingLevels, ArrayType);

  /**
   * Set the maximum number of iterations specified at each fitting level.
   * Default = 50.
   */
  itkSetMacro(MaximumNumberOfIterations, VariableSizeArrayType);

  /**
   * Get the maximum number of iterations specified at each fitting level.
   * Default = 50.
   */
  itkGetConstMacro(MaximumNumberOfIterations, VariableSizeArrayType);

  /**
   * Set the convergence threshold.  Convergence is determined by the
   * coefficient of variation of the difference image between the current bias
   * field estimate and the previous estimate.  If this value is less than the
   * specified threshold, the algorithm proceeds to the next fitting level or
   * terminates if it is at the last level.
   */
  itkSetMacro(ConvergenceThreshold, RealType);

  /**
   * Get the convergence threshold.  Convergence is determined by the
   * coefficient of variation of the difference image between the current bias
   * field estimate and the previous estimate.  If this value is less than the
   * specified threshold, the algorithm proceeds to the next fitting level or
   * terminates if it is at the last level.
   */
  itkGetConstMacro(ConvergenceThreshold, RealType);

  /**
   * Typically, a reduced size image is used as input to the N4 filter using
   * something like itkShrinkImageFilter.  Since the output is a corrected
   * version of the input, the user will probably want to apply the bias
   * field correction to the full resolution image.  This can be done by
   * using the LogBiasFieldControlPointLattice to reconstruct the bias field
   * at the full image resolution (using the class
   * BSplineControlPointImageFilter).
   */
  itkGetConstObjectMacro(LogBiasFieldControlPointLattice, BiasFieldControlPointLatticeType);

  /**
   * Get the number of elapsed iterations.  This is a helper function for
   * reporting observations.
   */
  itkGetConstMacro(ElapsedIterations, unsigned int);

  /**
   * Get the current convergence measurement.  This is a helper function for
   * reporting observations.
   */
  itkGetConstMacro(CurrentConvergenceMeasurement, RealType);

  /**
   * Get the current fitting level.  This is a helper function for
   * reporting observations.
   */
  itkGetConstMacro(CurrentLevel, unsigned int);

  /**
   * Reconstruct bias field given the control point lattice.
   */
  RealImagePointer
  ReconstructBiasField(const BiasFieldControlPointLatticeType *);

protected:
  N4BiasFieldCorrectionImageFilter();
  ~N4BiasFieldCorrectionImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  // N4 algorithm functions:  The basic algorithm iterates between sharpening
  // the intensity histogram of the corrected input image and spatially
  // smoothing those results with a B-spline scalar field estimate of the
  // bias field.  The former is handled by the function SharpenImage()
  // whereas the latter is handled by the function UpdateBiasFieldEstimate().
  // Convergence is determined by the coefficient of variation of the difference
  // image between the current bias field estimate and the previous estimate.

  /**
   * Sharpen the intensity histogram of the current estimate of the corrected
   * image and map those results to a new estimate of the unsmoothed corrected
   * image.
   */
  void
  SharpenImage(const RealImageType * unsharpenedImage, RealImageType * sharpenedImage) const;

  /**
   * Given the unsmoothed estimate of the bias field, this function smooths
   * the estimate and adds the resulting control point values to the total
   * bias field estimate.
   */
  RealImagePointer
  UpdateBiasFieldEstimate(RealImageType *, std::size_t);

  /**
   * Convergence is determined by the coefficient of variation of the difference
   * image between the current bias field estimate and the previous estimate.
   */
  RealType
  CalculateConvergenceMeasurement(const RealImageType *, const RealImageType *) const;

  MaskPixelType m_MaskLabel;
  bool          m_UseMaskLabel{ false };

  // Parameters for deconvolution with Wiener filter

  unsigned int m_NumberOfHistogramBins{ 200 };
  RealType     m_WienerFilterNoise{ static_cast<RealType>(0.01) };
  RealType     m_BiasFieldFullWidthAtHalfMaximum{ static_cast<RealType>(0.15) };

  // Convergence parameters

  VariableSizeArrayType m_MaximumNumberOfIterations;
  unsigned int          m_ElapsedIterations{ 0 };
  RealType              m_ConvergenceThreshold{ static_cast<RealType>(0.001) };
  RealType              m_CurrentConvergenceMeasurement;
  unsigned int          m_CurrentLevel{ 0 };

  // B-spline fitting parameters

  typename BiasFieldControlPointLatticeType::Pointer m_LogBiasFieldControlPointLattice;

  unsigned int m_SplineOrder{ 3 };
  ArrayType    m_NumberOfControlPoints;
  ArrayType    m_NumberOfFittingLevels;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkN4BiasFieldCorrectionImageFilter.hxx"
#endif

#endif
