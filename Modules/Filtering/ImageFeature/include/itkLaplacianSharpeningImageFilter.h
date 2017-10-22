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
#ifndef itkLaplacianSharpeningImageFilter_h
#define itkLaplacianSharpeningImageFilter_h

#include "itkNumericTraits.h"
#include "itkImageToImageFilter.h"

namespace itk
{
/**
 * \class LaplacianSharpeningImageFilter
 * \brief This filter sharpens an image using a Laplacian.
 * LaplacianSharpening highlights regions of rapid intensity change
 * and therefore highlights or enhances the edges.  The result is an
 * image that appears more in focus.
 *
 * \par The LaplacianSharpening at each pixel location is computed by
 * convolution with the itk::LaplacianOperator.
 *
 * \par Inputs and Outputs
 * The input to this filter is a scalar-valued itk::Image of arbitrary
 * dimension. The output is a scalar-valued itk::Image.
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa LaplacianOperator
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKImageFeature
 *
 * \wiki
 * \wikiexample{ImageProcessing/LaplacianSharpeningImageFilter,Sharpen an image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT LaplacianSharpeningImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard "Self" & Superclass typedef.   */
  typedef LaplacianSharpeningImageFilter                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef typename TOutputImage::InternalPixelType            OutputInternalPixelType;
  typedef typename NumericTraits< OutputPixelType >::RealType RealType;
  typedef typename TInputImage::PixelType                     InputPixelType;
  typedef typename TInputImage::InternalPixelType             InputInternalPixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Image typedef support. */
  typedef TInputImage                      InputImageType;
  typedef TOutputImage                     OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;

  /** Smart pointer typedef support.   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods)  */
  itkTypeMacro(LaplacianSharpeningImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** LaplacianSharpeningImageFilter needs a larger input requested
   * region than the output requested region (larger in the direction
   * of the derivative).  As such, LaplacianSharpeningImageFilter
   * needs to provide an implementation for
   * GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Enable/Disable using the image spacing information in
   *  calculations. Use this option if you  want derivatives in
   *  physical space. Default  is UseImageSpacingOn. */
  itkBooleanMacro( UseImageSpacing );

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);

protected:
  LaplacianSharpeningImageFilter()
  {
    m_UseImageSpacing = true;
  }

  virtual ~LaplacianSharpeningImageFilter() ITK_OVERRIDE {}

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default.   */
  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream &, Indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LaplacianSharpeningImageFilter);

  bool m_UseImageSpacing;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianSharpeningImageFilter.hxx"
#endif

#endif
