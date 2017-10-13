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
#ifndef itkExtractOrthogonalSwath2DImageFilter_h
#define itkExtractOrthogonalSwath2DImageFilter_h

#include "itkImageAndPathToImageFilter.h"
#include "itkParametricPath.h"

namespace itk
{
/** \class ExtractOrthogonalSwath2DImageFilter
 * \brief Extracts into rectangular form a "swath" image from the input image along the parametric path.
 *
 * Extracts a rectangular "swath" image from the 2D input image by interpolating
 * image pixels orthogonal to the parametric path while walking along the
 * path.  The top half of the swath image corresponds to pixels to the left of
 * the path when walking along the path, and the bottom half of the swath image
 * likewise corresponds to pixels to the right of the path when walking along
 * the path.  The center row of the swath image corresponds to pixels laying
 * directly on the path.  The input and output images must be of the same type.
 *
 * \ingroup   ImageFilters
 * \ingroup   PathFilters
 * \ingroup ITKPath
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ExtractOrthogonalSwath2DImageFilter:public
  ImageAndPathToImageFilter< TImage, ParametricPath< 2 >, TImage >
{
public:
  /** Standard class typedefs. */
  typedef ExtractOrthogonalSwath2DImageFilter Self;
  typedef ImageAndPathToImageFilter< TImage, ParametricPath< 2 >, TImage >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtractOrthogonalSwath2DImageFilter, ImageAndPathToImageFilter);

  /** Some convenient typedefs. */
  typedef           TImage                        ImageType;
  typedef typename  ImageType::Pointer            ImagePointer;
  typedef typename  ImageType::ConstPointer       ImageConstPointer;
  typedef typename  ImageType::RegionType         ImageRegionType;
  typedef typename  ImageType::IndexType          ImageIndexType;
  typedef typename  ImageType::PixelType          ImagePixelType;
  typedef           ParametricPath< 2 >           PathType;
  typedef typename  PathType::ConstPointer        PathConstPointer;
  typedef typename  PathType::InputType           PathInputType;
  typedef typename  PathType::OutputType          PathOutputType;
  typedef typename  PathType::IndexType           PathIndexType;
  typedef typename  PathType::ContinuousIndexType PathContinuousIndexType;
  typedef typename  PathType::OffsetType          PathOffsetType;
  typedef typename  PathType::VectorType          PathVectorType;
  typedef typename  ImageType::SizeType           SizeType;

  /** ImageDimension constants */
  itkStaticConstMacro(PathDimension, unsigned int,
                      2);
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Spacing (size of a pixel) of the output image. The
   * spacing is normally the geometric distance between image samples,
   * but in the case of a swath image it is meaningless since the size
   * of each pixel varies depending on the curvature of the input path.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  virtual void SetSpacing(const double *spacing);

  virtual void SetSpacing(const float *spacing);

  virtual const double * GetSpacing() const;

  /** The origin of the output image. The origin is the geometric
   * coordinates of the index (0,0,...,0).  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  virtual void SetOrigin(const double *origin);

  virtual void SetOrigin(const float *origin);

  virtual const double * GetOrigin() const;

  /** Set the size of the swath image.
   * The number of rows (size[1]) MUST be odd */
  itkSetMacro(Size, SizeType)

  /** Set the default pixel value of the swath image, to be used if the swath
   * extends past the edge of the input image data. */
  itkSetMacro(DefaultPixelValue, ImagePixelType)

  //--------------------------------------------------------------------------
  //

  /** Request the largest possible region on all outputs. */
  virtual void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE
  {
    output->SetRequestedRegionToLargestPossibleRegion();
  }

  //
  //--------------------------------------------------------------------------

protected:
  ExtractOrthogonalSwath2DImageFilter()
  {
    m_DefaultPixelValue = NumericTraits< ImagePixelType >::ZeroValue();
    m_Size[0] = 512;
    m_Size[1] = 16 * 2 + 1; // must be odd
    m_Origin[0]  = m_Origin[1]  = 0.0;
    m_Spacing[0] = m_Spacing[1] = 1.0;
  }

  virtual ~ExtractOrthogonalSwath2DImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  //--------------------------------------------------------------------------
  //

  /** GenerateOutputInformation does not rely on input information */
  virtual void GenerateOutputInformation(void) ITK_OVERRIDE;

  /** Request the largest possible region on all inputs. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE
  {
    Superclass::GenerateInputRequestedRegion();
    this->GetNonConstImageInput()->SetRequestedRegionToLargestPossibleRegion();
    this->GetNonConstPathInput()->SetRequestedRegionToLargestPossibleRegion();
  }

  virtual void GenerateData(void) ITK_OVERRIDE;

  //
  //--------------------------------------------------------------------------

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExtractOrthogonalSwath2DImageFilter);

  ImagePixelType m_DefaultPixelValue;
  SizeType       m_Size;
  double         m_Origin[ImageDimension];
  double         m_Spacing[ImageDimension];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtractOrthogonalSwath2DImageFilter.hxx"
#endif

#endif
