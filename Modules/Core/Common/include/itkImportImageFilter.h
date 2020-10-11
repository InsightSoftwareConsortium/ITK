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
#ifndef itkImportImageFilter_h
#define itkImportImageFilter_h

#include "itkImageSource.h"

namespace itk
{
/** \class ImportImageFilter
 * \brief Import data from a standard C array into an itk::Image
 *
 * ImportImageFilter provides a mechanism for importing data into an itk::Image.
 * ImportImageFilter is an image source, so it behaves like any other pipeline
 * object.
 *
 * This class is templated over the pixel type and the image dimension of
 * the output image.
 *
 * \ingroup IOFilters
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/ConvertArrayToImage,Convert Array To Image}
 * \endsphinx
 */
template <typename TPixel, unsigned int VImageDimension = 2>
class ITK_TEMPLATE_EXPORT ImportImageFilter : public ImageSource<Image<TPixel, VImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImportImageFilter);

  /** Typedef for the output image.   */
  using OutputImageType = Image<TPixel, VImageDimension>;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using SpacingType = typename OutputImageType::SpacingType;
  using OriginType = typename OutputImageType::PointType;
  using ImportImageContainerType = ImportImageContainer<SizeValueType, TPixel>;

  /** Standard class type aliases. */
  using Self = ImportImageFilter;
  using Superclass = ImageSource<OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImportImageFilter, ImageSource);

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = Index<VImageDimension>;

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = Size<VImageDimension>;

  /** Region type alias support A region is used to specify a
   * subset of an image. */
  using RegionType = ImageRegion<VImageDimension>;

  /** Type of the output image pixel type. */
  using OutputImagePixelType = TPixel;

  /** Get the pointer from which the image data is imported. */
  TPixel *
  GetImportPointer();

  /** Set the pointer from which the image data is imported.  "num" is
   * the number of pixels in the block of memory. If
   * "LetImageContainerManageMemory" is false, then the this filter will
   * not free the memory in its destructor and the application providing the
   * buffer retains the responsibility of freeing the memory for this image
   * data.  If "LetImageContainerManageMemory" is true, then the ImageContainer
   * will free the memory when it is destroyed. */
  void
  SetImportPointer(TPixel * ptr, SizeValueType num, bool LetImageContainerManageMemory);

  /** Set the region object that defines the size and starting index
   * for the imported image. This will serve as the LargestPossibleRegion,
   * the BufferedRegion, and the RequestedRegion.
   * \sa ImageRegion */
  void
  SetRegion(const RegionType & region)
  {
    if (m_Region != region)
    {
      m_Region = region;
      this->Modified();
    }
  }

  /** Get the region object that defines the size and starting index
   * for the imported image. This will serve as the LargestPossibleRegion,
   * the BufferedRegion, and the RequestedRegion.
   * \sa ImageRegion */
  const RegionType &
  GetRegion() const
  {
    return m_Region;
  }

  /** Set the spacing (size of a pixel) of the image.
   * \sa GetSpacing() */
  itkSetMacro(Spacing, SpacingType);
  itkGetConstReferenceMacro(Spacing, SpacingType);
  itkSetVectorMacro(Spacing, const float, VImageDimension);

  /** Set the origin of the image.
   * \sa GetOrigin() */
  itkSetMacro(Origin, OriginType);
  itkGetConstReferenceMacro(Origin, OriginType);
  itkSetVectorMacro(Origin, const float, VImageDimension);

  using DirectionType = Matrix<SpacePrecisionType, VImageDimension, VImageDimension>;

  /** Set the direction of the image
   * \sa GetDirection() */
  virtual void
  SetDirection(const DirectionType & direction);

  /**  Get the direction of the image
   * \sa SetDirection */
  itkGetConstReferenceMacro(Direction, DirectionType);

protected:
  ImportImageFilter();
  ~ImportImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This filter does not actually "produce" any data, rather it "wraps"
   * the user supplied data into an itk::Image.  */
  void
  GenerateData() override;

  /** This is a source, so it must set the spacing, size, and largest possible
   * region for the output image that it will produce.
   * \sa ProcessObject::GenerateOutputInformation() */
  void
  GenerateOutputInformation() override;

  /** This filter can only produce the amount of data that it is given,
   * so we must override ProcessObject::EnlargeOutputRequestedRegion()
   * (The default implementation of a source produces the amount of
   * data requested.  This source, however, can only produce what it is
   * given.)
   *
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  RegionType    m_Region;
  SpacingType   m_Spacing;
  OriginType    m_Origin;
  DirectionType m_Direction;

  typename ImportImageContainerType::Pointer m_ImportImageContainer;
  SizeValueType                              m_Size;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImportImageFilter.hxx"
#endif

#endif
