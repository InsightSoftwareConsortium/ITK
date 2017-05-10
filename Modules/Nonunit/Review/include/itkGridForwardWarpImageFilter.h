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
#ifndef itkGridForwardWarpImageFilter_h
#define itkGridForwardWarpImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class GridForwardWarpImageFilter
 * \brief Warps a grid using an input deformation field.
 *
 * GridForwardWarpImageFilter warps a grid with respect to
 * a given deformation field.
 *
 * A displacement field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the input image. The vector type must support element access via operator
 * [].
 *
 * The output image is produced by forward mapping.
 *
 * Each vector in the displacement field represent the distance between
 * a geometric point in the input space and a point in the output space such
 * that:
 *
 * \f[ p_{in} = p_{out} + d \f]
 *
 * Typically the mapped position does not correspond to an integer pixel
 * position in the output image. We round it.
 *
 * Warning: the functionality provided by this class is currently implemented
 * with more options and multi-threaded in itk::GridImageSource by setting up
 * an appropriate itk::ResampleImageFilter instance to such an object.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/510
 *
 * \ingroup ITKReview
 */
template<
  typename TDisplacementField,
  typename TOutputImage
  >
class ITK_TEMPLATE_EXPORT GridForwardWarpImageFilter:
  public ImageToImageFilter< TDisplacementField, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GridForwardWarpImageFilter                             Self;
  typedef ImageToImageFilter< TDisplacementField, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(GridForwardWarpImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Inherit some types from the superclass. */
  typedef typename Superclass::OutputImageType    OutputImageType;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  typedef typename OutputImageType::IndexType     IndexType;
  typedef typename OutputImageType::SizeType      SizeType;
  typedef typename OutputImageType::PixelType     PixelType;
  typedef typename OutputImageType::SpacingType   SpacingType;

  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(DisplacementFieldDimension, unsigned int,
                      TDisplacementField::ImageDimension);

  /** Deformation field typedef support. */
  typedef TDisplacementField                           DisplacementFieldType;
  typedef typename DisplacementFieldType::ConstPointer DisplacementFieldConstPointer;
  typedef typename DisplacementFieldType::PixelType    DisplacementType;

  /** Set the background value */
  itkSetMacro(BackgroundValue, PixelType);

  /** Get the background value */
  itkGetConstMacro(BackgroundValue, PixelType);

  /** Set the foreground value */
  itkSetMacro(ForegroundValue, PixelType);

  /** Get the foreground value */
  itkGetConstMacro(ForegroundValue, PixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, DisplacementFieldDimension > ) );
  itkConceptMacro( DisplacementFieldHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TDisplacementField::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  GridForwardWarpImageFilter();
  ~GridForwardWarpImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GridForwardWarpImageFilter);

  PixelType    m_BackgroundValue;
  PixelType    m_ForegroundValue;
  unsigned int m_GridPixSpacing;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGridForwardWarpImageFilter.hxx"
#endif

#endif
