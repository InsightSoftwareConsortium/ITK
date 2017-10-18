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
#ifndef itkWarpVectorImageFilter_h
#define itkWarpVectorImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkPoint.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class WarpVectorImageFilter
 * \brief Warps an image using an input displacement field.
 *
 * WarpVectorImageFilter warps an existing image with respect to
 * a given displacement field. The input image is expected to have
 * vector-like pixel types.
 *
 * A displacement field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the input image. The vector type must support element access via operator
 * [].
 *
 * The output image is produced by inverse mapping: the output pixels
 * are mapped back onto the input image. This scheme avoids the creation of
 * any holes and overlaps in the output image.
 *
 * Each vector in the displacement field represent the distance between
 * a geometric point in the input space and a point in the output space such
 * that:
 *
 * \f[ p_{in} = p_{out} + d \f]
 *
 * Typically the mapped position does not correspond to an integer pixel
 * position in the input image. Interpolation via an image function
 * is used to compute values at non-integer positions. The default
 * interpolation typed used is the LinearInterpolateImageFunction.
 * The user can specify a particular interpolation function via
 * SetInterpolator(). Note that the input interpolator must derive
 * from base class InterpolateImageFunction.
 *
 * Position mapped to outside of the input image buffer are assigned
 * a edge padding value.
 *
 * The LargetPossibleRegion for the output is inherited
 * from the input displacement field. The output image
 * spacing, origin and direction may be set via
 * SetOutputSpacing, SetOutputOrigin and
 * SetOutputDirection. The default are respectively a
 * vector of 1's, a vector of 0's and an identity matrix.
 *
 * This class is templated over the type of the input image, the
 * type of the output image and the type of the displacement field.
 *
 * The input image is set via SetInput. The input displacement field
 * is set via SetDisplacementField.
 *
 * This filter is implemented as a multithreaded filter.
 *
 * \warning This filter assumes that the input type, output type
 * and displacement field type all have the same number of dimensions.
 *
 * \warning WarpVectorImageFilter works only with input images
 * of the type Image< Vector< T, D >, N > and not VectorImage< T, N >.
 *
 * \author  Corinne Mattmann
 *
 * \ingroup GeometricTransform MultiThreaded
 * \ingroup ITKImageGrid
 */
template<
  typename TInputImage,
  typename TOutputImage,
  typename TDisplacementField
  >
class ITK_TEMPLATE_EXPORT WarpVectorImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef WarpVectorImageFilter                           Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(WarpVectorImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Inherit some types from the superclass. */
  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::OutputImageType        OutputImageType;
  typedef typename Superclass::OutputImagePointer     OutputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

  typedef typename OutputImageType::IndexType            IndexType;
  typedef typename OutputImageType::SizeType             SizeType;
  typedef typename OutputImageType::PixelType            PixelType;
  typedef typename OutputImageType::SpacingType          SpacingType;
  typedef typename OutputImageType::PixelType::ValueType ValueType;

  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Dimension of the Vector pixel type. */
  itkStaticConstMacro(PixelDimension, unsigned int,
                      PixelType::Dimension);

  /** Displacement field typedef support. */
  typedef TDisplacementField                        DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer   DisplacementFieldPointer;
  typedef typename DisplacementFieldType::PixelType DisplacementType;

#ifdef ITKV3_COMPATIBILITY
  typedef TDisplacementField                       DeformationFieldType;
  typedef typename DeformationFieldType::Pointer   DeformationFieldPointer;
  typedef typename DeformationFieldType::PixelType DeformationType;
#endif

  /** Interpolator typedef support. */
  typedef double                                                         CoordRepType;
  typedef VectorInterpolateImageFunction< InputImageType, CoordRepType > InterpolatorType;
  typedef typename InterpolatorType::Pointer                             InterpolatorPointer;
  typedef VectorLinearInterpolateImageFunction< InputImageType, CoordRepType >
  DefaultInterpolatorType;

  /** Point type */
  typedef Point< CoordRepType, itkGetStaticConstMacro(ImageDimension) > PointType;

  /** Type for representing the direction of the output image */
  typedef typename TOutputImage::DirectionType DirectionType;

  /** Set the displacement field. */
  void SetDisplacementField(const DisplacementFieldType *field);

  /** Set the displacement field (non const for backward compatibility). */
  void SetDisplacementField(DisplacementFieldType *field);

  /** Get a pointer the displacement field. */
  DisplacementFieldType * GetDisplacementField();

#ifdef ITKV3_COMPATIBILITY
  void SetDeformationField(const DeformationFieldType *field)
  {
    this->SetDisplacementField(field);
  }

  void SetDeformationField(DeformationFieldType *field)
  {
    this->SetDisplacementField(field);
  }

  DeformationFieldType * GetDeformationField(void)
  {
    return static_cast<DeformationFieldType *> (GetDisplacementField());
  }
#endif

  /** Get/Set the interpolator function. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void SetOutputSpacing(const double *values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, PointType);
  virtual void SetOutputOrigin(const double *values);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, PointType);

  /** Set/Get the direction (orientation) of the output image */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Set the edge padding value */
  itkSetMacro(EdgePaddingValue, PixelType);

  /** Get the edge padding value */
  itkGetConstMacro(EdgePaddingValue, PixelType);

  /** WarpVectorImageFilter produces an image which is a different
   * size than its input image. As such, it needs to provide an
   * implementation for GenerateOutputInformation() which set
   * the output information according the OutputSpacing, OutputOrigin
   * and the displacement field's LargestPossibleRegion. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** It is difficult to compute in advance the input image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole input image.
   *
   * For the displacement field, the input requested region
   * set to be the same as that of the output requested region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** This method is used to set the state of the filter before
   * multi-threading. */
  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< ValueType > ) );
  itkConceptMacro( DisplacementFieldHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TDisplacementField::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  WarpVectorImageFilter();
  ~WarpVectorImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** WarpVectorImageFilter is implemented as a multi-threaded filter.
   * As such, it needs to provide and implementation for
   * ThreadedGenerateData(). */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WarpVectorImageFilter);

  PixelType     m_EdgePaddingValue;
  SpacingType   m_OutputSpacing;
  PointType     m_OutputOrigin;
  DirectionType m_OutputDirection;

  InterpolatorPointer m_Interpolator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWarpVectorImageFilter.hxx"
#endif

#endif
