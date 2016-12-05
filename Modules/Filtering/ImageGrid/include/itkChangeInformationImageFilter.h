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
#ifndef itkChangeInformationImageFilter_h
#define itkChangeInformationImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class ChangeInformationImageFilter
 * \brief Change the origin, spacing and/or region of an Image.
 *
 * Change the origin, spacing, direction and/or buffered region of an itkImage. This
 * "Information" along with an Image's container comprise the
 * itkImage. By default, the output's information is set to the
 * input's information. The methods ChangeSpacingOn/Off,
 * ChangeOriginOn/Off, ChangeDirectionOn/Off  and ChangeRegionOn/Off
 * control whether the default origin, spacing, direction or buffered
 * region should be changed. If On, the associated information will be
 * replaced with either the ReferenceImage information (if
 * UseReferenceImage is true) or the ivars OutputSpacing,
 * OutputOrigin, OutputDirection, OutputOffset.
 *
 * In addition, the method CenterImageOn will recompute the output image
 * origin (using the selected output spacing) the align the center of the
 * image with the coordinate 0.
 *
 * \ingroup GeometricTransform
 *
 * \ingroup ITKImageGrid
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT ChangeInformationImageFilter:
  public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef ChangeInformationImageFilter                   Self;
  typedef ImageToImageFilter< TInputImage, TInputImage > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef TInputImage   InputImageType;
  typedef TInputImage   OutputImageType;

  /** Typedef to describe the output and input image region types. */
  typedef typename OutputImageType::RegionType  OutputImageRegionType;
  typedef typename InputImageType::RegionType   InputImageRegionType;

  /** Typedef to describe the pointer to the input. */
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  /** Typedef to describe the type of pixel. */
  typedef typename OutputImageType::PixelType OutputImagePixelType;
  typedef typename InputImageType::PixelType  InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename OutputImageType::IndexType        OutputImageIndexType;
  typedef typename OutputImageType::SizeType         OutputImageSizeType;
  typedef typename OutputImageType::OffsetType       OutputImageOffsetType;
  typedef typename OutputImageType::DirectionType    OutputImageDirectionType;
  typedef typename OutputImageType::OffsetValueType  OutputImageOffsetValueType;
  typedef typename InputImageType::IndexType         InputImageIndexType;
  typedef typename InputImageType::SizeType          InputImageSizeType;
  typedef typename InputImageType::OffsetType        InputImageOffsetType;
  typedef typename InputImageType::DirectionType     InputImageDirectionType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Image spacing, origin and direction typedefs */
  typedef typename InputImageType::SpacingType   SpacingType;
  typedef typename InputImageType::PointType     PointType;
  typedef typename InputImageType::DirectionType DirectionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChangeInformationImageFilter, ImageToImageFilter);

  /** Copy the information from another Image.  By default,
   *  the information is copied from the input image. */
  void SetReferenceImage(InputImageType *image)
  {
    if ( image != m_ReferenceImage )
      {
      m_ReferenceImage = image;
      this->ProcessObject::SetNthInput(1, const_cast< InputImageType *>( image ) );
      this->Modified();
      }
  }

  itkGetModifiableObjectMacro(ReferenceImage, TInputImage);

  itkSetMacro(UseReferenceImage, bool);
  itkBooleanMacro(UseReferenceImage);
  itkGetConstMacro(UseReferenceImage, bool);

  /** Specify a new data spacing explicitly.  The default is to
   * use the spacing of the Input, or of the ReferenceImage
   * if UseReferenceImage is set. */
  itkSetMacro(OutputSpacing, SpacingType);
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Specify a new data origin explicitly.  The default is to
   *  use the origin of the Input, or of the ReferenceImage
   *  if UseReferenceImage is true. */
  itkSetMacro(OutputOrigin, PointType);
  itkGetConstReferenceMacro(OutputOrigin, PointType);

  /** Specify a new direciton cosine matrix explicitly.  The default is to
   *  use the direction of the Input, or of the ReferenceImage
   *  if UseReferenceImage is true. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Specify an offset for the buffered region. The default is to
   *  use the same buffered region as the input or an Offset computed from
   *  the ReferenceImage's buffered region (if UseReferenceImage is true.)
   *  NOTE: Changing the buffered region should not be done without a
   *  corresponding change in the requested region. Of course, the pipeline
   *  controls the requested region. Therefore, changing the buffered region
   *  may mean the filter cannot produce the requested region.
   */
  itkSetMacro(OutputOffset, OutputImageOffsetType);
  itkGetConstReferenceMacro(OutputOffset, OutputImageOffsetType);
  itkSetVectorMacro(OutputOffset, OutputImageOffsetValueType, ImageDimension);

  /** Change the origin, spacing and region of the output image. */
  void ChangeAll()
  {
    this->ChangeSpacingOn();
    this->ChangeOriginOn();
    this->ChangeDirectionOn();
    this->ChangeRegionOn();
  }

  /** Do not change the origin, spacing, direction or region of the
   * output image. */
  void ChangeNone()
  {
    this->ChangeSpacingOff();
    this->ChangeOriginOff();
    this->ChangeDirectionOff();
    this->ChangeRegionOff();
  }

  /** Change the Spacing of the output image. If false, the output
   *  image spacing will be set to the input image spacing. If true, the
   *  output image spacing will be set to:
   *      the ReferenceImage spacing (if UseReferenceImage is true) or
   *      OutputSpacing. */

  itkSetMacro(ChangeSpacing, bool);
  itkBooleanMacro(ChangeSpacing);
  itkGetConstMacro(ChangeSpacing, bool);

  /** Change the Origin of the output image. If false, the output
   *  image origin will be set to the input image origin. If true, the
   *  output image origin will be set to:
   *      the ReferenceImage origin (if UseReferenceImage is true) or
   *      OutputOrigin. */

  itkSetMacro(ChangeOrigin, bool);
  itkBooleanMacro(ChangeOrigin);
  itkGetConstMacro(ChangeOrigin, bool);

  /** Change the direction of the output image. If false, the output
   *  image direction will be set to the input image direction. If true, the
   *  output image direction will be set to:
   *  the ReferenceImage direction (if UseReferenceImage is true) or
   *  OutputDirection. */

  itkSetMacro(ChangeDirection, bool);
  itkBooleanMacro(ChangeDirection);
  itkGetConstMacro(ChangeDirection, bool);

  /** Change the BufferedRegion of the output image. */

  itkSetMacro(ChangeRegion, bool);
  itkBooleanMacro(ChangeRegion);
  itkGetConstMacro(ChangeRegion, bool);

  /** Set the Origin of the output so that image coordinate (0,0,0)
   * lies at the Center of the Image.  This will override
   * SetOutputOrigin. */
  itkSetMacro(CenterImage, bool);
  itkBooleanMacro(CenterImage);
  itkGetConstMacro(CenterImage, bool);

  /** Apply changes to the output image information. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** Apply changes to the input image requested region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Copy the input buffer. */
  void GenerateData() ITK_OVERRIDE;

protected:
  ChangeInformationImageFilter();
  //~ChangeInformationImageFilter() {} default implementation ok

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Override VeriyInputInformation() since this filter's inputs do
   * not need to occoupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  virtual void VerifyInputInformation() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ChangeInformationImageFilter);

  InputImagePointer m_ReferenceImage;

  bool m_CenterImage;
  bool m_ChangeSpacing;
  bool m_ChangeOrigin;
  bool m_ChangeDirection;
  bool m_ChangeRegion;
  bool m_UseReferenceImage;

  SpacingType   m_OutputSpacing;
  PointType     m_OutputOrigin;
  DirectionType m_OutputDirection;

  OutputImageOffsetType m_OutputOffset;
  OutputImageOffsetType m_Shift;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChangeInformationImageFilter.hxx"
#endif

#endif
