/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeInformationImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkChangeInformationImageFilter_h
#define __itkChangeInformationImageFilter_h

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
 * \ingroup GeometricTransforms
 *
 */
template <class TInputImage>
class ITK_EXPORT ChangeInformationImageFilter:
    public ImageToImageFilter<TInputImage,TInputImage>
{
public:
  /** Standard class typedefs. */
  typedef ChangeInformationImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TInputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Typedef to describe the output and input image region types. */
  typedef typename TInputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType InputImageRegionType;

  /** Typedef to describe the pointer to the input. */  
  typedef typename TInputImage::Pointer InputImagePointer;

  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TInputImage::IndexType OutputImageIndexType;
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TInputImage::SizeType OutputImageSizeType;
  typedef typename TInputImage::SizeType InputImageSizeType;
  typedef typename TInputImage::OffsetType OutputImageOffsetType;
  typedef typename TInputImage::OffsetType InputImageOffsetType;
  typedef typename TInputImage::DirectionType OutputImageDirectionType;
  typedef typename TInputImage::DirectionType InputImageDirectionType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Image spacing, origin and direction typedefs */
  typedef typename TInputImage::SpacingType SpacingType;
  typedef typename TInputImage::PointType   PointType;
  typedef typename TInputImage::DirectionType DirectionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChangeInformationImageFilter, ImageToImageFilter);

  /** Copy the information from another Image.  By default,
   *  the information is copied from the input image. */
  void SetReferenceImage (TInputImage *image)
  {
    if (image != m_ReferenceImage)
      {
      m_ReferenceImage = image;
      this->ProcessObject::SetNthInput(1, image);
      this->Modified();
      }
  }
  itkGetObjectMacro(ReferenceImage, TInputImage);

  itkSetMacro(UseReferenceImage, bool);
  itkBooleanMacro(UseReferenceImage);
  itkGetMacro(UseReferenceImage, bool);

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
  itkSetVectorMacro(OutputOffset, long, ImageDimension);
  itkGetVectorMacro(OutputOffset, const long, ImageDimension);

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
  itkGetMacro(ChangeSpacing, bool);

  /** Change the Origin of the output image. If false, the output
   *  image origin will be set to the input image origin. If true, the
   *  output image origin will be set to:
   *      the ReferenceImage origin (if UseReferenceImage is true) or
   *      OutputOrigin. */

  itkSetMacro(ChangeOrigin, bool);
  itkBooleanMacro(ChangeOrigin);
  itkGetMacro(ChangeOrigin, bool);

  /** Change the direction of the output image. If false, the output
   *  image direction will be set to the input image direction. If true, the
   *  output image direction will be set to:
   *  the ReferenceImage direction (if UseReferenceImage is true) or
   *  OutputDirection. */

  itkSetMacro(ChangeDirection, bool);
  itkBooleanMacro(ChangeDirection);
  itkGetMacro(ChangeDirection, bool);

  /** Change the BufferedRegion of the output image. */

  itkSetMacro(ChangeRegion, bool);
  itkBooleanMacro(ChangeRegion);
  itkGetMacro(ChangeRegion, bool);

  /** Set the Origin of the output so that image coordinate (0,0,0)
   * lies at the Center of the Image.  This will override 
   * SetOutputOrigin. */
  itkSetMacro(CenterImage, bool);
  itkBooleanMacro(CenterImage);
  itkGetMacro(CenterImage, bool);

  /** Apply changes to the output image information. */
  virtual void GenerateOutputInformation();

  /** Apply changes to the input image requested region. */
  virtual void GenerateInputRequestedRegion();

  /** Copy the input buffer. */
  void GenerateData();

protected:
  ChangeInformationImageFilter();
  ~ChangeInformationImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ChangeInformationImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputImagePointer m_ReferenceImage;

  bool m_CenterImage;
  bool m_ChangeSpacing;
  bool m_ChangeOrigin;
  bool m_ChangeDirection;
  bool m_ChangeRegion;
  bool m_UseReferenceImage;
  
  SpacingType m_OutputSpacing;
  PointType m_OutputOrigin;
  DirectionType m_OutputDirection;

  long m_OutputOffset[ImageDimension];
  OutputImageOffsetType m_Shift;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChangeInformationImageFilter.txx"
#endif
  
#endif
