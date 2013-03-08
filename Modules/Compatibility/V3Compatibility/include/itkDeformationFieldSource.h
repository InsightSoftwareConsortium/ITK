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
#ifndef __itkDeformationFieldSource_h
#define __itkDeformationFieldSource_h

#include "itkImageSource.h"
#include "itkKernelTransform.h"
#include "itkImageRegionIteratorWithIndex.h"

#ifndef ITKV3_COMPATIBILITY
#error "This file is only valid when ITKV3_COMPATIBILITY is turned on.  Users are encouraged to convert to itkLandmarkDisplacementFieldSource.h in ITKv4"
#endif

namespace itk
{
/** \class DeformationFieldSource
 * \brief Computes a deformation field from two sets of landmarks.
 *
 * DeformationFieldSource produces a deformation field from two set of input
 * landmarks.  One set of landmarks are associated to the input space while the
 * second set of landmarks is associated with the output space.
 *
 * A KernelBase spline is used to interpolate the deformations and produce
 * deformation values for all the nodes of the image grid that will be produced
 * as output.
 *
 * The number of landmarks in the KernelBased spline will have a dramatic
 * effect on both the precision of output deformation field and the
 * computational time required for the filter to complete the estimation.
 *
 *
 * This source object expects the image to be of pixel type Vector.
 *
 * \deprecated
 * \ingroup ImageSource
 * \ingroup ITKV3Compatibility
 */
template< class TOutputImage >
class ITK_EXPORT DeformationFieldSource:
  public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef DeformationFieldSource      Self;
  typedef ImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformationFieldSource, ImageSource);

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Transform typedef.
   *
   *  The KernelBased spline transform types are defined here.
   */
  typedef KernelTransform< double, itkGetStaticConstMacro(ImageDimension) > KernelTransformType;
  typedef typename KernelTransformType::PointSetType                        LandmarkPointSetType;
  typedef typename LandmarkPointSetType::PointType                          LandmarkPointType;
  typedef typename KernelTransformType::Pointer                             KernelTransformPointerType;
  typedef typename KernelTransformType::PointsContainer                     LandmarkContainer;
  typedef typename LandmarkContainer::ConstPointer                          LandmarkContainerPointer;

  /** Image size typedef. */
  typedef typename OutputImageType::SizeType OutputSizeType;

  /** Image index typedef. */
  typedef typename OutputImageType::IndexType OutputIndexType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType    OutputPixelType;
  typedef typename OutputPixelType::ValueType OutputPixelComponentType;

  /** Image spacing typedef */
  typedef typename TOutputImage::SpacingType   SpacingType;
  typedef typename TOutputImage::PointType     OriginPointType;
  typedef typename TOutputImage::DirectionType DirectionType;

  /** Get/Set the coordinate transformation.
   * Set the KernelBase spline used for resampling the deformation grid.
   * */
  itkSetObjectMacro(KernelTransform, KernelTransformType);
  itkGetModifiableObjectMacro(KernelTransform, KernelTransformType);

  /** Set the size of the output image. */
  itkSetMacro(OutputRegion, OutputImageRegionType);

  /** Get the size of the output image. */
  itkGetConstReferenceMacro(OutputRegion, OutputImageRegionType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void SetOutputSpacing(const double *values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OriginPointType);
  virtual void SetOutputOrigin(const double *values);

  /** Set the output direciton cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, OriginPointType);

  /** Set the list of source landmarks */
  itkSetConstObjectMacro(SourceLandmarks, LandmarkContainer);
  itkSetConstObjectMacro(TargetLandmarks, LandmarkContainer);

  /** DeformationFieldSource produces an image which is a different size
   * than its input.  As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** Method Compute the Modified Time based on changed to the components. */
  unsigned long GetMTime(void) const;

protected:
  DeformationFieldSource();
  ~DeformationFieldSource() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /**
   * GenerateData() computes the internal KernelBase spline and resamples
   * the deformation field.
   */
  void GenerateData();

  /** Subsample the input deformation field and generate the
   *  landmarks for the kernel base spline
   */
  void PrepareKernelBaseSpline();

private:
  DeformationFieldSource(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented

  KernelTransformPointerType m_KernelTransform;      // Coordinate transform to
                                                     // use

  OutputImageRegionType m_OutputRegion;              // Region of the output
                                                     // image
  SpacingType     m_OutputSpacing;                   // output image spacing
  OriginPointType m_OutputOrigin;                    // output image origin
  DirectionType   m_OutputDirection;                 // output image direction
                                                     // cosines

  LandmarkContainerPointer m_SourceLandmarks;        // List of source landmarks
  LandmarkContainerPointer m_TargetLandmarks;        // List of target landmarks
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformationFieldSource.hxx"
#endif

#endif
