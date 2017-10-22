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
#ifndef itkLandmarkDisplacementFieldSource_h
#define itkLandmarkDisplacementFieldSource_h

#include "itkImageSource.h"
#include "itkKernelTransform.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class LandmarkDisplacementFieldSource
 * \brief Computes a displacement field from two sets of landmarks.
 *
 * LandmarkDisplacementFieldSource produces a displacement field from two set of input
 * landmarks.  One set of landmarks are associated to the input space while the
 * second set of landmarks is associated with the output space.
 *
 * A KernelBase spline is used to interpolate the displacements and produce
 * displacement values for all the nodes of the image grid that will be produced
 * as output.
 *
 * The number of landmarks in the KernelBased spline will have a dramatic
 * effect on both the precision of output displacement field and the
 * computational time required for the filter to complete the estimation.
 *
 *
 * This source object expects the image to be of pixel type Vector.
 *
 * \ingroup ImageSource
 * \ingroup ITKDisplacementField
 */
template< typename TOutputImage >
class ITK_TEMPLATE_EXPORT LandmarkDisplacementFieldSource:
  public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LandmarkDisplacementFieldSource Self;
  typedef ImageSource< TOutputImage >     Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LandmarkDisplacementFieldSource, ImageSource);

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
   * Set the KernelBase spline used for resampling the displacement grid.
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

  /** LandmarkDisplacementFieldSource produces an image which is a different size
   * than its input.  As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  /** Method Compute the Modified Time based on changed to the components. */
  ModifiedTimeType GetMTime(void) const ITK_OVERRIDE;

protected:
  LandmarkDisplacementFieldSource();
  ~LandmarkDisplacementFieldSource() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * GenerateData() computes the internal KernelBase spline and resamples
   * the displacement field.
   */
  void GenerateData() ITK_OVERRIDE;

  /** Subsample the input displacement field and generate the
   *  landmarks for the kernel base spline
   */
  void PrepareKernelBaseSpline();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LandmarkDisplacementFieldSource);

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
#include "itkLandmarkDisplacementFieldSource.hxx"
#endif

#endif
