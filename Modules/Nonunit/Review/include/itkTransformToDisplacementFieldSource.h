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
#ifndef __itkTransformToDisplacementFieldSource_h
#define __itkTransformToDisplacementFieldSource_h

#include "itkTransform.h"
#include "itkImageSource.h"

namespace itk
{
/** \class TransformToDisplacementFieldSource
 * \brief Generate a displacement field from a coordinate transform
 *
 * Output information (spacing, size and direction) for the output
 * image should be set. This information has the normal defaults of
 * unit spacing, zero origin and identity direction. Optionally, the
 * output information can be obtained from a reference image. If the
 * reference image is provided and UseReferenceImage is On, then the
 * spacing, origin and direction of the reference image will be used.
 *
 * Since this filter produces an image which is a different size than
 * its input, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \author Marius Staring, Leiden University Medical Center, The Netherlands.
 *
 * This class was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/1387
 *
 * \ingroup GeometricTransform
 * \ingroup ITKReview
 */
template< class TOutputImage,
          class TTransformPrecisionType = double >
class ITK_EXPORT TransformToDisplacementFieldSource:
  public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TransformToDisplacementFieldSource Self;
  typedef ImageSource< TOutputImage >        Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformToDisplacementFieldSource, ImageSource);

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Typedefs for transform. */
  typedef Transform< TTransformPrecisionType,
                     itkGetStaticConstMacro(ImageDimension),
                     itkGetStaticConstMacro(ImageDimension) >     TransformType;
  typedef typename TransformType::ConstPointer TransformPointerType;

  /** Typedefs for output image. */
  typedef typename OutputImageType::PixelType     PixelType;
  typedef typename PixelType::ValueType           PixelValueType;
  typedef typename OutputImageType::RegionType    RegionType;
  typedef typename RegionType::SizeType           SizeType;
  typedef typename OutputImageType::IndexType     IndexType;
  typedef typename OutputImageType::PointType     PointType;
  typedef typename OutputImageType::SpacingType   SpacingType;
  typedef typename OutputImageType::PointType     OriginType;
  typedef typename OutputImageType::DirectionType DirectionType;

  /** Typedefs for base image. */
  typedef ImageBase< itkGetStaticConstMacro(ImageDimension) > ImageBaseType;

  /** Get/Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this must
   * be in physical coordinates and it is the output-to-input transform, NOT
   * the input-to-output transform that you might naively expect.  By default
   * the filter uses an Identity transform. You must provide a different
   * transform here, before attempting to run the filter, if you do not want to
   * use the default Identity transform. */
  itkSetConstObjectMacro(Transform, TransformType);
  itkGetConstObjectMacro(Transform, TransformType);

  /** Set the size of the output image. */
  virtual void SetOutputSize(const SizeType & size);

  /** Get the size of the output image. */
  virtual const SizeType & GetOutputSize();

  /** Set the start index of the output largest possible region.
  * The default is an index of all zeros. */
  virtual void SetOutputIndex(const IndexType & index);

  /** Get the start index of the output largest possible region. */
  virtual const IndexType & GetOutputIndex();

  /** Set the region of the output image. */
  itkSetMacro(OutputRegion, OutputImageRegionType);

  /** Get the region of the output image. */
  itkGetConstReferenceMacro(OutputRegion, OutputImageRegionType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void SetOutputSpacing(const double *values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OriginType);
  virtual void SetOutputOrigin(const double *values);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, OriginType);

  /** Set the output direction cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Helper method to set the output parameters based on this image */
  void SetOutputParametersFromImage(const ImageBaseType *image);

  /** DisplacementFieldImageFilter produces a vector image. */
  virtual void GenerateOutputInformation(void);

  /** Just checking if transform is set. */
  virtual void BeforeThreadedGenerateData(void);

  /** Compute the Modified Time based on changes to the components. */
  ModifiedTimeType GetMTime(void) const;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkStaticConstMacro(PixelDimension, unsigned int,
                      PixelType::Dimension);
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, PixelDimension > ) );
  /** End concept checking */
#endif

protected:
  TransformToDisplacementFieldSource(void);
  ~TransformToDisplacementFieldSource(void) {}

  void PrintSelf(std::ostream & os, Indent indent) const;

  /** TransformToDisplacementFieldSource can be implemented as a multithreaded
   * filter.
   */
  void ThreadedGenerateData(
    const OutputImageRegionType & outputRegionForThread,
    ThreadIdType threadId);

  /** Default implementation for resampling that works for any
   * transformation type.
   */
  void NonlinearThreadedGenerateData(
    const OutputImageRegionType & outputRegionForThread,
    ThreadIdType threadId);

  /** Faster implementation for resampling that works for with linear
   *  transformation types.
   */
  void LinearThreadedGenerateData(
    const OutputImageRegionType & outputRegionForThread,
    ThreadIdType threadId);

private:

  TransformToDisplacementFieldSource(const Self &); //purposely not implemented
  void operator=(const Self &);                    //purposely not implemented

  /** Member variables. */
  RegionType           m_OutputRegion;    // region of the output image
  TransformPointerType m_Transform;       // Coordinate transform to use
  SpacingType          m_OutputSpacing;   // output image spacing
  OriginType           m_OutputOrigin;    // output image origin
  DirectionType        m_OutputDirection; // output image direction cosines
};                                        // end class
                                          // TransformToDisplacementFieldSource
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformToDisplacementFieldSource.hxx"
#endif

#endif // end #ifndef __itkTransformToDisplacementFieldSource_h
