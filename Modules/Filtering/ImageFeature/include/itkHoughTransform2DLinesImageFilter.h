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
#ifndef itkHoughTransform2DLinesImageFilter_h
#define itkHoughTransform2DLinesImageFilter_h


#include "itkImageToImageFilter.h"
#include "itkLineSpatialObject.h"

namespace itk
{
/**
 * \class HoughTransform2DLinesImageFilter
 * \brief Performs the Hough Transform to find 2D straight lines
 *        in a 2D image.
 *
 * This filter derives from ImageToImageFilter.
 * The input is an image, and all pixels above some threshold are those
 * to be extracted. The output is the image of the accumulator.
 * GetLines() returns a list of LinesSpatialObjects.
 *
 * Lines are parameterized in the form:
 *
 * \f$ R = x \cos(\theta) + y \sin(\theta) \f$
 * where \f$R\f$ is the perpendicular distance from the origin and
 * \f$\theta\f$ the angle with the normal.
 *
 * The output is the accumulator array:
 *    -The first dimension (X) represents the distance R from the corner
 *     to the line
 *    -The second dimension (Y) represents the angle between the X axis
 *     and the normal to the line.
 *
 * The size of the array depends on the AngleAxisSize that could be set
 * (500 by default) for the angle axis. The distance axis depends on the
 * size of the diagonal of the input image.
 *
 * \ingroup ImageFeatureExtraction
 * \sa LineSpatialObject
 *
 * \ingroup ITKImageFeature
 *
 * \wiki
 * \wikiexample{Conversions/HoughTransform2DLinesImageFilter,HoughTransform2DLinesImageFilter}
 * \endwiki
 */

template< typename TInputPixelType, typename TOutputPixelType >
class ITK_TEMPLATE_EXPORT HoughTransform2DLinesImageFilter:
  public ImageToImageFilter< Image< TInputPixelType, 2 >, Image< TOutputPixelType, 2 > >
{
public:

  /** Standard "Self" typedef. */
  typedef HoughTransform2DLinesImageFilter Self;

  /** Input Image typedef */
  typedef Image< TInputPixelType, 2 >           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  /** Output Image typedef */
  typedef Image< TOutputPixelType, 2 >      OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Smart pointer typedef support. */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Line typedef */
  typedef LineSpatialObject< 2 >     LineType;
  typedef typename LineType::Pointer LinePointer;
  typedef std::list< LinePointer >   LinesListType;
  typedef LineType::LinePointType    LinePointType;

  typedef typename LinesListType::size_type LinesListSizeType;

  /** Standard "Superclass" typedef. */
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;

  /** Image index typedef */
  typedef typename InputImageType::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename InputImageType::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename InputImageType::RegionType OutputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HoughTransform2DLinesImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for evaluating the implicit function over the image. */
  void GenerateData() ITK_OVERRIDE;

  /** Set the threshold above which the filter should consider
      the point as a valid point */
  itkSetMacro(Threshold, float);

  /** Get the threshold value */
  itkGetConstMacro(Threshold, float);

  /** Set the resolution angle:
      The hough space descibes (in the angle direction) [-PI,PI[
      with a constant stepe AngleResolution */
  itkSetMacro(AngleResolution, float);

  /** Get the resolution angle */
  itkGetConstMacro(AngleResolution, float);

  /** Simplify the accumulator */
  void Simplify();

  /** Get the Simplified accumulator */
  itkGetModifiableObjectMacro(SimplifyAccumulator, OutputImageType);

  /** Get the list of lines. This recomputes the lines */
  LinesListType & GetLines();

  /** Get the list of lines.
  * \deprecated Use GetLines() without arguments instead! */
  itkLegacyMacro(LinesListType & GetLines(unsigned int n));

  /** Set/Get the number of lines to extract */
  itkSetMacro(NumberOfLines, LinesListSizeType);
  itkGetConstMacro(NumberOfLines, LinesListSizeType);

  /** Set/Get the radius of the disc to remove from the accumulator
   *  for each line found */
  itkSetMacro(DiscRadius, float);
  itkGetConstMacro(DiscRadius, float);

  /** Set the variance of the gaussian bluring for the accumulator */
  itkSetMacro(Variance, float);
  itkGetConstMacro(Variance, float);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( IntConvertibleToOutputCheck,
                   ( Concept::Convertible< int, TOutputPixelType > ) );
  itkConceptMacro( InputGreaterThanFloatCheck,
                   ( Concept::GreaterThanComparable< PixelType, float > ) );
  itkConceptMacro( OutputPlusIntCheck,
                   ( Concept::AdditiveOperators< TOutputPixelType, int > ) );
  // End concept checking
#endif

protected:

  HoughTransform2DLinesImageFilter();
  virtual ~HoughTransform2DLinesImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** HoughTransform2DLinesImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** HoughTransform2DLinesImageFilter's output is the accumulator
   * array.  The size of the output is a function of the size of the
   * input and the AngleAxisSize. Since this output is a different
   * size than the input, it must provide an implementation of
   * GenerateOutputInformation.
   * \sa ProcessObject::GenerateOutputRequestedRegion() */
  void GenerateOutputInformation() ITK_OVERRIDE;

  /** HoughTransform2DLinesImageFilter must produce the entire output */
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

private:

  HoughTransform2DLinesImageFilter(const Self &);
  void operator=(const Self &);

  float              m_AngleResolution;
  float              m_Threshold;
  OutputImagePointer m_SimplifyAccumulator;
  LinesListType      m_LinesList;
  LinesListSizeType  m_NumberOfLines;
  float              m_DiscRadius;
  float              m_Variance;
  ModifiedTimeType   m_OldModifiedTime;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHoughTransform2DLinesImageFilter.hxx"
#endif

#endif
