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
#ifndef itkScalarImageToRunLengthMatrixFilter_h
#define itkScalarImageToRunLengthMatrixFilter_h

#include "itkImage.h"
#include "itkHistogram.h"
#include "itkNumericTraits.h"
#include "itkVectorContainer.h"
#include "itkProcessObject.h"

namespace itk
{
namespace Statistics
{

/** \class ScalarImageToRunLengthMatrixFilter
 *  \brief This class computes a run length matrix (histogram) from
 *  a given image and a mask image if provided. Run length matrces are
 *  used for image texture description.
 *
 * This filters creates a grey-level run length matrix from a N-D scalar
 * image. This is another possible texture description.  See the following
 * references.
 * M. M. Galloway. Texture analysis using gray level run lengths. Computer
 * Graphics and Image Processing, 4:172-179, 1975.
 *
 * A. Chu, C. M. Sehgal, and J. F. Greenleaf. Use of gray value distribution of
 * run lengths for texture analysis.  Pattern Recognition Letters, 11:415-420,
 * 1990.
 *
 * B. R. Dasarathy and E. B. Holder. Image characterizations based on joint
 * gray-level run-length distributions. Pattern Recognition Letters, 12:490-502,
 * 1991.
 *
 * The basic idea is as follows:
 * Given an image and an offset (e.g. (1, -1) for a 2-d image), each element
 * in the joint histogram describes the frequency for a particular distance/
 * intensity pair within a given image.  This distance/intensity pair can be
 * described as follows: we start at a given voxel which has some intensity.
 * We then "jump" to neighboring pixels in increments provided by the offset(s)
 * as long as the pixel to which we are jumping is within the same intensity
 * bin as the original voxel.  The distance component is given by the distance
 * from the original to the final voxel satisfying our jumping criteria.
 *
 * The offset (or offsets) along which the co-occurences are calculated can be
 * set by the user. Traditionally, only one offset is used per histogram, and
 * offset components in the range [-1, 1] are used. For rotation-invariant
 * features averages of features computed over several histograms with different
 * offsets are generally used, instead of computing features from one histogram
 * create with several offsets. Additionally, instead of using offsets of two or
 * more pixels in any direction, multi-resolution techniques (e.g. image
 * pyramids) are generally used to deal with texture at different spatial
 * resolutions.
 *
 * This class calculates a 2-d histogram of all the intensity/distance pairs in
 * the given image's requested region, for a given set of offsets. That is, if
 * a given offset falls outside of the requested region (or outside the mask)
 * at a particular point, that distance/intensity pair will not be added to
 * the matrix.
 *
 * The number of histogram bins on each axis can be set (defaults to 256). Also,
 * by default the histogram min and max corresponds to the largest and smallest
 * possible pixel value of that pixel type. To customize the histogram bounds
 * for a given image, the max and min pixel values that will be placed in the
 * histogram can be set manually. NB: The min and max are INCLUSIVE.
 *
 * Further, the type of histogram frequency container used is an optional
 * template parameter. By default, a dense container is used, but for images
 * with little texture or in cases where the user wants more histogram bins,
 * a sparse container can be used for the histogram instead.
 *
 * WARNING: This probably won't work for pixels of double or long-double type
 * unless you set the histogram min and max manually. This is because the largest
 * histogram bin by default has max value of the largest possible pixel value
 * plus 1. For double and long-double types, whose "RealType" as defined by the
 * NumericTraits class is the same, and thus cannot hold any larger values,
 * this would cause a float overflow.
 *
 * IJ article: https://hdl.handle.net/1926/1374
 *
 * \sa ScalarImageToRunLengthFeaturesFilter
 * \sa ScalarImageToRunLengthMatrixFilter
 * \sa HistogramToRunLengthFeaturesFilter
 *
 * \author: Nick Tustison
 * \ingroup ITKStatistics
 */

template<typename TImageType, typename THistogramFrequencyContainer =
  DenseFrequencyContainer2>
class ITK_TEMPLATE_EXPORT ScalarImageToRunLengthMatrixFilter : public ProcessObject
{
public:
  /** Standard typedefs */
  typedef ScalarImageToRunLengthMatrixFilter  Self;
  typedef ProcessObject                       Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( ScalarImageToRunLengthMatrixFilter, ProcessObject );

  /** standard New() method support */
  itkNewMacro( Self );

  typedef TImageType                                      ImageType;
  typedef typename ImageType::Pointer                     ImagePointer;
  typedef typename ImageType::ConstPointer                ImageConstPointer;
  typedef typename ImageType::PixelType                   PixelType;
  typedef typename ImageType::IndexType                   IndexType;
  typedef typename ImageType::RegionType                  RegionType;
  typedef typename ImageType::SizeType                    RadiusType;
  typedef typename ImageType::OffsetType                  OffsetType;
  typedef VectorContainer<unsigned char, OffsetType>      OffsetVector;
  typedef typename OffsetVector::Pointer                  OffsetVectorPointer;
  typedef typename ImageType::PointType                   PointType;

  typedef typename NumericTraits<PixelType>::RealType     MeasurementType;
  typedef typename NumericTraits<PixelType>::RealType     RealType;

  typedef Histogram<MeasurementType, THistogramFrequencyContainer>
                                                          HistogramType;
  typedef typename HistogramType::Pointer                 HistogramPointer;
  typedef typename HistogramType::ConstPointer            HistogramConstPointer;
  typedef typename HistogramType::MeasurementVectorType   MeasurementVectorType;

  /** ImageDimension constants */
  itkStaticConstMacro( ImageDimension, unsigned int,
    TImageType::ImageDimension );

  /** Specify the default number of bins per axis */
  itkStaticConstMacro( DefaultBinsPerAxis, unsigned int, 256 );

  /**
   * Set the offsets over which the intensity/distance pairs will be computed.
   * Invoking this function clears the previous offsets.
   * Note: for each individual offset in the OffsetVector, the rightmost non-zero
   * offset element must be positive. For example, in the offset list of a 2D image,
   * (1, 0) means the offset  along x-axis. (1, 0) has to be set instead
   * of (-1, 0). This is required from the iterating order of pixel iterator.
   *
   */
  itkSetObjectMacro( Offsets, OffsetVector );

  /**
   * Set offset over which the intensity/distance pairs will be computed.
   * Invoking this function clears the previous offset(s).
   * Note: for each individual offset, the rightmost non-zero
   * offset element must be positive. For example, in the offset list of a 2D image,
   * (1, 0) means the offset  along x-axis. (1, 0) has to be set instead
   * of (-1, 0). This is required from the iterating order of pixel iterator.
   *
   */
  void SetOffset( const OffsetType offset );

  /**
   * Get the current offset(s).
   */
  itkGetModifiableObjectMacro(Offsets, OffsetVector );

  /** Set number of histogram bins along each axis */
  itkSetMacro( NumberOfBinsPerAxis, unsigned int );

  /** Get number of histogram bins along each axis */
  itkGetConstMacro( NumberOfBinsPerAxis, unsigned int );

  /**
   * Set the min and max (inclusive) pixel value that will be used in
   * generating the histogram.
   */
  void SetPixelValueMinMax( PixelType min, PixelType max );

  /** Get the min pixel value defining one dimension of the joint histogram. */
  itkGetConstMacro( Min, PixelType );

  /** Get the max pixel value defining one dimension of the joint histogram. */
  itkGetConstMacro( Max, PixelType );

  /**
   * Set the min and max (inclusive) pixel value that will be used in
   * generating the histogram.
   */
  void SetDistanceValueMinMax( RealType min, RealType max );

  /**
   * Get the min distance value defining one dimension of the joint histogram.
   */
  itkGetConstMacro( MinDistance, RealType );

  /**
   * Get the max distance value defining one dimension of the joint histogram.
   */
  itkGetConstMacro( MaxDistance, RealType );

  /** Method to set the input image */
  using Superclass::SetInput;
  void SetInput( const ImageType *image );

  /** Method to get the input image */
  const ImageType * GetInput() const;

  /** Method to set the mask image */
  void SetMaskImage( const ImageType *image );

  /** Method to get the mask image */
  const ImageType * GetMaskImage() const;

  /** method to get the Histogram */
  const HistogramType * GetOutput() const;

  /**
   * Set the pixel value of the mask that should be considered "inside" the
   * object. Defaults to 1.
   */
  itkSetMacro( InsidePixelValue, PixelType );
  itkGetConstMacro( InsidePixelValue, PixelType );

protected:
  ScalarImageToRunLengthMatrixFilter();
  virtual ~ScalarImageToRunLengthMatrixFilter() ITK_OVERRIDE {};
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** Standard itk::ProcessObject subclass method. */
  typedef DataObject::Pointer DataObjectPointer;

  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput( DataObjectPointerArraySizeType idx ) ITK_OVERRIDE;

  /** This method causes the filter to generate its output. */
  virtual void GenerateData() ITK_OVERRIDE;

  /**
   * Normalize the direction of the offset before it is applied.
   * The last non-zero dimension of the offest has to be positive in order
   * to match to scanning order of the iterator. Only the sign is changed.
   * For example, the input offset (-1, 0) will be normalized as
   * (1, 0).
   * */
  void NormalizeOffsetDirection(OffsetType &offset);

private:

  unsigned int             m_NumberOfBinsPerAxis;
  PixelType                m_Min;
  PixelType                m_Max;
  RealType                 m_MinDistance;
  RealType                 m_MaxDistance;
  PixelType                m_InsidePixelValue;

  MeasurementVectorType    m_LowerBound;
  MeasurementVectorType    m_UpperBound;
  OffsetVectorPointer      m_Offsets;

};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarImageToRunLengthMatrixFilter.hxx"
#endif

#endif
