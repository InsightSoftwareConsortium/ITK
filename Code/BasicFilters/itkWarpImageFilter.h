/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkWarpImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkWarpImageFilter_h
#define __itkWarpImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkPoint.h"

namespace itk
{

/** \class WarpImageFilter
 * \brief Warps an image using an input deformation field.
 *
 * WarpImageFilter warps an existing image with respect to
 * a given deformation field.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the input image. The vector type must support element access via operator
 * [].
 *
 * The output image is produced by inverse mapping: the output pixels
 * are mapped back onto the input image. This scheme avoids the creation of
 * any holes and overlaps in the output image.
 *
 * Each vector in the deformation field represent the distance between
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
 * The LargetPossibleRegion for the output is inherited from the 
 * input deformation field. The output image spacing and origin may be set 
 * via SetOutputSpacing, SetOutputOrigin. The default are respectively a 
 * vector of 1's and a vector of 0's.
 *
 * This class is templated over the type of the input image, the
 * type of the output image and the type of the deformation field.
 *
 * The input image is set via SetInput. The input deformation field
 * is set via SetDeformationField.
 *
 * This filter is implemented as a multithreaded filter.
 *
 * \warning This filter assumes that the input type, output type
 * and deformation field type all have the same number of dimensions.
 *
 * \ingroup GeometricTransforms
 */
template <
class TInputImage,
class TOutputImage,
class TDeformationField
>
class ITK_EXPORT WarpImageFilter :
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  
  /**
   * Standard "Self" typedef.
   */
  typedef WarpImageFilter      Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( WarpImageFilter, ImageToImageFilter );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Typedef to describe the output image region type.
   */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /**
   * Inherit some types from the superclass.
   */
  typedef typename Superclass::InputImageType        InputImageType;
  typedef typename Superclass::InputImagePointer     InputImagePointer;
  typedef typename Superclass::OutputImageType       OutputImageType;
  typedef typename Superclass::OutputImagePointer    OutputImagePointer;
  typedef typename OutputImageType::IndexType        IndexType;
  typedef typename OutputImageType::SizeType         SizeType;
  typedef typename OutputImageType::PixelType        PixelType;
  enum { ImageDimension = OutputImageType::ImageDimension };

  /**
   * Deformation field typedef support.
   */
  typedef TDeformationField    DeformationFieldType;
  typedef typename DeformationFieldType::Pointer  DeformationFieldPointer;
  typedef typename DeformationFieldType::PixelType DisplacementType;

  /** 
   * Interpolator typedef support.
   */
  typedef InterpolateImageFunction<InputImageType>   InterpolatorType;
  typedef typename InterpolatorType::Pointer   InterpolatorPointer;
  typedef LinearInterpolateImageFunction<InputImageType>
    DefaultInterpolatorType;

  /**
   * Point type
   */
  typedef Point<double,ImageDimension> PointType;

  /**
   * Set the deformation field.
   */
  void SetDeformationField( DeformationFieldType * field );

  /**
   * Get a pointer the deformation field.
   */
  DeformationFieldPointer GetDeformationField();

  /**
   * Set the interpolator function.
   */
  itkSetObjectMacro( Interpolator, InterpolatorType );

  /**
   * Get a pointer to the interpolator function.
   */
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /**
   * Set the output image spacing.
   */
  virtual void SetOutputSpacing( const double values[ImageDimension] );

  /**
   * Get the output image spacing.
   */
  const double * GetOutputSpacing()
    { return m_OutputSpacing; }

  /**
   * Set the output image origin.
   */
  virtual void SetOutputOrigin( const double values[ImageDimension] );

  /**
   * Get the output image origin.
   */
  const double * GetOutputOrigin()
    { return m_OutputSpacing; }

  /**
   * Set the edge padding value
   */
  itkSetMacro( EdgePaddingValue, PixelType );

  /**
   * Get the edge padding value
   */
  itkGetMacro( EdgePaddingValue, PixelType );


  /**
   * WarpImageFilter produces an image which is a different
   * size than its input image. As such, it needs to provide an
   * implemenation for GenerateOutputInformation() which set
   * the output information according the OutputSpacing, OutputOrigin
   * and the deformation field's LargestPossibleRegion.
   */
  virtual void GenerateOutputInformation();

  /**
   * It is difficult to compute in advance the input image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole input image.
   *
   * For the deformation field, the input requested region
   * set to be the same as that of the output requested region.
   */
  virtual void GenerateInputRequestedRegion();

  /**
   * WarpImageFilter is implemented as a multi-threaded filter.
   * As such, it needs to provide and implementation for 
   * ThreadedGenerateData().
   */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  /**
   * This method is used to set the state of the filter before 
   * multi-threading.
   */
  virtual void BeforeThreadedGenerateData();


protected:

  WarpImageFilter();
  ~WarpImageFilter() {};
  WarpImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  PixelType                  m_EdgePaddingValue;
  double                     m_OutputSpacing[ImageDimension];
  double                     m_OutputOrigin[ImageDimension];

  InterpolatorPointer        m_Interpolator;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWarpImageFilter.txx"
#endif

#endif
