/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlipImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFlipImageFilter_h
#define _itkFlipImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class FlipImageFilter
 * \brief Flips an image across user specified axes.
 *
 * FlipImageFilter flips an image across user specified axes.
 * The flip axes are set via method SetFlipAxes( array ) where
 * the input is a FixedArray<bool,ImageDimension>. The image
 * is flipped across axes for which array[i] is true. 
 * 
 * In terms of grid coordinates the image is flipped within 
 * the LargestPossibleRegion of the input image. As such,
 * the LargestPossibleRegion of the ouput image is the same
 * as the input.
 *
 * In terms of geometric coordinates, the output origin
 * is such that the image is flipped with respect to the
 * coordinate axes. 
 *
 * \ingroup GeometricTransforms
 * \ingroup Multithreaded
 * \ingroup Streamed
 */
template <class TImage>
class ITK_EXPORT FlipImageFilter :
    public ImageToImageFilter<TImage,TImage>
{

public:
  /** Standard class typedefs. */
  typedef FlipImageFilter         Self;
  typedef ImageToImageFilter<TImage,TImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(FlipImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Inherited types */
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType    OutputImageRegionType;

  /** Index related types */
  typedef typename TImage::IndexType IndexType;
  typedef typename IndexType::IndexValueType IndexValueType;

  /** FlipAxesArray type */
  typedef FixedArray<bool,itkGetStaticConstMacro(ImageDimension)> FlipAxesArrayType;

  /** Set/Get the axis to be flipped. The image is flipped along axes
   * for which array[i] is true. */
  itkSetMacro( FlipAxes, FlipAxesArrayType );
  itkGetMacro( FlipAxes, FlipAxesArrayType );

  /** FlipImageFilter produces an image with different originthan the input image.  
   * As such, FlipImageFilter needs to provide an
   * implementation for GenerateOutputInformation() in order to inform
   * the pipeline execution model.  The original documentation of this
   * method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** FlipImageFilter needs different input requested region than the output
   * requested region.  As such, FlipImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

protected:
  FlipImageFilter();
  ~FlipImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** FlipImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );  

private:
  
  FlipAxesArrayType       m_FlipAxes;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFlipImageFilter.txx"
#endif
  
#endif

