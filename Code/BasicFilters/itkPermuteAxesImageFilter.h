/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPermuteAxesImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPermuteAxesImageFilter_h
#define _itkPermuteAxesImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class PermuteAxesImageFilter
 * \brief Permutes the image axes according to a user specified order.
 *
 * PermuateAxesImageFilter permutes the image axes according to a
 * user specified order. The permutation order is set via method
 * SetOrder( order ) where the input is an array of ImageDimension
 * number of unsigned int. The elements of the array must be a rearrangment
 * of the numbers from 0 to ImageDimension - 1.
 *
 * The i-th axis of the output image corresponds with the order[i]-th 
 * axis of the input image.
 *
 * The output meta image information (LargestPossibleRegion, spacing, origin)
 * is computed by permuting the corresponding input meta information.
 *
 * \ingroup GeometricTransforms
 * \ingroup Multithreaded
 * \ingroup Streamed
 */
template <class TImage>
class ITK_EXPORT PermuteAxesImageFilter :
    public ImageToImageFilter<TImage,TImage>
{

public:
  /** Standard class typedefs. */
  typedef PermuteAxesImageFilter         Self;
  typedef ImageToImageFilter<TImage,TImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(PermuteAxesImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension );

  /** Inherited types */
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType    OutputImageRegionType;

  /** PermuteOrderArray type. */
  typedef FixedArray<unsigned int, itkGetStaticConstMacro(ImageDimension)> PermuteOrderArrayType;

  /** Set the permutation order.  The elements of order must be
   * a rearrangement of the numbers from 0 to ImageDimension - 1.*/
  void SetOrder( const PermuteOrderArrayType& order );

  /** Get the permutation order. */
  itkGetConstReferenceMacro( Order, PermuteOrderArrayType );

  /** Get the inverse permutation order. */
  itkGetConstReferenceMacro( InverseOrder, PermuteOrderArrayType );


  /** PermuteAxesImageFilter produces an image which is a different
   * resolution and with a different pixel spacing than its input
   * image.  As such, PermuteAxesImageFilter needs to provide an
   * implementation for GenerateOutputInformation() in order to inform
   * the pipeline execution model.  The original documentation of this
   * method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** PermuteAxesImageFilter needs different input requested region than the output
   * requested region.  As such, PermuteAxesImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

protected:
  PermuteAxesImageFilter();
  ~PermuteAxesImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** PermuteAxesImageFilter can be implemented as a multithreaded filter.
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

  PermuteOrderArrayType  m_Order;
  PermuteOrderArrayType  m_InverseOrder;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPermuteAxesImageFilter.txx"
#endif
  
#endif

