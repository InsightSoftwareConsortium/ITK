/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFilter.h
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
#ifndef __itkNeighborhoodOperatorImageFilter_h
#define __itkNeighborhoodOperatorImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{
/**
 * \class NeighborhoodOperatorImageFilter
 * \brief Applies a single NeighborhoodOperator to an image region. 
 *
 * This filter calculates successive inner products between a single
 * NeighborhoodOperator and a NeighborhoodIterator, which is swept
 * across every pixel in an image region.  For operators that are
 * symmetric across their axes, the result is a fast convolution
 * with the image region.  Apply the mirror()'d operator for
 * non-symmetric NeighborhoodOperators.
 *
 * 
 * \ingroup ImageFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT NeighborhoodOperatorImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef NeighborhoodOperatorImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

 /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename  TInputImage::PixelType         InputPixelType;
  typedef typename  TInputImage::InternalPixelType InputInternalPixelType;
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef       SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Typedef for generic boundary condition pointer
   */
  typedef ImageBoundaryCondition<OutputImageType> *
   ImageBoundaryConditionPointerType;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(NeighborhoodOperatorImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Superclass typedefs.
   */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /**
   * Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline).
   */
  void SetOperator(const Neighborhood<OutputPixelType, ImageDimension> &p)
  {
    m_Operator = p;
    this->Modified();
  }

  /**
   * Allows a user to override the internal boundary condition. Care should be
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition.
   */
  void OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
    { m_BoundsCondition = i; }

  /**
   * NeighborhoodOperatorImageFilter needs a larger input requested
   * region than the output requested region.  As such,
   * NeighborhoodOperatorImageFilter needs to provide an implementation for
   * GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion() throw (InvalidRequestedRegionError);

protected:
  NeighborhoodOperatorImageFilter() {}
  virtual ~NeighborhoodOperatorImageFilter() {}
  NeighborhoodOperatorImageFilter(const Self&) : m_Operator(0) {}
  void operator=(const Self&) {}
    
  /**
   * NeighborhoodOperatorImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()
   */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  /**
   * Pointer to the internal operator used to filter the image.
   */
  Neighborhood<InputPixelType, ImageDimension> m_Operator;

  /**
   * Pointer to a persistent boundary condition object used
   * for the image iterator.
   */
  ImageBoundaryConditionPointerType m_BoundsCondition;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperatorImageFilter.txx"
#endif

#endif
