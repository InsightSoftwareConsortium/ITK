/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNeighborhoodOperatorImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorNeighborhoodOperatorImageFilter_h
#define __itkVectorNeighborhoodOperatorImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class VectorNeighborhoodOperatorImageFilter
 * \brief Applies a single scalar NeighborhoodOperator to an 
 * itk::Vector image region. 
 *
 * This filter calculates successive inner products between a single
 * NeighborhoodOperator and a NeighborhoodIterator, which is swept
 * across every pixel in an image region.  For operators that are
 * symmetric across their axes, the result is a fast convolution
 * with the image region.  Apply the mirror()'d operator for
 * non-symmetric NeighborhoodOperators.
 *
 * This filter assumes that the input and output images have
 * pixels which are itk::Vectors of the same vector dimension.
 * The input NeighbourhoodOperator must have a scalar type
 * that matches the ValueType of vector pixels.
 *
 * To apply a scalar NeighborhoodOperator to a scalar image
 * use NeighborhoodOperatorImageFilter instead.
 * 
 * \ingroup ImageFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa NeighborhoodOperatorImageFilter
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT VectorNeighborhoodOperatorImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /** Standard class typedefs. */
  typedef VectorNeighborhoodOperatorImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef       SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorNeighborhoodOperatorImageFilter, ImageToImageFilter);
  
  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TInputImage::Pointer           InputImagePointer;
  typedef typename TOutputImage::Pointer           OutputImagePointer;
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename  TInputImage::PixelType         InputPixelType;
  typedef typename  TInputImage::InternalPixelType InputInternalPixelType;
  typedef typename OutputPixelType::ValueType      ScalarValueType;
  
  /** Determine image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension );

  /** Image typedef support */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Typedef for generic boundary condition pointer */
  typedef ImageBoundaryCondition<OutputImageType> *
  ImageBoundaryConditionPointerType;
  
  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline). */
  void SetOperator(const Neighborhood<ScalarValueType,
                   itkGetStaticConstMacro(ImageDimension)> &p)
  {
    m_Operator = p;
    this->Modified();
  }

  /** Allows a user to override the internal boundary condition. Care should be
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition. */
  void OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
  { m_BoundsCondition = i; }

  /** VectorNeighborhoodOperatorImageFilter needs a larger input requested
   * region than the output requested region.  As such,
   * VectorNeighborhoodOperatorImageFilter needs to provide an implementation for
   * GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw (InvalidRequestedRegionError);

protected:
  VectorNeighborhoodOperatorImageFilter() {}
  virtual ~VectorNeighborhoodOperatorImageFilter() {}
    
  /** VectorNeighborhoodOperatorImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  void PrintSelf(std::ostream& os, Indent indent) const
  { Superclass::PrintSelf(os,indent);  }
private:
  VectorNeighborhoodOperatorImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Pointer to the internal operator used to filter the image. */
  Neighborhood<ScalarValueType, itkGetStaticConstMacro(ImageDimension)> m_Operator;

  /** Pointer to a persistent boundary condition object used
   * for the image iterator. */
  ImageBoundaryConditionPointerType m_BoundsCondition;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorNeighborhoodOperatorImageFilter.txx"
#endif

#endif
