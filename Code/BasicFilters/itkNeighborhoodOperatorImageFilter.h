/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
 * The output of this filter is an image region equal in size and
 * dimension to the input.
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
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Typedef for generic boundary condition pointer
   */
  typedef ImageBoundaryCondition<OutputImageType,
    Neighborhood<typename OutputImageType::InternalPixelType *,
    OutputImageType::ImageDimension> > *
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

  void GenerateData();

protected:
  NeighborhoodOperatorImageFilter() {}
  virtual ~NeighborhoodOperatorImageFilter() {}
  NeighborhoodOperatorImageFilter(const Self&) : m_Operator(0) {}
  void operator=(const Self&) {}
    
private:
  /**
   * Pointer to the internal operator used to filter the image.
   */
  Neighborhood<OutputPixelType, ImageDimension> m_Operator;

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
