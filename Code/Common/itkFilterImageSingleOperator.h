/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageSingleOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageSingleOperator_h
#define __itkFilterImageSingleOperator_h

#include "itkFilterImageToImage.h"
#include "itkNeighborhoodOperator.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class FilterImageSingleOperator
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

template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT FilterImageSingleOperator :
    public FilterImageToImage< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageSingleOperator Self;

  /**
   * Standard parent class typedef support.
   */
  typedef FilterImageToImage< Image<TPixel, VDimension>,
    Image<TPixel, VDimension> > Superclass;
  
  /**
   * NeighborhoodOperator typedef support.
   */
  typedef NeighborhoodOperator<TPixel, VDimension> NeighborhoodOperator;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;

  /**
   * Image typedef support
   */
  typedef Image<TPixel, VDimension> Image;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FilterImageSingleOperator, FilterImageToImage);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline).
   */
  void SetOperator(const NeighborhoodOperator &op)
  {
    if (m_Operator) delete m_Operator;
    m_Operator = op.Copy();   
    this->Modified();
  }

  /**
   * Returns a copy of the operator being used
   * to filter the image.
   */
  NeighborhoodOperator GetOperator() const
  {
    return m_Operator;
  }

  /**
   * Because FilterImageSingleOperator works on pixel neighborhoods, it will
   * try to get enough information from the input to avoid using artificial
   * boundary conditions. We must overload GenerateInputRequestedRegion to
   * manipulate the input region size. 
   */
  virtual void GenerateInputRequestedRegion();

  void GenerateData();

protected:
  FilterImageSingleOperator() : m_Operator(0) {}
  virtual ~FilterImageSingleOperator()
  {
    if (m_Operator) delete m_Operator;
  }
  FilterImageSingleOperator(const Self&) : m_Operator(0) {}
  void operator=(const Self&) {}
    
private:
  /**
   * Pointer to the internal operator used to filter the image.  Memory
   * for the operator is allocated and freed by this filter object.
   */
  NeighborhoodOperator *m_Operator;

  /**
   * Flag to indicate whether bounds checking will be necessary or
   * not on the input.
   */
  bool m_CheckBounds;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageSingleOperator.txx"
#endif

#endif
