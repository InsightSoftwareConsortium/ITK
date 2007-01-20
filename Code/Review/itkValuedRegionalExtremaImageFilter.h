/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkValuedRegionalExtremaImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkValuedRegionalExtremaImageFilter_h
#define __itkValuedRegionalExtremaImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"
#include <stack>

namespace itk {

/** \class ValuedRegionalExtremaImageFilter 
 * \brief Uses a flooding algorithm to set all voxels that are not a
 * regional extrema to the max or min of the pixel type. 
 *
 * This is the class used by ValuedRegionalMinimaImageFilter and
 * ValuedRegionalMaximaImageFilter. There is no supression of regional
 * minima based on dynamics, as available in HMinimaImageFilter. This
 * flooding algorithm is a very simple one, but I'm not sure where it
 * came from - I certainly didn't invent it.
 *
 * Let's consider the case of regional minima.
 * The basic algorithm is:
 *    Boundary conditions are such that the image is logically
 *    surrounded by a border that is either maximal or minimal for the
 *    pixel type. An optimized version could explicitly set the border
 *    to avoid the need for boundary checks. For regional minima the
 *    boundary is set to the maximal value for the pixel type.
 *
 *    Pixels are visited in raster order. The neighbors of each pixel
 *    are examined. If any neighbor is greater than the centre, then
 *    the centre pixel cannot be a regional minima. The centre pixel
 *    is part of a flat region (consisting of at least one pixel) that
 *    is therefore not a regional minima either. This region is set to
 *    the maximum value for the pixel type using a flooding algorithm.
 *    
 *    There are some minor complications that prevent pixels being
 *    examined more than once -- basically check that the output value
 *    is less than the maximum for the pixel type.
 *
 * The implementation uses the functor model from itkMaximumImageFilter.
 * 
 * This class was contributed to the Insight Journal by 
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *       http://hdl.handle.net/1926/153 
 *
 * \sa ValuedRegionalMinimaImageFilter, ValuedRegionalMaximaImageFilter, HMinimaImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 */

template<class TInputImage, class TOutputImage, 
      class TFunction1, class TFunction2>
class ITK_EXPORT ValuedRegionalExtremaImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ValuedRegionalExtremaImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename InputImageType::SizeType        ISizeType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  
  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(ValuedRegionalExtremaImageFilter, 
               ImageToImageFilter);


  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);
  
  /**
   * Set/Get the value used to mark all pixels which are not extrema.
   */
  itkSetMacro(MarkerValue, typename TInputImage::PixelType);
  itkGetConstReferenceMacro(MarkerValue, typename TInputImage::PixelType);

  /**
   * Get whether the image is flat or not.
   */
  itkGetMacro(Flat, bool);

protected:
  ValuedRegionalExtremaImageFilter();
  ~ValuedRegionalExtremaImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ValuedRegionalExtremaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** ValuedRegionalExtremaImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));
  
  void GenerateData();
  

private:
  ValuedRegionalExtremaImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  typename TInputImage::PixelType m_MarkerValue;
  bool                m_FullyConnected;
  bool  m_Flat;

  typedef typename OutputImageType::IndexType OutIndexType;
  typedef typename InputImageType::IndexType InIndexType;
  typedef ConstShapedNeighborhoodIterator<InputImageType> CNInputIterator;
  typedef ShapedNeighborhoodIterator<OutputImageType> NOutputIterator;
  typedef std::stack<OutIndexType> IndexStack;
  //typedef NOutputIterator::IndexListType OIndexListType;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkValuedRegionalExtremaImageFilter.txx"
#endif


#endif
