/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiPartitioningImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkVoronoiPartitioningImageFilter_h
#define _itkVoronoiPartitioningImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVoronoiSegmentationImageFilterBase.h"
#include "itkImage.h"

namespace itk
{

/** \class VoronoiPartitioningImageFilter
 * 
 * Perform a partitioning of 2D images (single channel) by Voronoi Diagram.
 *
 * \ingroup HybridSegmentation
 */
template <class TInputImage, class TOutputImage>
class VoronoiPartitioningImageFilter:
public VoronoiSegmentationImageFilterBase<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef VoronoiPartitioningImageFilter       Self;
  typedef VoronoiSegmentationImageFilterBase<TInputImage,TOutputImage> 
          Superclass;
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiPartitioningImageFilter,
               VoronoiSegmentationImageFilterBase);

  /** Convenient typedefs. */
  typedef typename Superclass::BinaryObjectImage BinaryObjectImage;
  typedef typename Superclass::IndexList IndexList;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::InputImageType InputImageType;

  /** Create the output results.  */
  virtual void MakeSegmentBoundary(void);
  virtual void MakeSegmentObject(void);
  
protected:
  VoronoiPartitioningImageFilter();
  ~VoronoiPartitioningImageFilter();

  // Classify all the voronoi cells as interior , exterior or boundary.
  virtual void ClassifyDiagram(void);
  
  // Generate the seeds to be added by dividing the boundary cells.
  virtual void GenerateAddingSeeds(void);

  // Are the pixels specified in the index list homogeneous?
  virtual bool TestHomogeneity(IndexList &Plist);

private:
  VoronoiPartitioningImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}//end namespace


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiPartitioningImageFilter.txx"
#endif

#endif




