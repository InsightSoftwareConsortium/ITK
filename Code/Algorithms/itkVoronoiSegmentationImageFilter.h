/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkVoronoiSegmentationImageFilter_h
#define _itkVoronoiSegmentationImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVoronoiSegmentationImageFilterBase.h"
#include "itkImage.h"

namespace itk
{

/** \class VoronoiSegmentationImageFilter
 * 
 * Perform the segmentation of 2D images (single channel) by Voronoi Diagram.
 * Used as a node of the segmentation toolkits.
 * The homogeneity operator here is the testing of mean and standar deviation value.
 * By setting the tolerance level, the "internal" region was defined as those
 * that is closed to the gold-standard value in the sense that the difference
 * is within the tolerance value.
 *
 * See VoronoiSegmentationImageFilterBase for detail description of voronoi
 * segmenation principles.
 * 
 * The parameters here are: 
 * 1. the estimation of the statistics of the object. (mean and std.)
 * 2. the tolerance for the classification. (around the mean ans std. estimated value).
 *
 * The parameters can also be automatically set by given a prior, as a binary
 * image.
 *
 * Detail information about this algorithm can be found in:
 *  " Semi-automated color segmentation of anatomical tissue,"
 *   C. Imelinska, M. Downes, and W. Yuan  
 *  Computerized Medical Imaging and Graphics, Vor.24, pp 173-180, 2000.
 *
 * \ingroup HybridSegmentation 
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT VoronoiSegmentationImageFilter:
public VoronoiSegmentationImageFilterBase<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef VoronoiSegmentationImageFilter       Self;
  typedef VoronoiSegmentationImageFilterBase<TInputImage,TOutputImage> 
          Superclass;
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiSegmentationImageFilter,
               VoronoiSegmentationImageFilterBase);

  /** Convenient typedefs. */
  typedef typename Superclass::BinaryObjectImage BinaryObjectImage;
  typedef typename Superclass::IndexList IndexList;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::InputImageType InputImageType;

  /** Set/Get the Estimation of the mean pixel value for the object. */
  itkSetMacro(Mean, double);
  itkGetMacro(Mean, double);

  /** Set/Get the estimation of the STD of the pixel value for the
   *  object. */
  itkSetMacro(STD, double);
  itkGetMacro(STD, double);

  /** Set/Get the Tolearance of Mean for classifying the regions. */
  itkSetMacro(MeanTolerance, double);
  itkGetMacro(MeanTolerance, double);

  /** Set the Tolearance of STD for classifying the regions. */
  itkSetMacro(STDTolerance, double);

  /** Get the Tolearance of Variance for classifying the regions. */
  itkGetMacro(STDTolerance, double);
  
  /** Set/Get the mean percent error. */
  void SetMeanPercentError(double x);
  itkGetMacro(MeanPercentError, double);

  /** Set/Get the STD percent error. */
  itkGetMacro(STDPercentError, double);
  void SetSTDPercentError(double x);

  /** Take a prior from other segmentation node, should be an
   * binary object. */
  void TakeAPrior(BinaryObjectImage* aprior);
  
  
protected:
  VoronoiSegmentationImageFilter();
  ~VoronoiSegmentationImageFilter();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  double m_Mean;
  double m_STD;
  double m_MeanTolerance;
  double m_STDTolerance;
  double m_MeanPercentError;
  double m_STDPercentError;

  virtual bool TestHomogeneity(IndexList &Plist);

private:
  VoronoiSegmentationImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}//end namespace


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiSegmentationImageFilter.txx"
#endif

#endif




