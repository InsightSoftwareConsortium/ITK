/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFuzzyConnectednessImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFuzzyConnectednessImageFilter_h
#define __itkFuzzyConnectednessImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkScalar.h"

#include <queue>
#include <cmath>

namespace itk{

/** \class FuzzyConnectednessImageFilter
 * 
 * Perform the segmentation by single channel fuzzy connectedness.
 * Used as a node of the segmentation toolkits.
 * The Basic concept here is the fuzzy affinity which is defined between
 * two neighbor pixels, it reflects the similarity and possibility of these
 * two pixels to be in the same object. 
 * A "path" between two pixels is a list of pixels that connect them, the stregth
 * of a particular path was defined as the weakest affinity between the neighbor pixels
 * that form the path.
 * The fuzzy connectedness between two pixels is defined as the strongest path stregth
 * between these two pixels.
 * The segmentation based on fuzzy connectedness assumes that the fuzzy connectedness 
 * between any two pixels is significantly higher than those belongs to different objects.
 * A fuzzy connectedness scene was first computed, which is the fuzzy connectedness value 
 * to a preset seed point believed to be inside the object of interest.
 * then a threshold was applied to obtain the binary segmented object.
 * 
 * Usage:
 *
 * 1. use SetInput to import the input image object
 * 2. use SetParameter, SetSeed, SetThreshold to set the parameters
 * 3. run ExcuteSegment to perform the segmenation
 * 4. threshold can be set after the segmentation, and no computation
 *     will be redo. need MakeSegmentObject to update the result.
 * 5. use GetOutput to obtain the resulted binary image Object.
 *
 * Detail information about this algorithm can be found in:
 *  "Fuzzy Connectedness and Object Definition: Theory, Algorithms,
 *    and Applications in Image Segmentation", J. Udupa and S. Samarasekera
 *  Graphical Models and Image Processing, Vol.58, No.3. pp 246-261, 1996.
 */

template <class TInputImage, class TOutputImage>
class FuzzyConnectednessImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FuzzyConnectednessImageFilter       Self;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FuzzyConnectednessImageFilter,ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  enum {ImageDimension = TInputImage::ImageDimension };


  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef Image <unsigned short, ImageDimension> UShortImage;
  typedef typename TInputImage::IndexType IndexType;
  typedef typename TInputImage::SizeType SizeType;

  typedef std::queue<IndexType> QueueType;

  typedef typename TOutputImage::RegionType RegionType;
  
  /**
   * Set the Estimation of the mean pixel value for the object.
   */
  itkSetMacro(Mean, double);

  /**
   * Get the Estimation of the mean pixel value for the object.
   */
  itkGetMacro(Mean, double);
  /**
   * Set the Estimation of the variance of the pixel value for the object.
   */
  itkSetMacro(Var, double);
  /**
   * Get the Estimation of the variance of the pixel value for the object.
   */
  itkGetMacro(Var, double);

  /**
   * Set the Estimation of the mean difference between neighbor pixels for the object.
   */
  itkSetMacro(Diff_Mean, double);
  /**
   * Get the Estimation of the mean difference between neighbor pixels for the object.
   */
  itkGetMacro(Diff_Mean, double);
  /**
   * Set the Estimation of the variance of the difference between pixels for the object.
   */
  itkSetMacro(Diff_Var, double);
  /**
   * Get the Estimation of the variance of the difference between pixels for the object.
   */
  itkGetMacro(Diff_Var, double);
  
  /**
   * Set the Weight of the first term(standard statistics) in the affinity computation.
   */
  itkSetMacro(Weight, double);
  /**
   * Get the Weight of the first term(standard statistics) in the affinity computation.
   */
  itkGetMacro(Weight, double);
  /**
   * Set the Threshold value for the segmentation.
   */
  itkSetMacro(Threshold, double);
  /**
   * Get the Threshold value for the segmentation.
   */
  itkGetMacro(Threshold, double);

  /**
   * Setting the parameters for segmentation:
   */
  void SetParameters(const double inmean,const double invar, 
	  const double indifmean,const double indifvar,const double inweight);
	
  /**
   * Setting the beginning point, believed to be inside the object.
   */
  void SetSeed(const IndexType & seed);
	
  /**
   * Perform the segmentation.
   */
  void ExcuteSegment();

protected:
  FuzzyConnectednessImageFilter();
  ~FuzzyConnectednessImageFilter();

private:
  double m_Mean;
  double m_Var;
  double m_Diff_Mean;
  double m_Diff_Var;
  double m_Weight;
  double m_Threshold;
  IndexType m_Seed;
  SizeType m_size;

  typename InputImageType::Pointer m_InputImage;
  UShortImage::Pointer m_FuzzyScene;
  typename OutputImageType::Pointer m_SegmentObject; 
  
  QueueType m_Queue;

  void PushNeighbors(const IndexType &center);
  double FuzzyAffinity(const double f1, const double f2);
  double FindStrongPath(const IndexType &center);
  void MakeSegmentObject();
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFuzzyConnectednessImageFilter.txx"
#endif

#endif




	




