/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef _itkVoronoiSegmentationImageFilter_h
#define _itkVoronoiSegmentationImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVoronoi2DDiagram.h"
#include "itkImage.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
class VoronoiSegmentationImageFilter:
public ImageToImageFilter<TInputImage,TOutputImage>
{
/** \class VoronoiSegmentationImageFilter
 * 
 * Perform the segmentation of 2D images (single channel) by Voronoi Diagram.
 * Used as a node of the segmentation toolkits.
 * The parameters here are: 
 * 1. the estimation of the statistics of the object. (mean and std.)
 * 2. the tolerance for the classification. (around the mean ans std. estimated value).
 * The parameters can also be automatically set by given a prior, as a binary image.
 *
 *
 * Detail information about this algorithm can be found in:
 *  " Semi-automated color segmentation of anatomical tissue,"
 *   C. Imelinska, M. Downes, and W. Yuan  
 *  Computerized Medical Imaging and Graphics, Vor.24, pp 173-180, 2000.
 */
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VoronoiSegmentationImageFilter       Self;

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
  itkTypeMacro(VoronoiSegmentationImageFilter,ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  enum {ImageDimension = TInputImage::ImageDimension };


  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename TInputImage::IndexType IndexType;
  typedef typename TInputImage::SizeType SizeType;
  typedef typename TInputImage::RegionType RegionType;
  typedef Voronoi2DDiagram<double> VoronoiDiagram;
  
  typedef typename VoronoiDiagram::PointType PointType;
  typedef typename VoronoiDiagram::Cell Cell;
  typedef typename VoronoiDiagram::CellPointer CellPointer;
  typedef typename Cell::PointIdIterator PointIdIterator;

  typedef typename VoronoiDiagram::SeedsType SeedsType;
  typedef typename VoronoiDiagram::SeedsIterator SeedsIterator;
  typedef typename VoronoiDiagram::NeighborIdIterator NeighborIdIterator;
  typedef typename VoronoiDiagram::EdgeIterator EdgeIterator;
  typedef typename VoronoiDiagram::FortuneEdgeInfo EdgeInfo;
  typedef std::vector<PointType> PointTypeVector;
  typedef std::deque<PointType> PointTypeDeque;
  typedef itk::Image<bool,2>  BinaryObjectImage;
  typedef typename BinaryObjectImage::Pointer  BinaryObjectImagePointer;

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
   * Set the Tolearance of Mean for classifying the regions
   */
  itkSetMacro(MeanTolerance, double);

	/**
   * Get the Tolearance of Mean for classifying the regions
   */
  itkGetMacro(MeanTolerance, double);

	/**
   * Set the Tolearance of Variance for classifying the regions
   */
  itkSetMacro(VarTolerance, double);

	/**
   * Get the Tolearance of Variance for classifying the regions
   */
  itkGetMacro(VarTolerance, double);
	
	/**
   * Set the initial Number of Seeds for VD
   */
  itkSetMacro(NumberOfSeeds, int);
	/**
   * Get the Number of Seeds for VD
   */
  itkGetMacro(NumberOfSeeds, int);

	/**
   * Set the smallest region to be divided
   */
  itkSetMacro(MinRegion, int);

	/**
   * Get the smallest region to be divided
   */
  itkGetMacro(MinRegion, int);

	/**
   * Set the number of iterations to run (0: run until no more to divide);
   */
  itkSetMacro(Steps, int);

	/**
   * Get the number of iterations to run (0: run until no more to divide);
   */
  itkGetMacro(Steps, int);

	/**
   * Get the number of Seeds before adding new seeds;
   */
  itkGetMacro(LastStepSeeds, int);

  itkGetMacro(MeanPercentError, double);
  itkGetMacro(VarPercentError, double);

  void SetMeanPercentError(double x);
  void SetVarPercentError(double x);


	/**
	 * stuff need to be take care of before segmentation
	 */
  void InitializeSegment(void);

  /**
   * take a prior from other segmentation node, should be an
   * binary object.
   */
  void TakeAPrior(BinaryObjectImage* aprior);
  
  /**
   * Perform the segmentation.
   */
  void ExcuteSegment(void);

  /**
   * Perform the segmentation.
   */
  void ExcuteSegmentOneStep(void);

  /**
   * Make the output binary result as boundary. 
   */
  void MakeSegmentBoundary(void);

protected:
  VoronoiSegmentationImageFilter();
  ~VoronoiSegmentationImageFilter();

private:
  double m_Mean;
  double m_Var;
  double m_MeanTolerance;
  double m_VarTolerance;
  double m_MeanPercentError;
  double m_VarPercentError;
  SizeType m_Size;
  int m_NumberOfSeeds;
  int m_MinRegion;
  int m_Steps;
  int m_LastStepSeeds;
  int m_NumberOfSeedsToAdded;
  int m_NumberOfBoundary;
  std::vector<int> m_NumberOfPixels;
  std::vector<unsigned char> m_Label;
  int m_StepsRuned;

  typename InputImageType::Pointer m_InputImage;
  typename OutputImageType::Pointer m_OutputImage;
  typename VoronoiDiagram::Pointer m_WorkingVD;

  std::vector<PointType> m_SeedsToAdded;

	// private methods:
	// classify all the voronoi cells as interior or exterior or boundary
  void ClassifyDiagram(void);

	// generate the seeds to be added. (by divide the boundary cells)
  void GenerateAddingSeeds(void);

	// compute the statistics of the pixels inside the polygon.
  void GetStats(PointTypeDeque vertlist, double *savemean, double *savevar, int *num);

	// draw a straight line to the output image.
  void drawLine(PointType p1,PointType p2);
};

}//end namespace


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiSegmentationImageFilter.txx"
#endif

#endif




