/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilter.h
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
 *
 * \ingroup HybridSegmentation 
 */

template <class TInputImage, class TOutputImage>
class VoronoiSegmentationImageFilter:
public VoronoiSegmentationImageFilterBase<TInputImage,TOutputImage>
{
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
  typedef VoronoiSegmentationImageFilterBase<TInputImage,TOutputImage>   Superclass;


  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(VoronoiSegmentationImageFilter,VoronoiSegmentationImageFilterBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);


  typedef typename Superclass::BinaryObjectImage BinaryObjectImage;
  typedef typename Superclass::IndexList IndexList;

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
  
  void Reset(void); //reset the segmentation, ready for taking aprior from itself
  
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

  virtual bool TestHomogeneity(IndexList Plist);
};

}//end namespace


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiSegmentationImageFilter.txx"
#endif

#endif




