/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationRGBImageFilter.h
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

#ifndef _itkVoronoiSegmentationRGBImageFilter_h
#define _itkVoronoiSegmentationRGBImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVoronoi2DDiagram.h"
#include "itkVoronoiSegmentationImageFilterBase.h"
#include "itkImage.h"

namespace itk
{

/** \class VoronoiSegmentationRGBImageFilter
 * 
 * Perform the segmentation of 2D images (RGB image) by Voronoi Diagram.
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
 * the input image should be in the format of:
 * itkImage<itkVector<PixelType,3>, 2>.
 *
 * \ingroup HybridSegmentation 
 */

template <class TInputImage, class TOutputImage>
class VoronoiSegmentationRGBImageFilter:
public VoronoiSegmentationImageFilterBase<TInputImage,TOutputImage>
{

public:
  /**
   * Standard "Self" typedef.
   */
  typedef VoronoiSegmentationRGBImageFilter       Self;

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
  itkTypeMacro(VoronoiSegmentationRGBImageFilter,VoronoiSegmentationImageFilterBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);


  typedef typename Superclass::BinaryObjectImage BinaryObjectImage;
  typedef typename Superclass::IndexList IndexList;

  typedef Vector<float,6> RGBHCVPixel;
  typedef Image<RGBHCVPixel> RGBHCVImage;

  void SetMeanPercentError(double x[6]);
  void SetVarPercentError(double x[6]);

  void GetMeanPercentError(double x[6]){for(int i=0;i<6;i++) x[i]=m_MeanPercentError[i];};
  void GetVarPercentError(double x[6]){for(int i=0;i<6;i++) x[i]=m_VarPercentError[i];};
  void GetMean(double x[6]){for(int i=0;i<6;i++) x[i]=m_Mean[i];};
  void GetVar(double x[6]){for(int i=0;i<6;i++) x[i]=m_Var[i];};
  void SetMean(double x[6]){for(int i=0;i<6;i++) m_Mean[i]=x[i];};
  void SetVar(double x[6]){for(int i=0;i<6;i++) m_Var[i]=x[i];};
  void GetMeanTolerance(double x[6]){for(int i=0;i<6;i++) x[i]=m_MeanTolerance[i];};
  void GetVarTolerance(double x[6]){for(int i=0;i<6;i++) x[i]=m_VarTolerance[i];};

  /*
   * maximum value of the RGB, needed for color space coversions.
   * default as 8 bit per channel, if it is different, need to be
   * set before doing anything.
   */
  itkSetMacro(MaxValueOfRGB,double);
  itkGetMacro(MaxValueOfRGB,double);

  /*
   * set the three channels to test the mean and var respectivley
   * 0:red, 1:green, 2:blue, 3:hue, 4:chroma, 5:value
   */
  void SetTestMean(unsigned int t1,unsigned int t2,unsigned int t3){
    m_TestMean[0] = t1;
    m_TestMean[1] = t2;
    m_TestMean[2] = t3;
  }
  void SetTestVar(unsigned int t1,unsigned int t2,unsigned int t3){
    m_TestVar[0] = t1;
    m_TestVar[1] = t2;
    m_TestVar[2] = t3;
  }
  void GetTestMean(unsigned int x[3]){
    x[0]=m_TestMean[0];x[1]=m_TestMean[1];x[2]=m_TestMean[2];
  }
  void GetTestVar(unsigned int x[3]){
    x[0]=m_TestVar[0];x[1]=m_TestVar[1];x[2]=m_TestVar[2];
  }
  void InitializeSegment(void);
  void TakeAPrior(BinaryObjectImage* aprior);
  void Reset(void);

protected:
  VoronoiSegmentationRGBImageFilter();
  ~VoronoiSegmentationRGBImageFilter();

private:
  double m_Mean[6];
  double m_Var[6];  //actually it is the STD of the object. (sqrt(Var)).
  double m_MeanTolerance[6];
  double m_VarTolerance[6];
  double m_MeanPercentError[6];
  double m_VarPercentError[6];
  double m_MaxValueOfRGB;
  unsigned int m_TestMean[3];
  unsigned int m_TestVar[3];
  typename RGBHCVImage::Pointer m_WorkingImage;

  virtual bool TestHomogeneity(IndexList Plist);
};

}//end namespace


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiSegmentationRGBImageFilter.txx"
#endif

#endif




