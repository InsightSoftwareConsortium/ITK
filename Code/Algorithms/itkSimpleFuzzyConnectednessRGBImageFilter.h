/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFuzzyConnectednessRGBImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimpleFuzzyConnectednessRGBImageFilter_h
#define __itkSimpleFuzzyConnectednessRGBImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkSimpleFuzzyConnectednessImageFilterBase.h"
#include <vnl/vnl_matrix_fixed.h>

#include <queue>

namespace itk{

/** /class SimpleFuzzyConnectednessRGBImageFilter
 * 
 * Perform the segmentation for three channels (RGB) image 
 * via thresholding of a fuzzy connectedness scene. 
 * Used as a node of the segmentation toolkit.
 * Fuzzy affinity is defined between two neighboor pixels, to reflect
 * their similarity and assign a probability that these two pixels belong to the
 * same object.  A "path" between two pixels is a list of pixels that connect
 * them, the strength of a particular path is defined as the weakest affinity
 * between the neighboor pixels that form the path. The fuzzy connectedness
 * between two pixels is defined as the strongest path strength between these
 * two pixels.  The segmentation based on fuzzy connectedness assumes that
 * the fuzzy connectedness between any two pixels from a single object 
 * is significantly higher than those for pixels belonging to different objects.  
 * A fuzzy connectedness scene is first computed for a set of input seed
 * points selected inside the object of interest.  A threshold is then
 * applied to the fuzzy scene to extract the binary segmented object.
 * The fuzzy affinity here was defined as a gaussian function of the pixel difference
 * and the difference of the estimated object mean and the mean of the two input
 * pixels ( in a vectorial fashion).
 *
 * Usage:
 *
 * 1. use SetInput to import the input image object
 * 2. use SetParameter, SetSeed, SetThreshold to set the parameters
 * 3. run ExcuteSegment to perform the segmenation
 * 4. threshold can be set after the segmentation, and no computation
 *    will be redo. no need to run GenerateData. But if SetThreshold was used.
 *    MakeSegmentObject() should be called to get the updated result.
 * 5. use GetOutput to obtain the resulted binary image Object.
 * 6. GetFuzzyScene gives the pointer of Image<unsigned short> for the 
 *    fuzzy scene.*
 *
 * Input Parameters are:
 * (1) Input image in the form  itkImage<itkVector<Pixeltype,3>,Dimension>
 * (2) Seed points
 * (3) Threshold value.
 * 
 * The fuzzy scene can also be extracted with the GetFuzzyScene method.
 *
 * Detailed information about this algorithm can be found in:
 *  "Fuzzy Connectedness and Object Definition: Theory, Algorithms,
 *    and Applications in Image Segmentation", J. Udupa and S. Samarasekera
 *  Graphical Models and Image Processing, Vol.58, No.3. pp 246-261, 1996.
 *
 * 
 * \ingroup FuzzyConnectednessSegmentation */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT SimpleFuzzyConnectednessRGBImageFilter:
  public SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SimpleFuzzyConnectednessRGBImageFilter       Self;
  typedef SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
                                                 Superclass;
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleFuzzyConnectednessRGBImageFilter,
               SimpleFuzzyConnectednessImageFilterBase);

  /** The pixel type. */
  typedef typename TInputImage::PixelType PixelType;

  /** Setting and getting the segmentation parameters. */
  itkSetVectorMacro(Mean,double,3);
  void GetMean(double omean[3])
    {
    omean[0]=m_Mean[0];omean[1]=m_Mean[1];omean[2]=m_Mean[2];
    }
  void SetVariance(double ivar[3][3])
    {
    m_Variance[0][0]=ivar[0][0];m_Variance[0][1]=ivar[0][1];m_Variance[0][2]=ivar[0][2];
    m_Variance[1][0]=ivar[1][0];m_Variance[1][1]=ivar[1][1];m_Variance[1][2]=ivar[1][2];
    m_Variance[2][0]=ivar[2][0];m_Variance[2][1]=ivar[2][2];m_Variance[2][2]=ivar[2][2];
    }
  void GetVariance(double ovar[3][3])
    {
    ovar[0][0]=m_Variance[0][0];ovar[0][1]=m_Variance[0][1];ovar[0][2]=m_Variance[0][2];
    ovar[1][0]=m_Variance[1][0];ovar[1][1]=m_Variance[1][1];ovar[1][2]=m_Variance[1][2];
    ovar[2][0]=m_Variance[2][0];ovar[2][1]=m_Variance[2][1];ovar[2][2]=m_Variance[2][2];
    }
  itkSetVectorMacro(Diff_Mean,double,3);
  void GetDiff_Mean(double odmean[3])
    {
    odmean[0]=m_Diff_Mean[0];odmean[1]=m_Diff_Mean[1];odmean[2]=m_Diff_Mean[2];
    };
  void SetDiff_Variance(double idvar[3][3])
    {
    m_Diff_Variance[0][0]=idvar[0][0];m_Diff_Variance[0][1]=idvar[0][1];m_Diff_Variance[0][2]=idvar[0][2];
    m_Diff_Variance[1][0]=idvar[1][0];m_Diff_Variance[1][1]=idvar[1][1];m_Diff_Variance[1][2]=idvar[1][2];
    m_Diff_Variance[2][0]=idvar[2][0];m_Diff_Variance[2][1]=idvar[2][1];m_Diff_Variance[2][2]=idvar[2][2];
    }
  void GetDiff_Variance(double odvar[3][3])
    {
    odvar[0][0]=m_Diff_Variance[0][0];odvar[0][1]=m_Diff_Variance[0][1];odvar[0][2]=m_Diff_Variance[0][2];
    odvar[1][0]=m_Diff_Variance[1][0];odvar[1][1]=m_Diff_Variance[1][1];odvar[1][2]=m_Diff_Variance[1][2];
    odvar[2][0]=m_Diff_Variance[2][0];odvar[2][1]=m_Diff_Variance[2][1];odvar[2][2]=m_Diff_Variance[2][2];
    }

protected:
  SimpleFuzzyConnectednessRGBImageFilter();
  ~SimpleFuzzyConnectednessRGBImageFilter();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);
 
private:
  SimpleFuzzyConnectednessRGBImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  double m_Mean[3];
  double m_Variance[3][3]; 
  double m_Diff_Mean[3];
  double m_Diff_Variance[3][3];

  double m_VarianceInverse[3][3];
  double m_Diff_VarianceInverse[3][3];
  double m_VarianceDet;
  double m_Diff_VarianceDet;

  virtual double FuzzyAffinity(const PixelType f1, const PixelType f2);
};


} /** end namespace itk. */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleFuzzyConnectednessRGBImageFilter.txx"
#endif

#endif
