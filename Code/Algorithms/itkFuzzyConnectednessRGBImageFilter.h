/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFuzzyConnectednessRGBImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFuzzyConnectednessRGBImageFilter_h
#define __itkFuzzyConnectednessRGBImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkScalar.h"
#include <vnl/vnl_matrix_fixed.h>

#include <queue>

namespace itk{

/** \class FuzzyConnectednessRGBImageFilter
 * 
 * Perform the segmentation by three channel(RGB) fuzzy connectedness.
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
 *
 * the input image should be in the form of:
 *  itkImage<itkVector<Pixeltype,3>,2>
 */

template <class TInputImage, class TOutputImage>
class FuzzyConnectednessRGBImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FuzzyConnectednessRGBImageFilter       Self;

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
  itkTypeMacro(FuzzyConnectednessRGBImageFilter,ImageToImageFilter);

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
  typedef typename TInputImage::PixelType PixelRGB;
  typedef UShortImage::Pointer FuzzyScene;

  typedef std::queue<IndexType> QueueType;

  typedef typename TOutputImage::RegionType RegionType;
  
  /**
   * Set the Weight of the first term(standard statistics) in the affinity 
   * computation.
   */
  itkSetMacro(Weight, double);
  /**
   * Get the Weight of the first term(standard statistics) in the affinity 
   * computation.
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
   * Setting the beginning point, believed to be inside the object.
   */
  void SetSeed(const IndexType & seed);
	
  /**
   * Perform the segmentation.
   */
  void ExcuteSegment();

  /**
   * Update the binary result. (needed after update the threshold)
   */
  void MakeSegmentObject();

  FuzzyScene GetFuzzyScene(void){ return m_FuzzyScene; };

  
  /**
   * Seting and geting the parameters
   */
  void SetMean(double imean[3]){
    m_Mean[0]=imean[0];m_Mean[1]=imean[1];m_Mean[2]=imean[2];
  };
  void GetMean(double omean[3]){
    omean[0]=m_Mean[0];omean[1]=m_Mean[1];omean[2]=m_Mean[2];
  };
  void SetVar(double ivar[3][3]){
    m_Var[0][0]=ivar[0][0];m_Var[0][1]=ivar[0][1];m_Var[0][2]=ivar[0][2];
	m_Var[1][0]=ivar[1][0];m_Var[1][1]=ivar[1][1];m_Var[1][2]=ivar[1][2];
	m_Var[2][0]=ivar[2][0];m_Var[2][1]=ivar[2][2];m_Var[2][2]=ivar[2][2];
  };
  void GetVar(double ovar[3][3]){
    ovar[0][0]=m_Var[0][0];ovar[0][1]=m_Var[0][1];ovar[0][2]=m_Var[0][2];
    ovar[1][0]=m_Var[1][0];ovar[1][1]=m_Var[1][1];ovar[1][2]=m_Var[1][2];
    ovar[2][0]=m_Var[2][0];ovar[2][1]=m_Var[2][1];ovar[2][2]=m_Var[2][2];
  };
  void SetDiffMean(double idmean[3]){
    m_Diff_Mean[0]=idmean[0];m_Diff_Mean[1]=idmean[1];m_Diff_Mean[2]=idmean[2];
  };
  void GetDiffMean(double odmean[3]){
    odmean[0]=m_Diff_Mean[0];odmean[1]=m_Diff_Mean[1];odmean[2]=m_Diff_Mean[2];
  };
  void SetDiffVar(double idvar[3][3]){
    m_Diff_Var[0][0]=idvar[0][0];m_Diff_Var[0][1]=idvar[0][1];m_Diff_Var[0][2]=idvar[0][2];
    m_Diff_Var[1][0]=idvar[1][0];m_Diff_Var[1][1]=idvar[1][1];m_Diff_Var[1][2]=idvar[1][2];
    m_Diff_Var[2][0]=idvar[2][0];m_Diff_Var[2][1]=idvar[2][1];m_Diff_Var[2][2]=idvar[2][2];
  };
  void GetDiffVar(double odvar[3][3]){
    odvar[0][0]=m_Diff_Var[0][0];odvar[0][1]=m_Diff_Var[0][1];odvar[0][2]=m_Diff_Var[0][2];
    odvar[1][0]=m_Diff_Var[1][0];odvar[1][1]=m_Diff_Var[1][1];odvar[1][2]=m_Diff_Var[1][2];
    odvar[2][0]=m_Diff_Var[2][0];odvar[2][1]=m_Diff_Var[2][1];odvar[2][2]=m_Diff_Var[2][2];
  };

protected:
  FuzzyConnectednessRGBImageFilter();
  ~FuzzyConnectednessRGBImageFilter();

private:
  double m_Mean[3];
  double m_Var[3][3];
  double m_Diff_Mean[3];
  double m_Diff_Var[3][3];
  double m_Var_inverse[3][3];
  double m_Diff_Var_inverse[3][3];
  double m_Var_Det;
  double m_Diff_Var_Det;
  double m_Weight;
  double m_Threshold;
  IndexType m_Seed;
  SizeType m_size;

  typename InputImageType::Pointer m_InputImage;
  typename UShortImage::Pointer m_FuzzyScene;
  typename OutputImageType::Pointer m_SegmentObject; 
  
  QueueType m_Queue;

  void PushNeighbors(const IndexType &center);
  double FuzzyAffinity(const PixelRGB f1, const PixelRGB f2);
  double FindStrongPath(const IndexType &center);
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFuzzyConnectednessRGBImageFilter.txx"
#endif

#endif
