/*=========================================================================
  
  Filter: Automatic Threshold Image Filter
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdMaximumConnectedComponentsImageFilter.h
  Language:  C++
  Date:      11 July 2005
  Version:   Revision: 1.00

  Copyright (c) Ken Urish 2005. All rights reserved.
  
  Portions of this code are covered under the ITK and VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.


     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.
=========================================================================*/

#ifndef __ThresholdMaximumConnectedComponentsImageFilter_h
#define __ThresholdMaximumConnectedComponentsImageFilter_h

#include "itkInPlaceImageFilter.h"

#include "itkConceptChecking.h"

namespace itk
{

/**\class ThresholdMaximumConnectedComponentsImageFilter
 * \brief Finds the threshold set point of an image based off of 
 * maximizing the number of objects in the image larger than a given minimal value. 
 *
 * \par
 * This method is based on Topological Stable State Thresholding to calculate the
 * threshold set point. This method is particularly effective when there are a large
 * number of objects in a microscopy image. Uncomment the output statements in the 
 * GenerateData section to see how the filter focuses in on a threshold set point.
 * Please see the Insight Journal's MICCAI 2005 workshop for a complete 
 * description. References are below.
 *
 * \par Parameters
 * The MinimumPixelArea parameter is controlled through the class 
 * Get/SetMinimumPixelArea() method. Similar to the standard 
 * itk::BinaryThresholdImageFilter the Get/SetInside and Get/SetOutside values of the 
 * threshold can be set. The class' GetNumberOfObjects and 
 * GetThresholdSetPoint return the number of objects above the minimum pixel 
 * area and the calculated threshold set point.
 * 
 * \par Automatic Thresholding in ITK
 * There are multiple methods to automatically calculate the threshold 
 * set point of an image. As of version 2.6, ITK implements two of these. Otsu thresholding 
 * (see itk::OtsuThresholdImageFilter) is a common method for segmenting 
 * CT radiographs. Topological Stable State Thresholding works well on images 
 * with a large number of objects to be counted.
 *   
 * \par References: 
 * 1) Urish KL, August J, Huard J. "Unsupervised segmentation for myofiber 
 * counting in immunoflourescent images". Insight Journal. 
 * ISC/NA-MIC/MICCAI Workshop on Open-Source Software (2005)
 * Dspace handle: http://hdl.handle.net/1926/48
 * 2) Pikaz A, Averbuch, A. "Digital image thresholding based on topological 
 * stable-state". Pattern Recognition, 29(5): 829-843, 1996.
 * 
 * \par
 * Questions: email Ken Urish at ken.urish(at)gmail.com
 * Please cc the itk list serve for archival purposes.
 */
template <class TImage>
class ITK_EXPORT ThresholdMaximumConnectedComponentsImageFilter:public ImageToImageFilter<TImage,TImage>
{
public:
  /** Standard class typedefs. */
  typedef ThresholdMaximumConnectedComponentsImageFilter Self;
  typedef ImageToImageFilter<TImage,TImage>  Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdMaximumConnectedComponentsImageFilter, ImageToImageFilter);

  /** Typedef to describe the type of pixel. */
  typedef typename TImage::PixelType PixelType;
  
  /** The pixel type must support comparison operators. */
  itkConceptMacro(PixelTypeComparable, (Concept::Comparable<PixelType>));
   
  /** Set the minimum pixel area used to count objects on the image. Thus, only objects that 
   *  have a pixel area greater than the minimum pixel area will be counted as an object 
   *  in the optimization portion of this filter. Essentially, it eliminates noise from being 
   *  counted as an object. The default value is zero. */
  itkSetMacro(MinimumPixelArea,unsigned int);
  itkGetMacro(MinimumPixelArea,unsigned int);
  
  /** The following set macros are for the binary threshold function.
   *  This automatic threshold routine automatically calculates the lower threshold boundary.
   *  The upper threshold boundary, inside value, and outside value can be defined 
   *  by the user, however the standard values are used as default if not set by the user. 
   *  The default value of the: inside value is the maximum pixel type intenisty.
   *                            outside value is the minimum pixel type intenisty. 
   *                            upper threshold boundary is the maximum pixel type intenisty. */
  itkSetMacro(InsideValue, PixelType);
  itkSetMacro(OutsideValue, PixelType);
  itkSetMacro(UpperBoundary, PixelType);
  itkGetMacro(InsideValue, PixelType);
  itkGetMacro(OutsideValue, PixelType);
  itkGetMacro(UpperBoundary, PixelType);

  /** Returns the number of objects in the image. This information is only
   * valid after the filter has executed. Useful if your counting someething*/
  itkGetMacro(NumberOfObjects, unsigned int);

  /** Returns the automatic threshold setpoint. This information is only
   * valid after the filter has executed. */
  itkGetMacro(ThresholdSetPoint, unsigned int);

  /** Some additional typedefs.  */
  typedef TImage InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** Some additional typedefs.  */
  typedef TImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;
  

protected:
  ThresholdMaximumConnectedComponentsImageFilter();
  ~ThresholdMaximumConnectedComponentsImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** 
   *ImageToImageFilter::GenerateData()  */
  void GenerateData(void);

  /* Runs a series of filters that thresholds the image, dilates/erodes for edge enhancement, 
   * and counts the number of relabeled connected components  */
  void ComputeConnectedComponents(void); 


private:
 
  /** Typedef for filter pixel type.  */   
  typedef unsigned int FilterPixelType;

  itkStaticConstMacro( ImageDimension, unsigned int, TImage::ImageDimension );

  typedef itk::Image< FilterPixelType, ImageDimension > FilterImageType; 

  typedef typename FilterImageType::Pointer FilterImagePointer;

  ThresholdMaximumConnectedComponentsImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
   
  //Variables defined by the user

  unsigned int m_MinimumPixelArea;
  
  //Binary threshold variables
  PixelType m_OutsideValue;
  PixelType m_InsideValue;
  PixelType m_LowerBoundary;
  PixelType m_UpperBoundary;

  //Filter variables
  PixelType    m_ThresholdSetPoint;
  unsigned int m_NumOfConnectedComponents;
  unsigned int m_NumberOfObjects;   

  //Local Variables

  //Variables used in the search strategy
  PixelType lowerBound;
  PixelType upperBound;
  PixelType ccMidL;
  PixelType ccMidR;
  PixelType midpoint;
  PixelType midpointL;
  PixelType midpointR;
   
  PixelType inputPixelValue;
  unsigned int connectedComponentSize;
  unsigned int finalConnectedComponents;
  int i; // the loop in the connected components macro
  int j; // the loop in search to find maximal connected components
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdMaximumConnectedComponentsImageFilter.txx"
#endif
  
#endif

