/*=========================================================================

  Program:   itkUNC
  Module:    itkHoughTransform2DCirclesImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHoughTransform2DCirclesImageFilter_h
#define __itkHoughTransform2DCirclesImageFilter_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkEllipseSpatialObject.h"

namespace itk
{

/**
 * \class HoughTransformLines2DImageFilter
 * \brief Performs the Hough Transform to find circles in a 2D image.
 *  
 * This filter derives from the base class ImageToImageFilter
 * The input is an image, and all pixels above some threshold are those
 * we want to consider during the process.
 *
 * This filter produces two output:
 *   1) The accumulator array, which represents probability of centers.
 *   2) The array or radii, which has the radius value at each coordinate point.
 *
 *  When the filter found a "correct" point, it computes the gradient at this 
 * point and draw a regular narrow-banded circle using the minimum and maximum 
 * radius given by the user, and fill in the array of radii.
 *
 * \ingroup ImageFeatureExtraction 
 *
 * */

template<typename TInputPixelType, typename TOutputPixelType>
class ITK_EXPORT HoughTransform2DCirclesImageFilter :
public ImageToImageFilter< Image<TInputPixelType,2>, Image<TOutputPixelType,2> >
{
public:
   
  /** Standard "Self" typedef. */
  typedef HoughTransform2DCirclesImageFilter Self;

   /** Input Image typedef */ 
  typedef Image<TInputPixelType,2> InputImageType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

   /** Output Image typedef */
  typedef Image<TOutputPixelType,2> OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;


  /** Standard "Superclass" typedef. */
  typedef ImageToImageFilter< Image<TInputPixelType,2>
                          , Image<TOutputPixelType,2> >  Superclass;

  /** Smart pointer typedef support. */
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Image index typedef */
  typedef typename InputImageType::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename InputImageType::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename InputImageType::RegionType OutputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( HoughTransform2DCirclesImageFilter, ImageToImageFilter );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Method for evaluating the implicit function over the image. */
  void GenerateData();

  /** Set both Minimum and Maximum radius values */
  void SetRadius(double radius);
  
  /** Set the minimum radiu value the filter should look for */
  itkSetMacro(MinimumRadius,double);

  /** Set the maximum radius value the filter should look for */
  itkSetMacro(MaximumRadius,double);

  /** Set the threshold above which the filter should consider
      the point as a valid point */
  itkSetMacro(Threshold,double);

  /** Get the threshold value */
  itkGetMacro(Threshold,double);

  /** Get the radius image */
  itkGetObjectMacro(RadiusImage,OutputImageType);

  /** Set the scale of the derivative function (using DoG) */
  itkSetMacro(SigmaGradient,double);

  /** Get the scale value */
  itkGetMacro(SigmaGradient,double);

protected:

  HoughTransform2DCirclesImageFilter();
  virtual ~HoughTransform2DCirclesImageFilter() {};

  HoughTransform2DCirclesImageFilter(const Self&) {}
  void operator=(const Self&) {}

  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  double m_MinimumRadius;
  double m_MaximumRadius;
  double m_Threshold;
  double m_SigmaGradient;
  OutputImagePointer m_RadiusImage;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHoughTransform2DCirclesImageFilter.txx"
#endif

#endif
