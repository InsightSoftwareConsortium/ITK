/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHoughTransform2DLinesImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHoughTransform2DLinesImageFilter_h
#define __itkHoughTransform2DLinesImageFilter_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkLineSpatialObject.h"

namespace itk
{

/**
 * \class HoughTransform2DLinesImageFilter
 * \brief Performs the Hough Transform to find 2D straight lines
 *        in a 2D image.
 *  
 * This filter derives from ImageToImageFilter
 * The input is an image, and all pixels above some threshold are those
 * to be extracted. The output is the image of the accumulator.
 * GetLines() returns a list of LinesSpatialObjects
 *
 * Lines are parameterized in the form: R = x*cos(Teta)+y*sin(Teta)
 * where R is the perpendicular distance from the origin and Teta 
 * the angle with the normal.
 *
 * The output is the accumulator array:
 *    -The first dimension (X) represents the distance R from the corner 
 *     to the line
 *    -The second dimension (Y) represents the angle between the X axis
 *     and the normal to the line.
 *   
 * The size of the array depends on the AngleAxisSize that could be set
 * (500 by default) for the angle axis. The distance axis depends on the
 * size of the diagonal of the input image.
 *
 * \ingroup ImageFeatureExtraction 
 * \sa LineSpatialObject
 *
 * */

template<typename TInputPixelType, typename TOutputPixelType>
class ITK_EXPORT HoughTransform2DLinesImageFilter :
public ImageToImageFilter< Image<TInputPixelType,2>, Image<TOutputPixelType,2> >
{
public:
   
  /** Standard "Self" typedef. */
  typedef HoughTransform2DLinesImageFilter Self;

  /** Input Image typedef */
  typedef Image<TInputPixelType,2> InputImageType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  
  /** Output Image typedef */
  typedef Image<TOutputPixelType,2> OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Smart pointer typedef support. */
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Line typedef */
  typedef LineSpatialObject<2>         LineType;
  typedef typename LineType::Pointer   LinePointer;
  typedef std::list<LinePointer>       LinesListType;
  typedef LineType::LinePointType      LinePointType;

  /** Standard "Superclass" typedef. */
  typedef ImageToImageFilter<InputImageType, OutputImageType>  Superclass;

  /** Number of dimensions */
  itkStaticConstMacro(NDimension, unsigned int,InputImageType::ImageDimension);

  /** Image size typedef */
  typedef Size<InputImageType::ImageDimension> SizeType;

  /** Image index typedef */
  typedef typename InputImageType::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename InputImageType::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename InputImageType::RegionType OutputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( HoughTransform2DLinesImageFilter, ImageToImageFilter );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Method for evaluating the implicit function over the image. */
  void GenerateData();

  /** Set the threshold above which the filter should consider
      the point as a valid point */
  itkSetMacro(Threshold,float);

  /** Get the threshold value */
  itkGetMacro(Threshold,float);

  /** Set the resolution angle:
      The hough space descibes (in the angle direction) [-PI,PI[
      with a constant stepe AngleResolution */
  itkSetMacro(AngleResolution,float);

  /** Get the resolution angle */
  itkGetMacro(AngleResolution,float);

  /** Set the angle axis size:
      This value determine the size (in Y) of the accumulator image */
  itkSetMacro(AngleAxisSize,unsigned int);

  /** Get the resolution angle */
  itkGetMacro(AngleAxisSize,float);

  /** Simplify the accumulator */
  void Simplify(void);

  /** Get the Simplified accumulator */
  itkGetObjectMacro(SimplifyAccumulator,OutputImageType);

  /** Get the list of lines. This recomputes the lines */
  LinesListType & GetLines(unsigned int n=0);

  /** Set/Get the number of lines to extract */
  itkSetMacro(NumberOfLines,unsigned int);
  itkGetMacro(NumberOfLines,unsigned int);

  /** Set/Get the radius of the disc to remove from the accumulator
   *  for each line found */
  itkSetMacro(DiscRadius,float);
  itkGetMacro(DiscRadius,float);

  /** Set the variance of the gaussian bluring for the accumulator */
  itkSetMacro(Variance,float);
  itkGetMacro(Variance,float);

protected:

  HoughTransform2DLinesImageFilter();
  virtual ~HoughTransform2DLinesImageFilter() {};

  HoughTransform2DLinesImageFilter(const Self&) {}
  void operator=(const Self&) {}

  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  float m_AngleResolution;
  float m_Threshold;
  float m_AngleAxisSize;
  OutputImagePointer m_SimplifyAccumulator;
  LinesListType m_LinesList;
  unsigned int  m_NumberOfLines;
  float         m_DiscRadius;
  float         m_Variance;
  unsigned long m_OldModifiedTime;
  unsigned long m_OldNumberOfLines;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHoughTransform2DLinesImageFilter.txx"
#endif

#endif
