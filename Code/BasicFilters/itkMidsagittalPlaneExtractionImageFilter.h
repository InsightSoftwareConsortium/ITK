/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMidsagittalPlaneExtractionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMidsagittalPlaneExtractionImageFilter_h
#define __itkMidsagittalPlaneExtractionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkPoint.h"

namespace itk
{
  
/** \class MidsagittalPlaneExtractionImageFilter
 * \brief Implements a Reflection of an image along a selected direction.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.  
 * 
 * \ingroup   IntensityImageFilters     Singlethreaded
 */
  typedef struct
  {
    double shiftX;
    double *shiftsY;
    double stepY;
    double n;
    double center;
    double angleEstimate;
//    itk::Point<double, InputImageType::ImageDimension> flippedCenterOfMass;
//    itk::Point<double, 3> flippedCenterOfMass;
//    itk::Point flippedCenterOfMass;
    double *flippedCenterOfMass;
  } estimateType;

 template <class TInputImage, class TOutputImage>
class ITK_EXPORT MidsagittalPlaneExtractionImageFilter : public ImageToImageFilter<TInputImage,TOutputImage> 
{
public:
  /** Standard class typedefs. */
  typedef MidsagittalPlaneExtractionImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(MidsagittalPlaneExtractionImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename    InputImageType::Pointer    InputImagePointer;
  typedef typename    InputImageType::RegionType InputImageRegionType; 
  typedef typename    InputImageType::PixelType  InputImagePixelType; 
  typedef TOutputImage OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;


  /** Set the direction in which to reflect the data. */
  itkGetMacro( Direction, unsigned int );
  itkSetMacro( Direction, unsigned int );
  

protected:
  MidsagittalPlaneExtractionImageFilter();
  virtual ~MidsagittalPlaneExtractionImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  double FindAngle(typename Superclass::InputImageConstPointer, int angleChoice, int numberOfPoints, double step, estimateType* anEstimate);
  double FindShift(typename Superclass::InputImageConstPointer, double numberOfPoints, double step, double shiftEstimate);
  double findHighestPoint(typename Superclass::InputImageConstPointer, int dimensionChoice, double startingHeight);
//  itk::Point<double, 3> *findCentroid(typename Superclass::InputImageConstPointer);
//  itk::Point<double, TInputImage::ImageDimension> *findCentroid(typename Superclass::InputImageConstPointer);

//  itk::Point<double, 3> *findCentroid(typename Superclass::InputImageConstPointer volume, TInputImage::RegionType::IndexType dimensionMask);
//  itk::Point<double, 3> *findCentroid(typename Superclass::InputImageConstPointer volume);
    double *findCentroid(typename Superclass::InputImageConstPointer volume, double* dimensionMask);

//  itk::Point *findCentroid(typename Superclass::InputImageConstPointer, TInputImage::RegionType::IndexType dimensionMask);
  estimateType* estimateAngle(typename Superclass::InputImageConstPointer);
  double estimateShift(typename Superclass::InputImageConstPointer);
  /** This method implements the actual reflection of the image.
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void GenerateData(void);

private:
  MidsagittalPlaneExtractionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int m_Direction;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMidsagittalPlaneExtractionImageFilter.txx"
#endif

#endif
