/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConfidenceConnectedImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConfidenceConnectedImageFilter_h
#define __itkConfidenceConnectedImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"

namespace itk{

/** /class ConfidenceConnectedImageFilter
 * /brief Segment pixels with similar statistics using connectivity
 *
 * This filter extracts a connected set of pixels whose pixel
 * intensities are consistent with the pixel statistics of a seed
 * point. The mean and variance across a neighborhood (8-connected,
 * 26-connected, etc.) are calculated for a seed point.  Then
 * pixels connected to this seed point whose values are within
 * the confidence interval for the seed point are grouped. The
 * width of the confidence interval is controlled by the "Multiplier"
 * variable (the confidence interval is the mean plus or minus
 * the "Multiplier" times the standard deviation). If the intensity
 * variations across a segment were gaussian, a "Multiplier" setting
 * of 2.5 would define a confidence interval wide enough to capture
 * 99% of samples in the segment.
 *
 * After this initial segmentation is calculated, the mean and
 * variance are re-calculated. All the pixels in the previous
 * segmentation are used to calculate the mean the standard deviation
 * (as opposed to using the pixels in the neighborhood of the seed
 * point).  The segmentation is then recalculted using these refined
 * estimates for the mean and variance of the pixel values.  This
 * process is repeated for the specified number of iterations.
 * Setting the "NumberOfIterations" to zero stops the algorithm
 * after the initial segmentation from the seed point.
 *
 * NOTE: the lower and upper threshold are restricted to lie within the
 * valid numeric limits of the input data pixel type. Also, the limits
 * may be adjusted to contain the seed point's intensity.
 * \ingroup RegionGrowingSegmentation
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ConfidenceConnectedImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ConfidenceConnectedImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).  */
  itkTypeMacro(ConfidenceConnectedImageFilter,
               ImageToImageFilter);

  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType; 
  typedef typename InputImageType::PixelType    InputImagePixelType; 
  typedef typename InputImageType::IndexType    IndexType;
  typedef typename InputImageType::SizeType     SizeType;
  
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  OutputImageRegionType; 
  typedef typename OutputImageType::PixelType   OutputImagePixelType; 
  
  typedef std::vector< IndexType >              SeedsContainerType;

  void PrintSelf ( std::ostream& os, Indent indent ) const;

  /** Set seed point. This method is deprecated, please use AddSeed() */
  void SetSeed(const IndexType & seed)
  {
    m_Seeds.clear();
    this->AddSeed( seed );
  };


  /** Add seed point. */
  void AddSeed(const IndexType & seed)
  {
    m_Seeds.push_back( seed );
    this->Modified();
  };

  /** Set/Get the multiplier to define the confidence interval.  Multiplier
   * can be anything greater than zero. A typical value is 2.5 */
  itkSetMacro(Multiplier, double);
  itkGetMacro(Multiplier, double);

  /** Set/Get the number of iterations */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetMacro(NumberOfIterations, unsigned int);

  /** Set/Get value to replace thresholded pixels */
  itkSetMacro(ReplaceValue, OutputImagePixelType);
  itkGetMacro(ReplaceValue, OutputImagePixelType);

  /** Get/Set the radius of the neighborhood over which the
      statistics are evaluated */
  itkSetMacro( InitialNeighborhoodRadius, unsigned int );
  itkGetConstReferenceMacro( InitialNeighborhoodRadius, unsigned int );

protected:
  ConfidenceConnectedImageFilter();
  ~ConfidenceConnectedImageFilter(){};
  
  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion();

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output);

  void GenerateData();
  
private:
  ConfidenceConnectedImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  SeedsContainerType      m_Seeds;
  double                  m_Multiplier;
  unsigned int            m_NumberOfIterations;
  OutputImagePixelType    m_ReplaceValue;
  unsigned int            m_InitialNeighborhoodRadius;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConfidenceConnectedImageFilter.txx"
#endif

#endif
