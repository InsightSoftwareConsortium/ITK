/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedThresholdImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConnectedThresholdImageFilter_h
#define __itkConnectedThresholdImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"

namespace itk{

/** /class ConnectedThresholdImageFilter
 * \brief Label pixels that are connected to a seed and lie within a range of values
 * 
 * ConnectedThresholdImageFilter labels pixels with ReplaceValue that are
 * connected to an initial Seed AND lie within a Lower and Upper
 * threshold range.
 *
 * \ingroup RegionGrowingSegmentation 
 */
template <class TInputImage, class TOutputImage>
class ConnectedThresholdImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ConnectedThresholdImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).  */
  itkTypeMacro(ConnectedThresholdImageFilter,
               ImageToImageFilter);

  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  typedef typename InputImageType::IndexType IndexType;
  typedef typename InputImageType::SizeType SizeType;
  
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType; 
  typedef typename OutputImageType::PixelType OutputImagePixelType; 
  
  void PrintSelf ( std::ostream& os, Indent indent ) const;

  
  /** Set seed point. */
  void SetSeed ( const IndexType & seed )
  {
    this->ClearSeeds();
    this->AddSeed ( seed );
  }
  void AddSeed(const IndexType & seed)
  {
    m_SeedList.push_back ( seed );
  };

  /** Clear the seed list. */
  void ClearSeeds ()
  {
    m_SeedList.clear();
  };

  /** Set/Get the lower threshold. The default is 0. */
  itkSetMacro(Lower, InputImagePixelType);
  itkGetMacro(Lower, InputImagePixelType);

  /** Set/Get the upper threshold. The default is the largest possible
   *  value for the InputPixelType. */
  itkSetMacro(Upper, InputImagePixelType);
  itkGetMacro(Upper, InputImagePixelType);
  
  /** Set/Get value to replace thresholded pixels. Pixels that lie *
   *  within Lower and Upper (inclusive) will be replaced with this
   *  value. The default is 1. */
  itkSetMacro(ReplaceValue, OutputImagePixelType);
  itkGetMacro(ReplaceValue, OutputImagePixelType);

protected:
  ConnectedThresholdImageFilter();
  ~ConnectedThresholdImageFilter(){};
  std::vector<IndexType> m_SeedList;
  InputImagePixelType m_Lower;
  InputImagePixelType m_Upper;
  OutputImagePixelType m_ReplaceValue;
  
  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion();

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output);

  void GenerateData();
  
private:
  ConnectedThresholdImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedThresholdImageFilter.txx"
#endif

#endif
