/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatImageToHistogram.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkStatImageToHistogram_h
#define __itkStatImageToHistogram_h

#include <itkProcessObject.h>
#include <itkObjectFactory.h>
#include "itkStatDenseHistogram.h"
#include "itkStatSparseHistogram.h"

namespace itk{

/** \class StatImageToHistogram 
 *  \brief It calculates histogram from a source image. 
 *
 */
template <class TImage, class THistogram>
class ITK_EXPORT StatImageToHistogram :
    public ProcessObject 
{
public:
 /**
  * Standard "Self" typedef.
  */
  typedef StatImageToHistogram  Self;

 /** 
  * Smart pointer typedef support 
  */
  typedef SmartPointer<Self>   Pointer;

 /**
  *  Pointer type for input image
  */
  typedef typename TImage::Pointer ImagePointer;

 /**
  *  Pointer type for output histogram
  */
  typedef typename THistogram::Pointer HistogramPointer;

 /**
  * Pixel type of an input image
  */
  typedef typename TImage::PixelType PixelType;

 /**
  * Method for creation through the object factory.
  */
  itkNewMacro(Self);

 /**
  * Method for executing the :algorithm
  */
  void GenerateData(void);
  
 /**
  * Connect a source image to fileter
  */
  void SetInput(ImagePointer image);

 /**
  * Connect a source image to fileter
  */
  void SetOutput(HistogramPointer histogram)
    {  m_Histogram = histogram; }

 /**
  * Get input image pointer
  */
  ImagePointer GetInput(){ return m_Image; };

 /**
  * Get output histogram pointer
  */
  HistogramPointer GetOutput(){ return m_Histogram; };
  
 
  /**
   * Set the lower bound of the histogram
   */
  //void SetLowerBound(float lower);

  /**
   * Set the upper bound of the histogram
   */
  //void SetUpperBound(float upper);

  /**
   * Set the number of bins in the histogram
   */
  //void SetNBins(int nbins);
  
protected:

  StatImageToHistogram();
  virtual ~StatImageToHistogram() {};
  StatImageToHistogram(const Self&) {};
  void operator=(const Self&) {};

private:

  //float m_LowerBound;
  //float m_UpperBound;
  //int   m_NBins;

  //bool  m_LowerFlag;
  //bool  m_UpperFlag;
  //bool  m_NBinsFlag;

  // Pointer for input image
  ImagePointer m_Image;

  // Pointer for output histogram
  HistogramPointer m_Histogram;

};

} // end of namespace

#ifndef __itkStatImageToHistogram_txx
#include "itkStatImageToHistogram.txx"
#endif

#endif
