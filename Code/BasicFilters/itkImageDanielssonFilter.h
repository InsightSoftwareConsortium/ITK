/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageDanielssonFilter.h itkImageDanielssonFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageDanielssonFilter_h
#define __itkImageDanielssonFilter_h

#include <itkProcessObject.h>
#include <itkSimpleImageRegionIterator.h>

namespace itk
{

/** \class ImageDanielssonFilter
*
*This class is parametrized over the type of the input image
*and the type of the output image.
*This class returns distance map of a 2D or 3D input image 
*and the Closest Point map.
*
*/

template <class TInputImage,class TOutputImage>
class ITK_EXPORT ImageDanielssonFilter :
   public ProcessObject
{
public:

	/**
	 * Enumeration for different types of metric
	 */
	typedef enum Metric 
	{FOUR=4,
	 SIX=6,
	 EIGHT=8,
     EIGHTEEN=18,
     TWENTY_SIX=26
	};
	/**
	 * Standard "Self" typedef.
	 */
	typedef ImageDanielssonFilter Self;
	/**
	 *Smart pointer typedef support
	 */
	typedef SmartPointer<Self> Pointer;
	/**
	 *Pointer Type for input image
	 */
	typedef typename TInputImage::Pointer InputImagePointer;
    /**
	 *Pointer Type for the output image
	 */
	typedef typename TOutputImage::Pointer OutputImagePointer;
    /**
	 *Input and output image iterators
	 */
	typedef itk::SimpleImageRegionIterator< TInputImage > InputImageIterator;
    typedef itk::SimpleImageRegionIterator< TOutputImage > OutputImageIterator;
    /**
	 *Set if the distance should be squared
	 */
    void SetSquaredDistance( bool NewSquareDistance );
	/**
	 *Get the distance squared
	 */
    bool GetSquareDistance();
    /**
	 *Set the metric
	 *Default values are 6, corresponding to a V6 neighborhood in 3D,
	 *and 8 corresponding to a V8 neighborhood in 2D
	 *( V4 in 2D and V18, V26 in 3D are also available)
	 */
    void SetMetric( unsigned char NewMetric );
    /**
	 *Get the metric
	 */
    unsigned char GetMetric();
    /**
	 *Set if Closest Point Map should be computed 
	 */
	void SetClosestComputation(bool NewClosestComputation);
    /**
	 *Get ClosestComputation
	 */
    unsigned char GetClosestComputation();
    /**
	 *Compute Danielsson distance map and/or Closest Points Map
	 */
	void Execute(void);  
    /**
     *Get Closest Point Map
     *This Map shows for each pixel what object is closest to it. 
	 *Each object should be labeled by a number (larger than 0), 
	 *so the map has a value for each pixel corresponding to the label 
	 *of the closest object. 
	 */
	InputImagePointer GetClosestPoints(void);
	/**
	 *Get Distance map image
	 *The distance map is shown as a gray value image depending on 
	 *the pixel type of the output image.
	 *Regarding the source image, background should be dark (gray value = 0) 
	 *and object should have a gray value larger than 0. 
	 *The minimal distance is calculated on the object frontier, 
	 *and the output image gives for each pixel its minimal distance from the object 
	 *(if there is more than one object the closest object is considered).
	 */
	OutputImagePointer GetOutput(void);
	/**
	 *Connect input image to the filter
	 */
    void SetInputImage(InputImagePointer InputImage);
	/**
	 *Method for creation through the object factory
	 */
    itkNewMacro(Self);


protected:

  InputImagePointer  m_inputimage;
  OutputImagePointer m_outputimage;
  InputImagePointer  m_outputclosestpoints;

  int                m_Dimension;
  Metric             m_Metric;
  bool               m_SquaredDistance;
  bool               m_ClosestComputation;

  void Danielsson(const InputImagePointer,unsigned char,short *&,short *&,
			   short *&);
  void update(int &distance,int *square_tablek,short *&dx,short *&dy,
			short *&dz,int ddx,int ddy, int ddz,int size,int sizex, 
			int sizey, int sizez,int index);

  ImageDanielssonFilter();
  virtual ~ImageDanielssonFilter() {};
  ImageDanielssonFilter(const Self&) {}
  void operator=(const Self&) {}
};


} //end namespace itk


#ifndef ITK_MANUAL_INSTANTATION
#include "itkImageDanielssonFilter.txx"
#endif

#endif