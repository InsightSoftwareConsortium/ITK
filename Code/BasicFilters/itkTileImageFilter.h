/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTileImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTileImageFilter_h
#define __itkTileImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk {

/** \class TileImageFilter 
 * \brief Tile multiple input images into a single output image.
 *
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT TileImageFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard Self typedef */
  typedef TileImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(TileImageFilter, ImageToImageFilter);
  
  /** Image pixel value typedef. */
  typedef typename TInputImage::PixelType   InputPixelType;
  typedef typename TOutputImage::PixelType   OutputPixelType;
  
  /** Image related typedefs. */
  typedef typename TInputImage::Pointer InputImagePointer;
  typedef typename TOutputImage::Pointer OutputImagePointer;

  typedef typename TInputImage::SizeType  InputSizeType;
  typedef typename TInputImage::IndexType  InputIndexType;
  typedef typename TInputImage::RegionType InputImageRegionType;
  typedef typename TOutputImage::SizeType  OutputSizeType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;


  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension ) ;
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension ) ;
  // Define a tile structure 
  class TileInfo {
  public:
    int m_ImageNumber;
    OutputImageRegionType m_Region;
    TileInfo() : m_ImageNumber(-1) {}
  };

  typedef Image<TileInfo,itkGetStaticConstMacro(OutputImageDimension)> TileImageType;

  /** LayoutArray type. */
  typedef FixedArray<unsigned int, itkGetStaticConstMacro(OutputImageDimension)> LayoutArrayType;

  /** Set/Get the layout of the tiles. */
  itkSetMacro(Layout,LayoutArrayType);
  itkGetMacro(Layout,LayoutArrayType);

  /** Set the pixel value for locations that are not covered by an
   * input image. The default default pixel value is 0. */
  itkSetMacro(DefaultPixelValue,OutputPixelType);

  /** Get the pixel value for locations that are not covered by an
   * input image. */
  itkGetMacro(DefaultPixelValue,OutputPixelType);

protected:
  TileImageFilter();
  ~TileImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateInputRequestedRegion();
  void GenerateOutputInformation();

  void  GenerateData ();

private:
  TileImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


  typename TileImageType::Pointer m_TileImage;
  OutputPixelType m_DefaultPixelValue;
  LayoutArrayType m_Layout;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTileImageFilter.txx"
#endif

#endif
