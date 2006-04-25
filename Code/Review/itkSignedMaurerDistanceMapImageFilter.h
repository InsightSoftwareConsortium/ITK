/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSignedMaurerDistanceMapImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSignedMaurerDistanceMapImageFilter_h
#define __itkSignedMaurerDistanceMapImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"

/** \class SignedMaurerDistanceMapImageFilter
 *
 *  \brief This filter calculates the squared Euclidean distance transform 
 *  of a binary image in linear time for arbitrary dimensions. 
 *
 *  \par Inputs and Outputs
 *  This is an image-to-image filter.  The dimensionality is arbitrary.  The
 *  only dimensionality constraint is that the input and output images be of
 *  the same dimensions and size.  To maintain integer arithmetic within the
 *  filter, the default output is the signed squared distance.  This implies
 *  that the input image should be of type "unsigned int" or "int" whereas the
 *  output image is of type "int".  Obviously, if the user wishes to utilize
 *  the image spacing or to have a filter with the Euclidean distance (as
 *  opposed to the squared distance), output image types of float or double
 *  should be used.
 *
 *  The inside is considered as having negative distances. Outside is treated
 *  as having positive distances. To change the convention, use the
 *  InsideIsPositive(bool) function.
 *
 *  \par Parameters
 *  Set/GetBackgroundValue specifies the background of the value of the input
 *  binary image.  Normally this is zero and, as such, zero is the default
 *  value.  Other than that, the usage is completely analagous to the
 *  itkDanielssonDistanceImageFilterClass except is does not return the Voronoi
 *  map.
 *
 *  \cite C. R. Maurer, Jr., R. Qi, and V. Raghavan, "A Linear Time Algorithm
 *  for Computing Exact Euclidean Distance Transforms of Binary Images in
 *  Arbitrary Dimensions", IEEE - Transactions on Pattern Analysis and Machine
 *  Intelligence, 25(2): 265-270, 2003.
 *
 * \ingroup ImageFeatureExtraction 
 *
 */

namespace itk
{

template <class TInputImage, class TOutputImage>
class SignedMaurerDistanceMapImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:

  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);


  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage    InputImageType;
  typedef TOutputImage   OutputImageType;

  /** Standard class typedefs. */
  typedef SignedMaurerDistanceMapImageFilter   Self;
  typedef ImageToImageFilter<
                       InputImageType, 
                       OutputImageType>        Superclass;

  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>             ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Image typedef support. */
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;

  typedef typename InputImageType::SizeType    InputSizeType;
  typedef typename OutputImageType::SizeType   OutputSizeType;

  typedef typename InputImageType::IndexType   InputIndexType;
  typedef typename OutputImageType::IndexType  OutputIndexType;

  typedef typename InputImageType::SpacingType  InputSpacingType;
  typedef typename OutputImageType::SpacingType OutputSpacingType;

  /** Set if the distance should be squared. */
  itkSetMacro(SquaredDistance, bool);

  /** Get the distance squared. */
  itkGetConstReferenceMacro(SquaredDistance, bool);

  /** Set On/Off if the distance is squared. */
  itkBooleanMacro(SquaredDistance);

  /** Set if the inside represents positive values in the signed distance
   *  map. By convention ON pixels are treated as inside pixels.           */
  itkSetMacro(InsideIsPositive, bool);

  /** Get if the inside represents positive values in the signed distance map. 
   *  See GetInsideIsPositive()  */
  itkGetConstReferenceMacro(InsideIsPositive, bool);

  /** Set if the inside represents positive values in the signed distance
   * map. By convention ON pixels are treated as inside pixels. Default is 
   * true.                             */
  itkBooleanMacro(InsideIsPositive);

  /** Set if image spacing should be used in computing distances. */
  itkSetMacro(UseImageSpacing, bool);

  /** Get whether spacing is used. */
  itkGetConstReferenceMacro(UseImageSpacing, bool);

  /** Set On/Off whether spacing is used. */
  itkBooleanMacro(UseImageSpacing);

  /** 
   * Set the background value which defines the object.  Usually this
   * value is = 0.
   */
  itkSetMacro(BackgroundValue, InputPixelType);
  itkGetConstReferenceMacro(BackgroundValue, InputPixelType);

protected:

  SignedMaurerDistanceMapImageFilter();

  virtual ~SignedMaurerDistanceMapImageFilter();
  
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

  
private:

  SignedMaurerDistanceMapImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  void VoronoiEDT(unsigned int, OutputIndexType);
  bool RemoveEDT(OutputPixelType, OutputPixelType, OutputPixelType, 
                 OutputPixelType, OutputPixelType, OutputPixelType);
  
  typename InputImageType::Pointer    m_BinaryImage;
  InputPixelType                      m_BackgroundValue;  
  OutputPixelType                     m_MaximumValue;
  InputSpacingType                    m_Spacing;

  bool     m_InsideIsPositive;
  bool     m_UseImageSpacing;
  bool     m_SquaredDistance;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSignedMaurerDistanceMapImageFilter.txx"
#endif

#endif

