/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFuzzyConnectednessImageFilterBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimpleFuzzyConnectednessImageFilterBase_h
#define __itkSimpleFuzzyConnectednessImageFilterBase_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include <vnl/vnl_matrix_fixed.h>

#include <queue>

namespace itk
{
/** \class SimpleFuzzyConnectednessImageFilterBase
 * \brief Base class for FuzzyConnectednessImageFilter object.
 *
 * Fuzzy connectedness image filter works on multi-dimensional image.
 * Fuzzy affinity is defined between two nearby pixels in a image, it has
 * higher value when the two pixel are closer, and the pixel value is similar
 * also both of them are similar to the defined object pixels.
 * Strength of a "path" between two pixels was defined as the weakest affinity
 * between pairs of connected pixels along the "path",
 * Fuzzy Connectedness of two pixels was defined as the strongest path strength
 * among all possible paths between the two pixels.
 * A fuzzy object was defined as the collection of pixels that within this collection
 * each pair of pixel have a strong fuzzy connectedness (say, above some threshold)
 * any pixel outside the object will have a weak fuzzy connectedness to any pixels
 * inside the object.
 * The simple fuzzy connectedness image filter implents the compute of a fuzzy
 * object by given prior information of the object.
 * this is the base class, all sub-classes should implement the definition of
 * fuzzy affinity between two nearby pixels. In this segmenation, all fuzzy affinity
 * was only defined between the 4-connected neighbor pixels.
 *
 * Detail information about this algorithm can be found in:
 *  "Fuzzy Connectedness and Object Definition: Theory, Algorithms,
 *    and Applications in Image Segmentation", J. Udupa and S. Samarasekera
 *  Graphical Models and Image Processing, Vol.58, No.3. pp 246-261, 1996.
 *
 * \ingroup FuzzyConnectednessSegmentation  */

template< class TInputImage, class TOutputImage >
class ITK_EXPORT SimpleFuzzyConnectednessImageFilterBase:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef SimpleFuzzyConnectednessImageFilterBase         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleFuzzyConnectednessImageFilterBase, ImageToImageFilter);

  /** Capture the image dimension from the input template parameters. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Convenient typedefs. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  typedef Image< unsigned short, itkGetStaticConstMacro(ImageDimension) > UShortImage;

  typedef typename TInputImage::IndexType       IndexType;
  typedef typename TInputImage::SizeType        SizeType;
  typedef typename TInputImage::PixelType       PixelType;
  typedef UShortImage                           FuzzySceneType;
  typedef typename FuzzySceneType::Pointer      FuzzyScenePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef std::queue< IndexType >               QueueType;
  typedef typename OutputImageType::RegionType  RegionType;
  typedef typename OutputImageType::PixelType   OutputPixelType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;

  /** Set/Get the weight of the first term (standard statistics) in the
   * affinity computation. */
  itkSetMacro(Weight, double);
  itkGetMacro(Weight, double);

  /** Set/Get the threshold value for the segmentation. This threshold value is
   * scaled with respect to the range of the unsigned short type.  The
   * resulting value is used to thresold the affinity map and generate a binary
   * image as output. */
  itkSetMacro(Threshold, double);
  itkGetMacro(Threshold, double);

  /** Set/Get the value to assign to the inside of the segmented object.
   *  By default this value is set to the maximum value of the output pixel type */
  itkSetMacro(InsideValue, OutputPixelType);
  itkGetMacro(InsideValue, OutputPixelType);

  /** Set/Get the value to assign to the outside of the segmented object.
   *  By default this value is set to the minimum value of the output pixel type */
  itkSetMacro(OutsideValue, OutputPixelType);
  itkGetMacro(OutsideValue, OutputPixelType);

  /** Setting the starting point, believed to be inside the object. */
  itkSetMacro(ObjectSeed, IndexType);
  itkGetConstReferenceMacro(ObjectSeed, IndexType);

  /** Update the binary result (needed after an update the threshold). */
  void MakeSegmentObject();

  /** Extract the FuzzyScene not thresholded. */
  const FuzzySceneType * GetFuzzyScene(void) const
  { return m_FuzzyScene; }

  /** A simple combination of SetThreshold and MakeSegmentObject methods. */
  void UpdateThreshold(const double x);

protected:
  SimpleFuzzyConnectednessImageFilterBase();
  ~SimpleFuzzyConnectednessImageFilterBase();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Standard pipeline method. */
  void GenerateData();

  double    m_Weight;
  double    m_Threshold;
  IndexType m_ObjectSeed;
  SizeType  m_Size;

  InputImageConstPointer m_InputImage;
  FuzzyScenePointer      m_FuzzyScene;
  OutputImagePointer     m_SegmentObject;

  QueueType m_Queue;

  void PushNeighbors(const IndexType & center);

  /** Define the fuzzy affinity function between two pixels. */
  virtual double FuzzyAffinity(const PixelType, const PixelType)
  { return 0; }

  double FindStrongPath(const IndexType & center);

private:
  SimpleFuzzyConnectednessImageFilterBase(const Self &); //purposely not
                                                         // implemented
  void operator=(const Self &);                          //purposely not
                                                         // implemented

  OutputPixelType m_InsideValue;
  OutputPixelType m_OutsideValue;
};
} // end namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleFuzzyConnectednessImageFilterBase.txx"
#endif

#endif
