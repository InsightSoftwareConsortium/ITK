/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFuzzyConnectednessImageFilterBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkSimpleFuzzyConnectednessImageFilterBase_h
#define __itkSimpleFuzzyConnectednessImageFilterBase_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include <vnl/vnl_matrix_fixed.h>

#include <queue>

namespace itk{

/** /class SimpleFuzzyConnectednessImageFilterBase
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

template <class TInputImage, class TOutputImage>
class SimpleFuzzyConnectednessImageFilterBase:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SimpleFuzzyConnectednessImageFilterBase       Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleFuzzyConnectednessImageFilterBase,ImageToImageFilter);

  /** Capture the image dimension from the input template parameters. */
  enum {ImageDimension = TInputImage::ImageDimension };

  /** Convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef Image <unsigned short, ImageDimension> UShortImage;
  typedef typename TInputImage::IndexType IndexType;
  typedef typename TInputImage::SizeType SizeType;
  typedef typename TInputImage::PixelType PixelType;
  typedef typename UShortImage::Pointer FuzzyScene;
  typedef std::queue<IndexType> QueueType;
  typedef typename TOutputImage::RegionType RegionType;
  
  /** Set/Get the weight of the first term (standard statistics) in the
   * affinity computation. */
  itkSetMacro(Weight, double);
  itkGetMacro(Weight, double);
  
  /** Set/Get the threshold value for the segmentation. */
  itkSetMacro(Threshold, double);
  itkGetMacro(Threshold, double);

  /** Setting the starting point, believed to be inside the object. */
  void SetSeed(const IndexType & seed);
  
  /** Update the binary result (needed after an update the threshold). */
  void MakeSegmentObject();

  /** Extract the FuzzyScene not thresholded. */
  FuzzyScene GetFuzzyScene(void)
    { return m_FuzzyScene; };

  /** A simple combination of SetThreshold and MakeSegmentObject methods. */
  void UpdateThreshold(const double x);
  
protected:
  SimpleFuzzyConnectednessImageFilterBase();
  ~SimpleFuzzyConnectednessImageFilterBase();

  /** Standard pipeline method.*/
  void GenerateData();

private:
  SimpleFuzzyConnectednessImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  double m_Weight;
  double m_Threshold;
  IndexType m_Seed;
  SizeType m_Size;

  typename InputImageType::Pointer m_InputImage;
  typename UShortImage::Pointer m_FuzzyScene;
  typename OutputImageType::Pointer m_SegmentObject; 
  
  QueueType m_Queue;

  void PushNeighbors(const IndexType &center);

  /** Define the fuzzy affinity function between two pixels. */
  virtual double FuzzyAffinity(const PixelType f1, const PixelType f2)
    { return 0; }

  double FindStrongPath(const IndexType &center);
};


} /** end namespace itk. */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleFuzzyConnectednessImageFilterBase.txx"
#endif

#endif
