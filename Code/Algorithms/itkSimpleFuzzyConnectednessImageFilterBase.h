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
 *
 * Detail information about this algorithm can be found in:
 *  "Fuzzy Connectedness and Object Definition: Theory, Algorithms,
 *    and Applications in Image Segmentation", J. Udupa and S. Samarasekera
 *  Graphical Models and Image Processing, Vol.58, No.3. pp 246-261, 1996.
 *
 * \ingroup FuzzyConnectednessSegmentation  
 */

template <class TInputImage, class TOutputImage>
class SimpleFuzzyConnectednessImageFilterBase:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SimpleFuzzyConnectednessImageFilterBase       Self;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SimpleFuzzyConnectednessImageFilterBase,ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  enum {ImageDimension = TInputImage::ImageDimension };

  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef Image <unsigned short, ImageDimension> UShortImage;
  typedef typename TInputImage::IndexType IndexType;
  typedef typename TInputImage::SizeType SizeType;
  typedef typename TInputImage::PixelType PixelType;
  typedef typename UShortImage::Pointer FuzzyScene;

  typedef std::queue<IndexType> QueueType;

  typedef typename TOutputImage::RegionType RegionType;
  
  /**
   * Set the Weight of the first term(standard statistics) in the affinity 
   * computation.
   */
  itkSetMacro(Weight, double);
  /**
   * Get the Weight of the first term(standard statistics) in the affinity 
   * computation.
   */
  itkGetMacro(Weight, double);
  
  /**
   * Set the Threshold value for the segmentation.
   */
  itkSetMacro(Threshold, double);
  /**
   * Get the Threshold value for the segmentation.
   */
  itkGetMacro(Threshold, double);

  /**
   * Setting the beginning point, believed to be inside the object.
   */
  void SetSeed(const IndexType & seed);
	
  /**
  * Standard pipeline method.
  */
  void GenerateData();

  /**
   * Update the binary result. (needed after update the threshold)
   */
  void MakeSegmentObject();

  FuzzyScene GetFuzzyScene(void){ return m_FuzzyScene; };

  /**
   * a simple combining of set threshold and makesegmentobject.
   */
  void UpdateThreshold(const double x);
  
protected:
  SimpleFuzzyConnectednessImageFilterBase();
  ~SimpleFuzzyConnectednessImageFilterBase();

  double m_Weight;
  double m_Threshold;
  IndexType m_Seed;
  SizeType m_size;

  typename InputImageType::Pointer m_InputImage;
  typename UShortImage::Pointer m_FuzzyScene;
  typename OutputImageType::Pointer m_SegmentObject; 
  
  QueueType m_Queue;

  void PushNeighbors(const IndexType &center);

  /*
   * define the fuzzy affinity function between two pixels,
   * need to be implement by the sub-classes.
   */
  virtual double FuzzyAffinity(const PixelType f1, const PixelType f2){return 0;};

  double FindStrongPath(const IndexType &center);
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimpleFuzzyConnectednessImageFilterBase.txx"
#endif

#endif
