/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedImageFilter.h
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
#ifndef __itkWatershedImageFilter_h
#define __itkWatershedImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkWatershedSegmenter.h"
#include "itkWatershedSegmentTreeGenerator.h"
#include "itkWatershedRelabeler.h"
#include "itkCommand.h"

namespace itk
{
/** \class WatershedImageFilter
 *  \brief A low-level image analysis algorithm that automatically produces a
 *   hierarchy of segmented, labeled images from a scalar-valued image input.
 *
 * \par Overview and terminology
 * \par
 * This filter implements a non-streaming version of an image segmentation
 * algorithm commonly known as ``watershed segmentation''.   Watershed
 * segmentation gets its name from the manner in which the algorithm  segments
 * regions into catchment basins. If a function \f$ f \f$ is a continuous
 * height function defined over an image domain, then a catchment basin is
 * defined as the set of points whose paths of steepest descent terminate at
 * the same local minimum of \f$ f \f$. 
 *
 * \par
 * The choice of height function (input) depends on the application, and the
 * basic watershed algorithm operates independently of that choice. For
 * intensity-based image data, you might typically use some sort of gradient
 * magnitude calculation as input. (see itk::GradientMagnitudeImageFilter)
 * 
 * \par
 * The watershed algorithm proceeds in several steps. First, an initial
 * classification of all points into catchment basin regions is done by tracing 
 * each point down its path of steepest descent to a local minima. Next,
 * neighboring regions and the boundaries between them are analyzed according
 * to  some saliency measure (such as minimum boundary height) to produce a
 * tree of merges among adjacent regions.  These merges occur at different
 * maximum saliency values.  The collective set of all possible merges up to a
 * specified saliency ``flood level'' is referred to in this documentation as a
 * ``merge tree''.  Metaphorically, the flood level is a value that reflects
 * the amount of precipitation that is rained into the catchment basins.  As
 * the flood level rises, boundaries between adjacent segments erode and those
 * segments merge.  The minimum value of the flood level is zero and the
 * maximum value is the difference between the highest and lowest values in the
 * input image. 
 *
 * \par
 * Note that once the initial analysis and segmentation is done to produce
 * the merge tree, it is trivial to produce a hierarchy of labeled images in
 * constant time.  The complexity of the algorithm is in the computation
 * of the merge tree.  Once that tree has been created, the initial segmented
 * image can be relabeled to reflect any maximum saliency value found in the
 * tree by simply identifying a subset of segment merges from the tree.
 *
 * \par Implementational details
 * This filter is a wrapper for several lower level process objects (watershed
 * algorithm components in the namespace ``watershed'').  For a more complete
 * picture of the implementation, refer to the documentation of those components.
 * The component classes were designed to operate in either a data-streaming or
 * a non-data-streaming mode.  The pipeline constructed in this class'
 * GenerateData() method does not support streaming, but is the common use case 
 * for the components.
 *
 * \par Description of the input to this filter
 * The input to this filter is a scalar itk::Image of any dimensionality.  This 
 * input image is assumed to represent some sort of height function or edge map 
 * based on the original image that you want to segment (such as would be
 * produced by itk::GradientMagnitudeImageFilter).  This filter does not do any 
 * pre-processing on its input other than a thresholding step. The algorithm
 * does not explicitly require that the input be of any particular data type,
 * but floating point or double precision data is recommended.
 *
 * \par 
 * The recommended pre-processing for scalar image input to this algorithm is
 * to use one of the itk::AnisotropicDiffusionImageFilter subclasses to smooth
 * the original image and then perform some sort of edge calculation based on 
 * gradients or curvature.
 * 
 * \par Description of the output of this filter
 * This filter will produce an itk::Image of unsigned long integer type and of
 * the same dimensionality as the input image.  The unsigned long output image
 * is referred to as the ``labeled image'' in this documentation.  Each pixel
 * in the image is assigned an unsigned long integer label that groups it
 * within a connected region.
 *
 * \par Some notes on filter parameters
 * Two parameters control the output of this filter, Threshold and Level.  The 
 * units of both parameters are percentage points of the maximum height value
 * in the input.  \par Threshold is used to set the absolute minimum height
 * value used during processing. Raising this threshold percentage effectively
 * decreases the number of local minima in the input, resulting in an initial
 * segmentation with fewer regions.  The assumption is that the shallow regions 
 * that thresholding removes are of of less interest.
 *
 * \par
 * The Level parameter controls the depth of metaphorical flooding of the
 * image. That is, it sets the maximum saliency value of interest in the
 * result. Raising and lowering the Level influences the number of segments in
 * the basic segmentation that are merged to produce the final output.  A level
 * of 1.0 is analogous to flooding the image up to a depth that is 100 percent
 * of the maximum value in the image.  A level of 0.0 produces the basic
 * segmentation, which will typically be very oversegmented.  Level values of
 * interest are typically low (i.e. less than about 0.40 or 40% ), since higher
 * values quickly start to undersegment the image.
 *
 * \par
 * The Level parameter can be used to create a hierarchy of output images in
 * constant time once an initial segmentation is done.  A typical scenario
 * might go like this: For the initial execution of the filter, set the Level
 * to the maximum saliency value that you anticipate might be of interest. Once 
 * the initial Update() of this process object has finished, the Level can be
 * manipulated anywhere below the initial setting without triggering a full
 * update of the segmentation mini-pipeline.  All that is now be required to
 * produce the new output is a simple relabeling of the output image.
 *
 * \par
 * Threshold and Level parameters are controlled through the class'
 * Get/SetThreshold() and Get/SetLevel() methods.
 *
 * \par Notes on streaming the watershed segmentation code
 *
 *
 *
 * 
 * \ingroup WatershedSegmentation  */
template <class TInputImage>
class ITK_EXPORT WatershedImageFilter :
    public ImageToImageFilter< TInputImage, Image<unsigned long,
    TInputImage::ImageDimension> >
{
public:
  /** Standard "Self" typedef.   */
  typedef WatershedImageFilter Self;

  /** The type of input image.   */
  typedef TInputImage InputImageType;

  /** Dimension of the input and output images. */
  enum {ImageDimension = InputImageType::ImageDimension };
  
  /** The type of output image.   */
  typedef Image<unsigned long, ImageDimension> OutputImageType;

  /** Other convenient typedefs   */
  typedef typename InputImageType::RegionType RegionType;
  typedef typename InputImageType::SizeType   SizeType;
  typedef typename InputImageType::IndexType  IndexType;
  
  /** Standard super class typedef support. */
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;

  /** Typedef support for the input image scalar value type. */
  typedef typename InputImageType::PixelType ScalarType;

  /** Smart pointer typedef support  */
  typedef SmartPointer<Self> Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(WatershedImageFilter, ImageToImageFilter);
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard process object method.  This filter is not multithreaded. */
  void GenerateData();

  /** Overloaded to link the input to this filter with the input of the
      mini-pipeline */
  void SetInput(InputImageType *input)
    {
      this->ProcessObject::SetNthInput(0, input);
      m_Segmenter->SetInputImage(input);
    }
  
  /** Set/Get the input thresholding parameter.  Units are a percentage of
   * the maximum depth in the image. */
  void SetThreshold(double);
  itkGetMacro(Threshold, double);
  
  /** Set/Get the flood level for generating the merge tree from the initial
   * segmentation   */
  void SetLevel(double);
  itkGetMacro(Level, double);
  
protected:
  WatershedImageFilter();
  virtual ~WatershedImageFilter() {}
  WatershedImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  /** A Percentage of the maximum depth (max - min pixel value) in the input
   *  image.  This percentage will be used to threshold the minimum values in
   *  the image. */ 
  double m_Threshold;

  /** The percentage of the maximum saliency value among adjacencies in the
   *  segments of the initial segmentation to which ``flooding'' of the image
   *  should occur.  A tree of segment merges is calculated up to this level*/
  double m_Level;

  /** The component parts of the segmentation algorithm.  These objects
   * must save state between calls to GenerateData() so that the
   * computationally expensive execution of segment tree generation is
   * not unneccessarily repeated. */
  typename watershed::Segmenter<InputImageType>::Pointer m_Segmenter;
  typename watershed::SegmentTreeGenerator<ScalarType>::Pointer m_TreeGenerator;
  typename watershed::Relabeler<ScalarType, ImageDimension>::Pointer m_Relabeler;

  unsigned long m_ObserverTag;
  bool m_FirstExecution;
};

/** A specialized Command object for updating the progress of a
 *  MiniPipeline.  Follows the progress of a series of filters
 *  and calls UpdateProgress on another filter (i.e. the filter
 * implementing the mini-pipeline). */
class WatershedMiniPipelineProgressCommand : public Command
{
public:
  /** Smart pointer declaration methods */
  typedef WatershedMiniPipelineProgressCommand Self;
  typedef Command Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  itkTypeMacro( WatershedMiniPipelineProgressCommand, Command );
  itkNewMacro(Self);

  /** Standard Command virtual methods */
  void Execute(Object *caller, const EventObject &event);
  void Execute(const Object *caller, const EventObject &event);

  /** Set/Get the filter whose UpdateProgress will be set by this
   * command object */
  void SetFilter( ProcessObject *p)
    { m_Filter = p; }
  const ProcessObject *GetFilter()
    { return m_Filter; }

  /** Set/Get the base count for stepping through filter progress values */
  itkSetMacro(Count, double);
  itkGetMacro(Count, double);

  /** Set/Get the number of filters that this command will expect to be
   * observing */
  itkSetMacro(NumberOfFilters, double);
  itkGetMacro(NumberOfFilters, double);
  
protected:
  WatershedMiniPipelineProgressCommand() : m_Count(0.0), m_Filter(0),
    m_NumberOfFilters(1) {}
  virtual ~WatershedMiniPipelineProgressCommand() {}
  
private:
  double m_Count;
  ProcessObject *m_Filter;
  double m_NumberOfFilters;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedImageFilter.txx"
#endif

#endif
