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
#include "itkNeighborhoodIterator.h"
#include "itkImage.h"
#include "itkDataObject.h"
#include <utility>
#include <stack>
#include <algorithm>
#include "itk_hash_map.h"
#include <deque>
#include <functional>

namespace itk
{
/**
 * \class WatershedSegmentBasicOutput
 * \brief Structure that holds a segmentation produced by the
 * WatershedImageFilter filter object.
 *
 * Consists of a labeled image and a heirarchy of merges
 * of labeled segments.  This structure encapsulates all the information needed
 * to examine the segmented image at any arbitrary level.
 *
 * The pixels must support the operators <, ==
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT WatershedSegmentBasicOutput : public TOutputImage
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef WatershedSegmentBasicOutput Self;

  /**
   * Standard super class typedef support.
   */
  typedef DataObject Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(WatershedSegmentBasicOutput, DataObject);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * The type of input image.
   */
  typedef TInputImage InputImageType;
  
  /**
   * The type of output image.
   */
  typedef TOutputImage OutputImageType;
  
  /**
   * Typedef support for the input image scalar value type.
   */
  typedef typename InputImageType::PixelType InputScalarType;

  /**
   * Typedef support for the output image scalar value type.
   */
  typedef typename OutputImageType::PixelType OutputScalarType;
  
  /**
   * Structure for storing merge information.
   */
  typedef struct
  {
    OutputScalarType FromLabel;
    OutputScalarType ToLabel;
    InputScalarType Saliency;
  } MergeType;

  /**
   * List for storing sequences of segment merges.
   */
  typedef std::deque<MergeType> MergeListType;
  
  /**
   * Standard get, set methods for member variables.
   */
  void SetMergeList(const MergeListType &m)
  {
    m_MergeHeirarchy = m;
    this->Modified();
  }
  
  MergeListType GetMergeList() const
  {    return m_MergeHeirarchy;  }

  itkSetMacro(MaxDepth, InputScalarType);
  itkGetMacro(MaxDepth, InputScalarType);
  
private:
  MergeListType m_MergeHeirarchy;
  InputScalarType m_MaxDepth;
  
};

template <class TInputImage, class TOutputImage>
class ITK_EXPORT RelabelWatershedImageFilter;


/**
 * \class WatershedImageFilter
 * \brief Produces a segmented, labeled image from a scalar-valued image
 * input.  The input is assumed to represent a height function.
 *
 * Watershed segmentation gets its name from the manner in which the algorithm
 * segments regions into catchment basins. If a function \f$ f \f$ is a continuous
 * height function defined over an image domain, then a catchment basin is
 * defined as the set of points whose paths of steepest descent terminate at
 * the same local minimum of \f$ f \f$.
 *
 * The choice of height function depends on the application and the basic
 * watershed algorithm is independent of the choice of height function. For
 * intensity-based image data, you might typically use some sort of gradient
 * magnitude calculation for the input to the algorithm
 *
 * The watershed algorithm proceeds in several steps. An initial classification
 * of all points into catchment basin regions is first done by tracing points
 * down to their local minimum, following the path of steepest descent. Next,
 * an analysis of neighboring basins and the height of their common boundary
 * points produces a tree of merges between those basins that occur at
 * different flood level thresholds (henceforth referred to as the merge
 * tree). Flood level is a value that reflects the amount of metaphorical
 * precipitation that is rained into the catchment basins. It's minimum value
 * is zero and its maximum value is the difference between the highest and
 * lowest values in the input image (height function).
 *
 * Note that once the initial analysis and segmentation is done, it is fairly
 * trivial to produce a labeled image that corresponds to any level in the
 * merge tree. All you have to do is relabel the initial segmentation according
 * to all the merges that have taken place. This filter, WatershedImageFilter,
 * creates the initial segmentation and the merge tree.  It can be set to
 * output a labeled image at a particular flood level threshold, and/or its
 * output can be coupled to the input of RelabelWatershedImageFilter, which
 * produces a labeled image at any flood level threshold simply by relabeling
 * the initial segmentation.
 *
 * Two parameters control the output of this filter, Threshold and Level.  The
 * units of both parameters are percentage points of the maximum height value
 * in the input.  Threshold is used to set the absolute minimum height value
 * used during processing. Raising this threshold percentage effectively
 * decreases the number of local minima in the input. Level controls the depth
 * of flooding of the image.  Raising the Level influences the number of
 * segments in the basic segmentation that merge to produce the final output.
 * A level of 1.0 is analogous to flooding the image up to a depth that is 100
 * percent of the maximum value in the image.  A level of 0.0 produces the
 * basic segmentation.
 *
 *
 * \todo Rework to accommodate streaming and add support for N-dimensional
 * images.
 * \todo Example code for the repository.
 * \todo References to the documentation.
 *
 * \sa RelabelWatershedImageFilter
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT WatershedImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef WatershedImageFilter Self;

  /**
   * The type of input image.
   */
  typedef TInputImage InputImageType;
  
  /**
   * The type of output image.
   */
  typedef TOutputImage OutputImageType;

  /**
   * Some common typedefs
   */
  typedef typename InputImageType::RegionType RegionType;
  typedef typename InputImageType::SizeType   SizeType;
  typedef typename InputImageType::IndexType  IndexType;

  /**
   * Standard super class typedef support.
   */
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;

  /**
   * Dimension of the input image.  Output image dimension must be the same
   * but cannot be enforced by templating over the image type.
   */
  enum {ImageDimension = InputImageType::ImageDimension };
  
  /**
   * Typedef support for the input image scalar value type.
   */
  typedef typename InputImageType::PixelType InputScalarType;

  /**
   * Typedef support for the output image scalar value type.
   */
  typedef typename OutputImageType::PixelType OutputScalarType;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(WatershedImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard process object method.
   */
  void GenerateData();

  /**
   * Standard set and get methods for filter parameters.
   */
  itkSetClampMacro(Threshold, float, 0.0f, 1.0f);
  itkGetMacro(Threshold, float);

  /**
   * Standard set and get methods for filter parameters.
   */
  itkSetClampMacro(Level, float, 0.0f, 1.0f);
  itkGetMacro(Level, float);

  /**
   * Set and get basic output methods.
   */
  typename WatershedSegmentBasicOutput<InputImageType, OutputImageType>::Pointer
  GetBasicOutput()
  {
    return static_cast<WatershedSegmentBasicOutput<InputImageType,
      OutputImageType>* >(this->ProcessObject::GetOutput(1).GetPointer());
  }

  void SetBasicOutput(WatershedSegmentBasicOutput<InputImageType,
                      OutputImageType>  *output)
  {    this->ProcessObject::SetNthOutput(1, output);  }

protected:
  WatershedImageFilter() :  m_Threshold(0.0f), m_Level(0.0f)
  {
    WatershedSegmentBasicOutput<InputImageType, OutputImageType>::Pointer
      output = WatershedSegmentBasicOutput<InputImageType, OutputImageType>::New();
    this->ProcessObject::SetNumberOfRequiredOutputs(2);
    this->ProcessObject::SetNthOutput(1,output.GetPointer());
  }
  virtual ~WatershedImageFilter() {}
  WatershedImageFilter(const Self&) {}
  void operator=(const Self&) {}

  /**
   * Convenience type for storing pairs of segment labels.
   */
  typedef std::pair<OutputScalarType, OutputScalarType> LabelPairType;

  /**
   * Structure storing information about image flat regions.
   */
  typedef struct
  {
    OutputScalarType*     BoundaryMinLabelPointer;
    InputScalarType       BoundaryMinValue;
    InputScalarType       BoundaryMaxValue;
    InputScalarType       RegionValue;
  } FlatRegion;

  /**
   * Hashing function for hash tables with OutputScalarType keys.
   */
  class strhash : public itk::hash<int>
  {
    typedef itk::hash<int> Superclass;
  public:
    ::size_t operator()(const OutputScalarType &s) const
    {
      return Superclass::operator()(static_cast<int>(s));
    }
  };

  /**
   * Table storing information about edges between segments.
   */
  typedef itk::hash_map<OutputScalarType, InputScalarType, strhash>
  EdgeTableType; 

  /**
   * Structure storing information about an image segment.
   */
  typedef struct
  {
    InputScalarType MinimumEdgeValue;
    InputScalarType Minimum;
    InputScalarType Depth;
    EdgeTableType EdgeTable;
    OutputScalarType MergedToLabel;
  } SegmentType;

  /**
   * Structure storing information about a segment merge.
   */
  typedef typename WatershedSegmentBasicOutput<InputImageType, OutputImageType>
  ::MergeType MergeType;


  /**
   * Boolean comparison functor for use in sorting functions.
   */
  struct merge_comp : public std::binary_function<bool, const MergeType&,
                      const MergeType& >
  {
    bool operator()(const MergeType &a, const MergeType &b)
    {
      return b.Saliency < a.Saliency;
    }
  };

  /**
   * Boolean comparison functor for use in sorting functions.
   */
  struct sort_comp : public std::binary_function<bool, const MergeType&,
                      const MergeType& >
  {
    bool operator()(const MergeType &a, const MergeType &b)
    {
      return a.Saliency < b.Saliency;
    }
  };

  /**
   * Table for storing segment information.
   */
  typedef itk::hash_map<OutputScalarType, SegmentType, strhash>
  SegmentTableType;

  /**
   * Table for storing flat region information.
   */
  typedef itk::hash_map<OutputScalarType, FlatRegion, strhash>
  FlatRegionTableType;

  /**
   * Table for storing correspondences between segment and region labels.
   */
  typedef itk::hash_map<OutputScalarType, OutputScalarType, strhash>
  LabelTableType;

  /**
   * List for storing sequences of segment merges.
   */
  typedef typename WatershedSegmentBasicOutput<InputImageType, OutputImageType>
  ::MergeListType  MergeListType;
  
  /**
   * Creates the basic segmentation from a thresholded copy of the input
   * image. First labels the single pixel minima and the flat regions.  Next
   * traces all remaining, unlabeled pixels to their local minima.  Finally,
   * the plateau flat regions (those that are not themselves a local minima or
   * maxima), are connected with the basin adjacent to their lowest boundary
   * pixel (pixel just outside the flat region with the smallest value).
   */
  static void CreateBasicSegmentation2D(InputImageType *, OutputImageType *, const
                          OutputScalarType);


  /**
   *
   */
  static void FillBorderPixels(InputImageType *, const RegionType &,
                               OutputScalarType);

  
  /**
   * Part of the basic segmentation.  Finds and labels all single pixel minima
   * and all flat regions (groups of pixels with at least one neighbor that has 
   * the same value).
   */
  static void LabelSPMandFlatPixels(InputImageType *, OutputImageType *, const
                    OutputScalarType, FlatRegionTableType &);
  
  /**
   * Part of the basic segmentation.  Traces all unlabeled pixels to a local
   * minimum and associates all pixels along the path to that local minimum
   * with the label of the local minimum.
   */
  static void TraceUnlabeledPixels2D( InputImageType *, OutputImageType *,
                                      const OutputScalarType);

  /**
   * Part of the basic segmentation. Connects all plateau regions (pixels of
   * equal value that are less than some and greater than others of the pixels
   * bordering the region) with a basin by associating each plateau with the
   * label of its smallest-valued neighbor pixel.
   */
  static void ConnectPlateausWithBasins( InputImageType *, OutputImageType *,
                                         const FlatRegionTableType &);

  /**
   * Finds the minimum value and the maximum value in an image.
   */
  static void FindMinMax(InputImageType *,
                         InputScalarType &, InputScalarType &);

  /**
   * Copies one image into another, raising any value smaller than the
   * threshold value to that of the threshold value.
   */
  static void MinimumThresholdImage(const InputScalarType,
                                    InputImageType *, InputImageType *);

  /**
   * Resolves dependencies among label equivalencies.  Returns a hash table
   * with unique keys in which each label is equated to a label that is itself
   * not contained in the table.
   */
  static LabelTableType MergeLabelPairs( std::stack<LabelPairType>);

  /**
   * Merges flat regions that have been identified as equivalent.
   */
  static void MergeFlatRegions(const LabelTableType &, FlatRegionTableType &);

  /**
   * Relabels the regions in an image by replacing labels according to a lookup 
   * table of (unique) equivalencies.
   */
  static void RelabelImage(OutputImageType *, const LabelTableType &);

  /**
   * Creates a table of information about each segment's minimum value,
   * neighboring segments, and edge information between each neighboring
   * segment.
   */
  static void CreateSegmentTable(SegmentTableType &, InputImageType *,
                                 OutputImageType *, const OutputScalarType);

  /**
   * Debugging code.
   */ 
  static void PrintSegmentTable(const SegmentTableType &);

  /**
   * Debugging code.
   */
  static void PrintMergeHeap(const MergeListType &);
  
  /**
   * Creates a heap of merge objects from a segment information table.  A merge 
   * object is a record of an edge between two neighboring segments and the
   * threshold flood level at which those two segments will merge.
   */
  static void CreateMergeHeap(const SegmentTableType &, MergeListType &);

  /**
   * Extracts a list of segment merges from a merge heap.  The merges are
   * tagged and ordered by the floodlevel at which they will occur. Merge
   * information is actually deleted from the heap as it is placed in the list.
   * In this way, the state of the merging process is saved between calls on a
   * particular heap.
   */
  static void ExtractMergeHeirarchy(SegmentTableType &, MergeListType &,
                                    MergeListType &, const InputScalarType,
                                    const OutputScalarType); 

  /**
   * Recursively identifies the last segment with which a segment has merged.
   */
  static OutputScalarType ResolveLabel(const SegmentTableType &segments,
                                       const OutputScalarType &label,
                                       const OutputScalarType &UNLABELED_PIXEL)
  {
    typename SegmentTableType::const_iterator it;
    it = segments.find(label);
    while ((*it).second.MergedToLabel != UNLABELED_PIXEL)
        { 
          it = segments.find((*it).second.MergedToLabel);
        }
    return ( (*it).first );
  }

  /**
   * Compiles and resolves a list of segment label equivalencies with which to
   * relabel an output image.
   */
  static LabelTableType ExtractEquivalencyTable(const MergeListType &, const
                                                InputScalarType &);


  static void CopyOutputToOutput(OutputImageType *,OutputImageType *);
  
private:
  friend class RelabelWatershedImageFilter<InputImageType,OutputImageType>;
  
  /**
   * A Percentage of the maximum pixel value in the input image.  This
   * percentage will be used to threshold the minimum values in the image.
   */
  float m_Threshold;

  /**
   * Internal data structure used to keep track of possible basic segment
   * merges and the flood levels at which they occur. 
   */
  MergeListType m_MergeHeap;

  /**
   * Internal data structure used to record all of the basic segmentations
   * possible merges and the flood level thresholds at which they occur.
   */
  MergeListType m_MergeHeirarchy;
  
  /**
   * The percentage of the maximum pixel value in the input image to which
   * flooding of the image will occur.
   */
  float m_Level;
  
  /**
   * A record of segment information from the basic segmentation of the image.
   */
  SegmentTableType m_BaseSegmentTable;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedImageFilter.txx"
#endif

#endif
