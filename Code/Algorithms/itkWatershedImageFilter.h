/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
#include "vcl_hash_map.h"
#include <deque>
#include <functional>

namespace itk
{

/**
 * \class WatershedSegmentBasicOutput
 * \brief Structure that holds a segmentation produced by the
 * WatershedImageFilter filter object.
 *
 * NOTE: The outputs of itkWatershedImageFilter are being restructured
 *       to eliminate this class.  Currently there is a bug in
 *       itkWatershedImageFilter which creates infinite looping when
 *       invoked in the Insight pipeline.  Restructuring the outputs
 *       will eliminate this bug. -cates 3/27/01
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
  typedef typename InputImageType::ScalarValueType InputScalarType;

  /**
   * Typedef support for the output image scalar value type.
   */
  typedef typename OutputImageType::ScalarValueType OutputScalarType;
  
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
    //this->Update();
  }
  
  MergeListType GetMergeList() const
  {    return m_MergeHeirarchy;  }
  
private:
  MergeListType m_MergeHeirarchy;
  
};

template <class TInputImage, class TOutputImage>
class ITK_EXPORT RelabelWatershedImageFilter;


/**
 * \class WatershedImageFilter
 * \brief Produces a segmented, labeled image from a scalar-valued image
 * input.  The input is assumed to represent a height function.
 *
 * BUG:  The outputs of itkWatershedImageFilter are being restructured
 *    .  Currently there is a bug in itkWatershedImageFilter which creates
 *       infinite looping when invoked in the Insight pipeline.  Restructuring
 *       the outputs will eliminate this bug. -cates 3/27/01
 *
 * Two parameters control the output of the filter, Threshold and Level.
 * The units of both parameters are percentage points of the maximum height
 * value in the input.  Threshold is used to set the absolute minimum height
 * value used during processing. Raising this threshold percentage effectively
 * decreases the  number of local minima in the input. Level controls the depth
 * of flooding of the image.  Raising the Level influences the number of segments
 * in the basic segmentation that merge to produce the final output.  A level
 * of 1.0 is analogous to flooding the image up to a depth that is 100 percent
 * of the maximum value in the image.  A level of 0.0 produces the basic
 * segmentation. 
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
  typedef typename InputImageType::ScalarValueType InputScalarType;

  /**
   * Typedef support for the output image scalar value type.
   */
  typedef typename OutputImageType::ScalarValueType OutputScalarType;
  
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
  WatershedImageFilter() :  m_Level(0.0f), m_Threshold(0.0f)
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
  class strhash : public vcl_hash<int>
  {
  public:
    ::size_t operator()(const OutputScalarType &s) const
    {
      return vcl_hash<int>::operator()(static_cast<int>(s));
    }
  };

  /**
   * Table storing information about edges between segments.
   */
  typedef vcl_hash_map<OutputScalarType, InputScalarType, strhash>
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
  typedef vcl_hash_map<OutputScalarType, SegmentType, strhash>
  SegmentTableType;

  /**
   * Table for storing flat region information.
   */
  typedef vcl_hash_map<OutputScalarType, FlatRegion, strhash>
  FlatRegionTableType;

  /**
   * Table for storing correspondences between segment and region labels.
   */
  typedef vcl_hash_map<OutputScalarType, OutputScalarType, strhash>
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
