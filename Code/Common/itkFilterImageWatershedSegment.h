/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageWatershedSegment.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageWatershedSegment_h
#define __itkFilterImageWatershedSegment_h

#include "itkFilterImageToImage.h"
#include "itkNeighborhoodIterator.h"
#include "itkImage.h"
#include "itkDataObject.h"
#include <utility>
#include <stack>
#include <hash_map>
#include <deque>
#include <functional>

namespace itk
{
/**
 * \class FilterImageWatershedSegment
 * \brief Produces a segmented, labeled image from a scalar-valued image
 * input.  The input is assumed to represent a height function.
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
 * \sa FilterImageWatershedLevelAdaptor
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

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(WatershedSegmentBasicOutput, DataObject);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Typedef support for the input image scalar value type.
   */
  typedef typename TInputImage::ScalarValueType InputScalarType;

  /**
   * Typedef support for the output image scalar value type.
   */
  typedef typename TOutputImage::ScalarValueType OutputScalarType;
  
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
    this->Update();
  }
  
  MergeListType GetMergeList() const
  {
    return m_MergeHeirarchy;
  }
  
private:
  MergeListType m_MergeHeirarchy;
  
};

template <class TInputImage, class TOutputImage>
class ITK_EXPORT FilterImageWatershedLevelAdaptor;

template <class TInputImage, class TOutputImage>
class ITK_EXPORT FilterImageWatershedSegment :
    public FilterImageToImage< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageWatershedSegment Self;

  /**
   * Standard super class typedef support.
   */
  typedef FilterImageToImage< TInputImage, TOutputImage > Superclass;

  /**
   * Dimension of the input image.  Output image dimension must be the same
   * but cannot be enforced by templating over the image type.
   */
  enum {ImageDimension = TInputImage::ImageDimension };
  
  /**
   * Typedef support for the input image scalar value type.
   */
  typedef typename TInputImage::ScalarValueType InputScalarType;

  /**
   * Typedef support for the output image scalar value type.
   */
  typedef typename TOutputImage::ScalarValueType OutputScalarType;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FilterImageWatershedSegment, FilterImageToImage);
  
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
  typename WatershedSegmentBasicOutput<TInputImage, TOutputImage>::Pointer
  GetBasicOutput()
  {
    return static_cast<WatershedSegmentBasicOutput<TInputImage, TOutputImage>*
      >(this->ProcessObject::GetOutput(1).GetPointer());
  }

  void SetBasicOutput(WatershedSegmentBasicOutput<TInputImage, TOutputImage>
                      *output)
  {
    this->ProcessObject::SetNthOutput(1, output);
  }


  virtual void Update()
  {
    if (this->GetOutput(0))
      {
        this->GetOutput(0)->Update();
      }
  }
  
protected:
  FilterImageWatershedSegment() :  m_Level(0.0f), m_Threshold(0.0f)
  {
    WatershedSegmentBasicOutput<TInputImage, TOutputImage>::Pointer
      output = WatershedSegmentBasicOutput<TInputImage, TOutputImage>::New();
    output->SetSource(this);
    this->ProcessObject::SetNumberOfOutputs(2);
    this->ProcessObject::SetNthOutput(1,output.GetPointer());
  }
  virtual ~FilterImageWatershedSegment() {}
  FilterImageWatershedSegment(const Self&) {}
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
  class strhash : public std::hash<int>
  {
  public:
    ::size_t operator()(const OutputScalarType &s) const
    {
      return std::hash<int>::operator()(static_cast<int>(s));
    }
  };

  /**
   * Table storing information about edges between segments.
   */
  typedef std::hash_map<OutputScalarType, InputScalarType, strhash>
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
  typedef typename WatershedSegmentBasicOutput<TInputImage, TOutputImage>
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
  typedef std::hash_map<OutputScalarType, SegmentType, strhash>
  SegmentTableType;

  /**
   * Table for storing flat region information.
   */
  typedef std::hash_map<OutputScalarType, FlatRegion, strhash>
  FlatRegionTableType;

  /**
   * Table for storing correspondences between segment and region labels.
   */
  typedef std::hash_map<OutputScalarType, OutputScalarType, strhash>
  LabelTableType;

  /**
   * List for storing sequences of segment merges.
   */
  typedef typename WatershedSegmentBasicOutput<TInputImage, TOutputImage>::MergeListType
  MergeListType;
  
  /**
   * Creates the basic segmentation from a thresholded copy of the input
   * image. First labels the single pixel minima and the flat regions.  Next
   * traces all remaining, unlabeled pixels to their local minima.  Finally,
   * the plateau flat regions (those that are not themselves a local minima or
   * maxima), are connected with the basin adjacent to their lowest boundary
   * pixel (pixel just outside the flat region with the smallest value).
   */
  static void CreateBasicSegmentation2D(TInputImage *, TOutputImage *, const
                          OutputScalarType);

  /**
   * Part of the basic segmentation.  Finds and labels all single pixel minima
   * and all flat regions (groups of pixels with at least one neighbor that has 
   * the same value).
   */
  static void LabelSPMandFlatPixels(TInputImage *, TOutputImage *, const
                    OutputScalarType, FlatRegionTableType &);
  
  /**
   * Part of the basic segmentation.  Traces all unlabeled pixels to a local
   * minimum and associates all pixels along the path to that local minimum
   * with the label of the local minimum.
   */
  static void TraceUnlabeledPixels2D( TInputImage *, TOutputImage *,
                                      const OutputScalarType);

  /**
   * Part of the basic segmentation. Connects all plateau regions (pixels of
   * equal value that are less than some and greater than others of the pixels
   * bordering the region) with a basin by associating each plateau with the
   * label of its smallest-valued neighbor pixel.
   */
  static void ConnectPlateausWithBasins( TInputImage *, TOutputImage *,
                                         const FlatRegionTableType &);

  /**
   * Finds the minimum value and the maximum value in an image.
   */
  static void FindMinMax(TInputImage *,
                         InputScalarType &, InputScalarType &);

  /**
   * Copies one image into another, raising any value smaller than the
   * threshold value to that of the threshold value.
   */
  static void MinimumThresholdImage(const InputScalarType,
                                    TInputImage *, TInputImage *);

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
  static void RelabelImage(TOutputImage *, const LabelTableType &);

  /**
   * Creates a table of information about each segment's minimum value,
   * neighboring segments, and edge information between each neighboring
   * segment.
   */
  static void CreateSegmentTable(SegmentTableType &, TInputImage *,
                                 TOutputImage *, const OutputScalarType);

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
    while (it->second.MergedToLabel != UNLABELED_PIXEL)
        { 
          it = segments.find(it->second.MergedToLabel);
        }
    return it->first;
  }

  /**
   * Compiles and resolves a list of segment label equivalencies with which to
   * relabel an output image.
   */
  static LabelTableType ExtractEquivalencyTable(const MergeListType &, const
                                                InputScalarType &);


  static void CopyOutputToOutput(TOutputImage *,TOutputImage *);
  
private:
  friend class FilterImageWatershedLevelAdaptor<TInputImage,TOutputImage>;
  
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
   * The percentage of the maximum pixle value in the input image to which
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
#include "itkFilterImageWatershedSegment.txx"
#endif

#endif
