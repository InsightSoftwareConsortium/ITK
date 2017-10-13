/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkWatershedSegmenter_h
#define itkWatershedSegmenter_h


#include "itkWatershedBoundary.h"
#include "itkWatershedSegmentTable.h"
#include "itkEquivalencyTable.h"

namespace itk
{
namespace watershed
{
/** \class Segmenter
 *
 * This filter implements the first step in the N-d watershed segmentation
 * algorithm.  It produces a segmented, labeled image from a scalar-valued
 * image input. This filter is used in conjunction with
 * WatershedSegmentTreeGenerator and WatershedRelabeler to produce a final
 * segmented image volume. See itk::WatershedImageFilter for an overview of the
 * entire algorithm and notes on the terminology used in describing it.
 *
 * \par
 * The filter is designed to operate in streaming or non-streaming mode.  For
 * more information, see the itk::WatershedImageFilter documentation.
 *
 * \par Input
 * There is one input to this algorithm, a real-valued (scalar) itk::Image of
 * arbitrary dimension.  The input is assumed to represents a height function,
 * such as a gradient magnitude edge image.  The filter can process an image of
 * any dimension. Note that the terms "pixel'' and ``voxel" are
 * interchangeable in this and other watershed component class documentation.
 *
 * \par Outputs
 * There are three potential outputs of this algorithm described below.
 *
 * \par
 * The first output is a labeled image of IdentifierType integers.  This is an
 * initial segmentation and labeling that is fed into successive components of
 * the watershed algorithm.
 *
 * \par
 * The second output is a table of segment information,
 * itk::watershed::SegmentTable. This table is a record of each segment
 * numbered in the initial segmentation (output number one) with relevant
 * information needed in successive stages of the algorithm.
 *
 * \par
 * The third output is a data structure containing boundary pixel information,
 * itk::watershed::Boundary.  This data is only generated if the flag
 * DoBoundaryAnalysis is set to true and is only useful in streaming
 * applications.
 *
 * \par Parameters
 * Threshold is specified as a percentage (0.0 - 1.0) of the maximum height of
 * the image. This filter thresholds the input image to remove all values below
 * \f$ L = min + T * (max - min) \f$, where \f$ max, min \f$ are the maximum,
 * minimum values in the image and \f$ T \f$ is the threshold parameter
 * value. Values in the image less than \f$ L \f$ are raised to \f$ L \f$.
 *
 * \par
 * Thresholding minimum values in the image decreases the number of local
 * minima in the image and produces an initial segmentation with fewer
 * segments.  The assumption is that the "shallow" regions that this
 * thresholding eliminates are generally not of interest.
 *
 * \sa WatershedImageFilter
 * \ingroup WatershedSegmentation
 * \ingroup ITKWatersheds
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT Segmenter:
  public ProcessObject
{
public:
  /** Standard self typedefs */
  typedef Segmenter Self;

  /** Define image types and dimensionality  */
  typedef TInputImage InputImageType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef Image< IdentifierType, itkGetStaticConstMacro(ImageDimension) >
  OutputImageType;
  typedef typename InputImageType::RegionType ImageRegionType;
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef Boundary< InputPixelType, itkGetStaticConstMacro(ImageDimension) >
  BoundaryType;
  typedef typename BoundaryType::IndexType         BoundaryIndexType;
  typedef typename BoundaryType::FlatHashValueType BoundaryFlatHashValueType;
  typedef SegmentTable< InputPixelType >           SegmentTableType;
  typedef DataObject::Pointer                      DataObjectPointer;

  /** Methods to implement smart pointers and work with the itk object factory
   */
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(WatershedSegmenter, ProcessObject);

  /** Typedefs necessary on microsoft VC++ to avoid internal compiler errors */
  typedef typename InputImageType::Pointer   InputImageTypePointer;
  typedef typename OutputImageType::Pointer  OutputImageTypePointer;
  typedef typename SegmentTableType::Pointer SegmentTableTypePointer;
  typedef typename BoundaryType::Pointer     BoundaryTypePointer;

  /** A constant used in the labeling algorithm.  */
  itkStaticConstMacro(NULL_LABEL, IdentifierType, 0);

  /** A constant used in the labeling algorithm.  */
  itkStaticConstMacro(NULL_FLOW, short, -1);

  /** Get/Set the input image.   */
  InputImageType * GetInputImage(void)
  {
    return itkDynamicCastInDebugMode< InputImageType * >
           ( this->ProcessObject::GetInput(0) );
  }

  void SetInputImage(InputImageType *img)
  {  this->ProcessObject::SetNthInput(0, img); }

  /** Get/Set the labeled output image.  The output image is always of
    IdentifierType integers. */
  OutputImageType * GetOutputImage(void)
  {
    return itkDynamicCastInDebugMode< OutputImageType * >
           ( this->ProcessObject::GetOutput(0) );
  }

  void SetOutputImage(OutputImageType *img)
  { this->ProcessObject::SetNthOutput(0, img);    }

  /** Get/Set the segment table.  The segment table is a table of segmentation
   * information identifying each region produced by the labeling algorithm. */
  SegmentTableType * GetSegmentTable(void)
  {
    return itkDynamicCastInDebugMode< SegmentTableType * >
           ( this->ProcessObject::GetOutput(1) );
  }

  void SetSegmentTable(SegmentTableType *s)
  { this->ProcessObject::SetNthOutput(1, s); }

  /** Returns the boundary information data necessary only for data streaming
    applications.  */
  BoundaryType * GetBoundary(void)
  {
    return itkDynamicCastInDebugMode< BoundaryType * >
           ( this->ProcessObject::GetOutput(2) );
  }

  void SetBoundary(BoundaryType *b)
  { this->ProcessObject::SetNthOutput(2, b); }

  /** Standard non-threaded pipeline execution method. */
  virtual void GenerateData() ITK_OVERRIDE;

  /** This method is necessary until the streaming mechanisms of the Itk
   * pipeline are full fleshed out.  It is only used for streaming
   * applications.  Calling this method gets/sets the image size of the
   * complete volume being streamed.  The member variables controlled by
   * this method will not be modified by the Itk pipeline and are necessary
   * for analysis of boundaries.   */
  void SetLargestPossibleRegion(ImageRegionType reg)
  {
    if ( reg == m_LargestPossibleRegion ) { return; }
    m_LargestPossibleRegion = reg;
    this->Modified();
  }

  ImageRegionType GetLargestPossibleRegion() const
  { return m_LargestPossibleRegion; }

  /** Helper function.  Other classes may have occasion to use this. Relabels
      an image according to a table of equivalencies. */
  static void RelabelImage(OutputImageTypePointer,
                           ImageRegionType,
                           EquivalencyTable::Pointer);

  /** Standard itk::ProcessObject subclass method. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  /** Gets/Sets the initial label (IdentifierType integer value) used
   * by the labeling algorithm.  Only necessary for streaming applications. */
  itkSetMacro(CurrentLabel, IdentifierType);
  itkGetConstMacro(CurrentLabel, IdentifierType);

  /** Gets/Sets the input threshold. Threshold is specified as a percentage
   * (0.0 - 1.0) of the maximum height of the image. This filter thresholds the
   * input image to remove all values below \f$ L = min + T * (max -  min) \f$,
   * where \f$ max, min \f$ are the maximum, minimum values in the image and \f$
   * T \f$ is the threshold parameter value. Values in the image less than \f$ L
   * \f$ are raised to \f$ L \f$. Thresholding minimum values in the image
   * decreases the number of local minima in the image and produces an initial
   * segmentation with fewer segments.  The assumption is that the "shallow"
   * regions that this thresholding eliminates are generally not of
   * interest. */
  itkSetClampMacro(Threshold, double, 0.0, 1.0);
  itkGetConstMacro(Threshold, double);

  /** Turns on special labeling of the boundaries for streaming applications.
   * The default value is FALSE, meaning that boundary analysis is turned
   * off.   */
  itkSetMacro(DoBoundaryAnalysis, bool);
  itkGetConstMacro(DoBoundaryAnalysis, bool);

  /** Determines whether the algorithm will sort the adjacencies in its
   * SegmentTable before returning.  Default is true.  This is an option only
   * useful for streaming applications where the sorting only needs to be done
   * after all iterations have taken place. */
  itkGetConstMacro(SortEdgeLists, bool);
  itkSetMacro(SortEdgeLists, bool);

protected:
  /** Structure storing information about image flat regions.
   * Flat regions are connected pixels of the same value.  */
  struct flat_region_t {
    IdentifierType *min_label_ptr;
    InputPixelType bounds_min;
    //    InputPixelType  bounds_max; // <-- may not be necc.
    InputPixelType value;
    bool is_on_boundary;
    flat_region_t():is_on_boundary(false) {}
  };

  /** Table for storing flat region information.  */
  typedef itksys::hash_map< IdentifierType, flat_region_t, itksys::hash< IdentifierType > >
  flat_region_table_t;

  struct connectivity_t {
    unsigned int size;
    unsigned int *index;
    typename InputImageType::OffsetType * direction;
  };

  /** Table for storing tables of edges.  This is convenient in
   * generating the segment table,  even though the edge tables
   * are stored as ordered lists.  An "edge" in this context
   * is synonymous with a segment "adjacency".   */
  typedef itksys::hash_map< IdentifierType, InputPixelType, itksys::hash< IdentifierType >
                            > edge_table_t;

  typedef itksys::hash_map< IdentifierType, edge_table_t, itksys::hash< IdentifierType >
                         > edge_table_hash_t;

  Segmenter();
  Segmenter(const Self &) {}
  virtual ~Segmenter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void operator=(const Self &) {}

  /** Constructs the connectivity list and the corresponding set of directional
   * Offset indices. */
  virtual void GenerateConnectivity();

  /** This method asks for an image region that is one pixel larger
   * at each boundary than the region being processed.  This single pixel
   * expansion represents an overlap with adjacent image chunks   */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void GenerateOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  virtual void UpdateOutputInformation() ITK_OVERRIDE;

  /**  Allocates boundary structure information and sets the
   * boundary data to null values.   */
  void InitializeBoundary();

  /** Performs a gradient descent connected component analysis
   * at the boundaries of the images that border other
   * image chunks.  Useful only in data streaming applications.   */
  void AnalyzeBoundaryFlow(InputImageTypePointer,
                           flat_region_table_t &,
                           InputPixelType);

  /** Fills boundary pixels with a specified value.  Used by labeling
   * methods to build a very high "wall" around the image so that
   * gradient descent does not need to watch boundaries.   */
  void BuildRetainingWall(InputImageTypePointer,
                          ImageRegionType, InputPixelType);

  /** Labels all the local minima in the image.  Also identifies and labels
   * connected  "flat" regions.   */
  void LabelMinima(InputImageTypePointer,
                   ImageRegionType, flat_region_table_t &,
                   InputPixelType);

  /** Follows each unlabeled pixel in the image down its path of steepest
   * descent.  Each pixel along that path is identified with the local minima
   * already labeled at the end of the path.   */
  void GradientDescent(InputImageTypePointer, ImageRegionType);

  /** Associates each flat region with a local minimum and relabels
    accordingly.  */
  void DescendFlatRegions(flat_region_table_t &, ImageRegionType);

  /** Adds entries to the output segment table for all labeled segments in the
   * image.  */
  void UpdateSegmentTable(InputImageTypePointer, ImageRegionType);

  /** Traverses each boundary and fills in the data needed for joining
   * streamed chunks of an image volume.  Only necessary for streaming
   * applications.   */
  void CollectBoundaryInformation(flat_region_table_t &);

  /** Helper function.  Thresholds low values and copies values from one image
   * into another. The source and destination regions must match in size (not
   * enforced).  For integral types, the dynamic range of the image is
   * adjusted such that the maximum value in the image is always at
   * least one less than the maximum value allowed for that data type. */
  static void Threshold(InputImageTypePointer destination,
                        InputImageTypePointer source,
                        const ImageRegionType source_region,
                        const ImageRegionType destination_region,
                        InputPixelType threshold);

  /** Helper function.  Finds the minimum and maximum values in an image. */
  static void MinMax(InputImageTypePointer img,
                     ImageRegionType region,
                     InputPixelType & min,
                     InputPixelType & max);

  /** Helper function. Finds the minimum and maximum values in an image.   */
  static void MergeFlatRegions(flat_region_table_t &, EquivalencyTable::Pointer);

  /** Helper functions for filling in regions with values   */
  static void SetInputImageValues(InputImageTypePointer img,
                                  const ImageRegionType region,
                                  InputPixelType value);

  static void SetOutputImageValues(OutputImageTypePointer img,
                                   const ImageRegionType region,
                                   IdentifierType value);

  /** This is a debugging method.  Will be removed. 11/14/01 jc   */
  //  bool CheckLabeledBoundaries();

  /** Holds generalized connectivity information for connected component
   * labeling and gradient descent analysis in pixel neighborhoods.  */
  connectivity_t m_Connectivity;

private:
  /** Helper, debug method.   */
  //  void PrintFlatRegions(flat_region_table_t &t);

  /** This is the actual data set size.  The pipeline will alter its
   *  LargestPossibleRegion, so we need to preserve it here explicitly for
   *  streaming applications*/
  ImageRegionType m_LargestPossibleRegion;

  bool            m_SortEdgeLists;
  bool            m_DoBoundaryAnalysis;
  double          m_Threshold;
  double          m_MaximumFloodLevel;
  IdentifierType  m_CurrentLabel;
};
} // end namespace watershed
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedSegmenter.hxx"
#endif

#endif
