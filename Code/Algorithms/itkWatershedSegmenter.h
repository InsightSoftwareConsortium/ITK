/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmenter.h
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
#ifndef __itkWatershedSegmenter_h
#define __itkWatershedSegmenter_h

#include "itk_hash_map.h"
#include "itkWatershedBoundary.h"
#include "itkWatershedSegmentTable.h"
#include "itkWatershedEquivalencyTable.h"
#include "itkImage.h"

namespace itk
{
namespace watershed
{
/** \class WatershedSegmenter
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
 * any dimension. Note that the terms ``pixel'' and ``voxel'' are
 * interchangeable in this and other watershed component class documentation.
 *
 * \par Outputs
 * There are three potential outputs of this algorithm described below.
 *
 * \par 
 * The first output is a labeled image of unsigned long integers.  This is an
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
 * /f$ L = min + T * (max - min) /f$, where /f$ max, min /f$ are the maximum,
 * minimum values in the image and /f$ T /f$ is the threshold parameter
 * value. Values in the image less than /f$ L /f$ are raised to /f$ L /f$.
 *
 * \par
 * Thresholding minimum values in the image decreases the number of local
 * minima in the image and produces an initial segmentation with fewer
 * segments.  The assumption is that the ``shallow'' regions that this
 * thresholding eliminates are generally not of interest.
 * 
 * \sa WatershedImageFilter
 * \ingroup WatershedSegmentation  */
template <class TInputImage>
class Segmenter
  : public ProcessObject
{
public:
  /** Define image types and dimensionality  */
  typedef TInputImage InputImageType;
  enum { ImageDimension = InputImageType::ImageDimension };
  typedef Image<unsigned long, ImageDimension> OutputImageType;
  typedef typename InputImageType::RegionType ImageRegionType;
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef Boundary<InputPixelType, ImageDimension> BoundaryType;
  typedef typename BoundaryType::IndexType BoundaryIndexType;
  typedef SegmentTable<InputPixelType> SegmentTableType;
  typedef DataObject::Pointer DataObjectPointer;
  
  /** Methods to implement smart pointers and work with the itk object factory
   */
  typedef Segmenter Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(Segmenter, ProcessObject);
  
  /** A constant used in the labeling algorithm.  */
  static unsigned long NULL_LABEL;

  /** A constant used in the labeling algorithm.  */
  static short NULL_FLOW;
 
  /** Get/Set the input image.   */
  typename InputImageType::Pointer GetInputImage()
    { return static_cast<InputImageType *>
        (this->ProcessObject::GetInput(0).GetPointer());    }
  void SetInputImage(InputImageType *img)
    {  this->ProcessObject::SetNthInput(0, img); }

  /** Get/Set the labeled output image.  The output image is always of
    unsigned long integers. */
  typename OutputImageType::Pointer GetOutputImage()
    { return static_cast<OutputImageType *>
        (this->ProcessObject::GetOutput(0).GetPointer()); }
  void SetOutputImage(OutputImageType *img)
    { this->ProcessObject::SetNthOutput(0, img);    }
  
  /** Get/Set the segment table.  The segment table is a table of segmentation
   * information identifying each region produced by the labeling algorithm. */ 
  SegmentTableType::Pointer GetSegmentTable()
    { return static_cast<SegmentTableType *>
        (this->ProcessObject::GetOutput(1).GetPointer()); }
  void SetSegmentTable(SegmentTableType *s)
    { this->ProcessObject::SetNthOutput(1, s); }
  
  /** Returns the boundary information data necessary only for data streaming
    applications.  */
  BoundaryType::Pointer GetBoundary()
    { return static_cast<BoundaryType *>
        (this->ProcessObject::GetOutput(2).GetPointer()); }
  void SetBoundary(BoundaryType *b)
    { this->ProcessObject::SetNthOutput(2,b); }
  
  /** Standard non-threaded pipeline execution method. */
  void GenerateData();

  /** This method is necessary until the streaming mechanisms of the Itk
   * pipeline are full fleshed out.  It is only used for streaming
   * applications.  Calling this method gets/sets the image size of the
   * complete volume being streamed.  The member variables controlled by
   * this method will not be modified by the Itk pipeline and are necessary
   * for analysis of boundaries.   */
  void SetLargestPossibleRegion(ImageRegionType reg)
    {
      m_LargestPossibleRegion = reg;
      this->Modified();
    }
  ImageRegionType GetLargestPossibleRegion() const
    {      return m_LargestPossibleRegion; }

  /** Helper function.  Other classes may have occasion to use this. Relabels
      an image according to a table of equivalencies. */
  static void RelabelImage(typename OutputImageType::Pointer,
                           ImageRegionType,
                           EquivalencyTable::Pointer);
  
  /** Standard itk::ProcessObject subclass method. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);

  /** Gets/Sets the initial label (unsigned long integer value) used
   * by the labeling algorithm.  Only necessary for streaming applications. */
  itkSetMacro(CurrentLabel, unsigned long);
  itkGetMacro(CurrentLabel, unsigned long);

  /** Gets/Sets the input threshold. Threshold is specified as a percentage
   * (0.0 - 1.0) of the maximum height of the image. This filter thresholds the
   * input image to remove all values below /f$ L = min + T * (max -  min) /f$,
   * where /f$ max, min /f$ are the maximum, minimum values in the image and /f$
   * T /f$ is the threshold parameter value. Values in the image less than /f$ L
   * /f$ are raised to /f$ L /f$. Thresholding minimum values in the image
   * decreases the number of local minima in the image and produces an initial
   * segmentation with fewer segments.  The assumption is that the ``shallow''
   * regions that this thresholding eliminates are generally not of interest.*/ 
  itkSetClampMacro(Threshold, double, 0.0, 1.0);
  itkGetMacro(Threshold, double);

  /** Turns on special labeling of the boundaries for streaming applications.
   * The default value is FALSE, meaning that boundary analysis is turned
   * off.   */
  itkSetMacro(DoBoundaryAnalysis, bool);
  itkGetMacro(DoBoundaryAnalysis, bool);

  /** Prior to thresholding the input image, the filter calculates minimum
   * and maximum values for the image.  These values are stored as
   * member variables.   */
  itkGetMacro(Minimum, InputPixelType);
  itkSetMacro(Minimum, InputPixelType);
  itkGetMacro(Maximum, InputPixelType);
  itkSetMacro(Maximum, InputPixelType);
  
  /** Determines whether the algorithm will sort the adjacencies in its
   * SegmentTable before returning.  Default is true.  This is an option only
   * useful for streaming applications where the sorting only needs to be done
   * after all iterations have taken place.*/
  itkGetMacro(SortEdgeLists, bool);
  itkSetMacro(SortEdgeLists, bool);

protected:
  /** Structure storing information about image flat regions.
   * Flat regions are connected pixels of the same value.  */
  struct flat_region_t
  {
    unsigned long   *min_label_ptr;
    InputPixelType  bounds_min;
    //    InputPixelType  bounds_max; // <-- may not be necc.
    InputPixelType  value;
    bool            is_on_boundary;
    flat_region_t() : is_on_boundary(false) {}
  };

  /** Table for storing flat region information.  */
  typedef itk::hash_map<unsigned long, flat_region_t, itk::hash<unsigned long> >
    flat_region_table_t;

  struct connectivity_t
  {
    unsigned int size;
    unsigned int *index;
    typename InputImageType::OffsetType *direction;
  };

  /** Table for storing tables of edges.  This is convenient in
   * generating the segment table,  even though the edge tables
   * are stored as ordered lists.  An ``edge'' in this context
   * is synonymous with a segment ``adjacency''.   */
  typedef itk::hash_map<unsigned long, InputPixelType, itk::hash<unsigned long> 
  > edge_table_t;
  
  typedef itk::hash_map<unsigned long, edge_table_t, itk::hash<unsigned long>
  > edge_table_hash_t;
  
  Segmenter();
  Segmenter(const Self&) {}
  virtual ~Segmenter()
    {
      if (m_Connectivity.index != 0)     delete[] m_Connectivity.index;
      if (m_Connectivity.direction !=0 ) delete[] m_Connectivity.direction;
    }
  void PrintSelf(std::ostream& os, Indent indent) const;
  void operator=(const Self&) {}
  
  /** Constructs the connectivity list and the corresponding set of directional
   * Offset indicies. */
  virtual void GenerateConnectivity();
  
  /** This method asks for an image region that is one pixel larger
   * at each boundary than the region being processed.  This single pixel
   * expansion represents an overlap with adjacent image chunks   */
  void GenerateInputRequestedRegion();
  void GenerateOutputRequestedRegion(DataObject *output);
  void UpdateOutputInformation();

  /**  Allocates boundary structure information and sets the
   * boundary data to null values.   */
  void InitializeBoundary();

  /** Performs a gradient descent connected component analysis
   * at the boundaries of the images that border other
   * image chunks.  Useful only in data streaming applications.   */
  void AnalyzeBoundaryFlow(typename InputImageType::Pointer,
                           flat_region_table_t &,
                           InputPixelType);

  /** Fills boundary pixels with a specified value.  Used by labeling
   * methods to build a very high ``wall'' around the image so that
   * gradient descent does not need to watch boundaries.   */
  void BuildRetainingWall(typename InputImageType::Pointer,
                          ImageRegionType, InputPixelType);

  /** Labels all the local minima in the image.  Also identifies and labels
   * connected  ``flat'' regions.   */
  void LabelMinima(typename InputImageType::Pointer,
                   ImageRegionType, flat_region_table_t &,
                   InputPixelType);

  /** Follows each unlabeled pixel in the image down its path of steepest
   * descent.  Each pixel along that path is identified with the local minima
   * already labeled at the end of the path.   */
  void GradientDescent(typename InputImageType::Pointer,
                       ImageRegionType);

  /** Associates each flat region with a local minimum and relabels
    accordingly.  */ 
  void DescendFlatRegions(flat_region_table_t &, ImageRegionType);

  /** Adds entries to the output segment table for all labeled segments in the
   * image.  */
  void UpdateSegmentTable(typename InputImageType::Pointer, ImageRegionType);

  /** Traverses each boundary and fills in the data needed for joining
   * streamed chunks of an image volume.  Only necessary for streaming
   * applications.   */
  void CollectBoundaryInformation(flat_region_table_t &);
  
  /** Helper function.  Thresholds low values and copies values from one image
   * into another. The source and destination regions must match in size (not
   * enforced).   */
  static void Threshold(typename InputImageType::Pointer destination,
                        typename InputImageType::Pointer source,
                        const ImageRegionType source_region,
                        const ImageRegionType destination_region,
                        InputPixelType threshold);

  /** Helper function.  Finds the minimum and maximum values in an image. */
  static void MinMax(typename InputImageType::Pointer img,
                     ImageRegionType region,
                     InputPixelType &min,
                     InputPixelType &max);

  /** Helper function. Finds the minimum and maximum values in an image.   */ 
  static void MergeFlatRegions(flat_region_table_t &, EquivalencyTable::Pointer);
    
  /** Helper functions for filling in regions with values   */
  static void SetImageValues(typename InputImageType::Pointer img,
                             const ImageRegionType region,
                             InputPixelType value);

  static void SetImageValues(typename OutputImageType::Pointer img,
                             const ImageRegionType region,
                             unsigned long value);

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

  bool m_SortEdgeLists;
  bool m_DoBoundaryAnalysis;
  double m_Threshold;
  double m_MaximumFloodLevel;
  InputPixelType m_Minimum;
  InputPixelType m_Maximum;
  unsigned long m_CurrentLabel;
};
  
}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedSegmenter.txx"
#endif

#endif
