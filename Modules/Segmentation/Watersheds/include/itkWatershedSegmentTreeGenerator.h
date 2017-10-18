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
#ifndef itkWatershedSegmentTreeGenerator_h
#define itkWatershedSegmentTreeGenerator_h

#include "itkWatershedSegmentTable.h"
#include "itkWatershedSegmentTree.h"
#include "itkEquivalencyTable.h"

#include <algorithm>
#include <utility>

namespace itk
{
namespace watershed
{
/**
 * \class SegmentTreeGenerator
 * This filter is a process object class that implements a step in the N-d
 * watershed segmentation algorithm.  It produces an ordered list (SegmentTree)
 * of binary merges between segments at increasing saliency levels. This
 * filter is used in conjunction with WatershedSegmenter and WatershedRelabeler
 * to process an initial segmentation into a final relabeled volume.  See
 * itk::WatershedImageFilter for an overview of watershed segmentation.
 *
 * \par Inputs
 * There are two inputs to this filter, the second is optional.
 * (1) A table of segments (SegmentTable) derived from a labeled image.
 * (2) Optionally, a table of predetermined equivalencies among segments
 * (EquivalencyTable), necessary for streaming applications where
 * segments may be joined across streamed chunk boundaries.  The flag
 * Merge must be set to true to enable this functionality.
 *
 * \par Outputs
 * The output of this filter is a list of binary merges of segments at
 * increasing saliency.  This is the data structure
 * itk::watershed::WatershedSegmentTree referred to as a "merge tree" in the
 * itk::WatershedImageFilter documentation.
 *
 * \par Parameters
 * There are two parameters to this filter described below.
 *
 * \par
 * FloodLevel is specified as a percentage (0.0 - 1.0) of the maximum possible
 * saliency value in the initial image from which the segment table was
 * derived.  A value of 0.0 calculates no merges.  A value of 1.0 calculates
 * all of the potential merges that can occur as the FloodLevel is increased to
 * the maximum saliency value.  Typically, there is no need to calculate merges
 * past about 40% of the total depth.  Because this is the most computationally
 * intensive piece of the watershed segmentation algorithm, it is a good idea
 * to tune this parameter as low as possible on larger volumes.
 *
 * \par
 * Merge is a boolean flag indicating whether or not to pre-merge the segments
 * marked as equivalent in the EquivalencyTable.  This is only useful for
 * streaming applications and is turned off by default.  (TRUE == merge, FALSE
 * == do not merge).
 * \sa itk::WatershedImageFilter
 * \ingroup WatershedSegmentation
 * \ingroup ITKWatersheds
 */
template< typename TScalar >
class ITK_TEMPLATE_EXPORT SegmentTreeGenerator:public ProcessObject
{
public:
  /**  Standard itk smart pointer declarations    */
  typedef SegmentTreeGenerator       Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  itkTypeMacro(WatershedSegmentTreeGenerator, ProcessObject);

  /** Convenient type definitions */
  typedef TScalar                    ScalarType;
  typedef SegmentTable< ScalarType > SegmentTableType;
  typedef SegmentTree< ScalarType >  SegmentTreeType;
  typedef EquivalencyTable           EquivalencyTableType;
  typedef OneWayEquivalencyTable     OneWayEquivalencyTableType;
  typedef DataObject::Pointer        DataObjectPointer;

  /** Typedefs to avoid internal compiler error bug on Microsoft VC++ */
  typedef typename SegmentTableType::Pointer           SegmentTableTypePointer;
  typedef typename OneWayEquivalencyTableType::Pointer OneWayEquivalencyTableTypePointer;
  typedef typename SegmentTreeType::Pointer            SegmentTreeTypePointer;

  /** Get/Set the input table of segments to process */
  SegmentTableType * GetInputSegmentTable()
  {
    return static_cast< SegmentTableType * >( this->ProcessObject::GetInput(0) );
  }

  void SetInputSegmentTable(SegmentTableType *st)
  {
    // Reset the highest calculated flood level if we are given a
    // different input image.
    if ( st != this->GetInput(0) )
      {
      m_HighestCalculatedFloodLevel = 0.0;
      }
    this->ProcessObject::SetNthInput(0, st);
  }

  /** Get/Set input table of equivalencies to pre-merge before
   * running the tree generator algorithm.  Only useful for
   * streaming applications */
  void SetInputEquivalencyTable(EquivalencyTableType *eq)
  {
    this->ProcessObject::SetNthInput(1, eq);
  }

  EquivalencyTableType * GetInputEquivalencyTable()
  {
    return
      static_cast< EquivalencyTableType * >( this->ProcessObject::GetInput(1) );
  }

  /** Get/Set the output data */
  SegmentTreeType * GetOutputSegmentTree()
  {
    return static_cast< SegmentTreeType * >
           ( this->ProcessObject::GetOutput(0) );
  }

  /** Standard non-threaded itk pipeline method */
  virtual void GenerateData() ITK_OVERRIDE;

  /** Get/Set a boolean flag indicating whether or not to pre-merge the
    segments marked  as equivalent in the EquivalencyTable.  This is only
    useful for streaming  applications and is turned off by default.  (TRUE ==
    merge, FALSE == do not merge).       */
  itkSetMacro(Merge, bool);
  itkGetConstMacro(Merge, bool);

  /** Get/Set FloodLevel.  FloodLevel is specified as a percentage (0.0 - 1.0)
   of the maximum possible saliency value in the initial image from which the
   segment table was derived. A value of 0.0 calculates no merges.  A value of
   1.0 calculates all of the potential merges that can occur as the FloodLevel
   is increased to the  maximum saliency value.    */
  void SetFloodLevel(double);

  itkGetConstMacro(FloodLevel, double);

  /** Get/Set HighestCalculatedFloodLevel.  HighestCalculatedFloodLevel keeps
   * track of the highest level this filter has computed.  It is
   * used to prevent unnecessary re-execution of the filter. */
  itkSetMacro(HighestCalculatedFloodLevel, double);
  itkGetConstMacro(HighestCalculatedFloodLevel, double);

  /** Get/Set a flag that prevents the filter from copying its input
   * segment table before executing.  This can be enabled to conserve
   * memory, especially in streaming applications where memory is a
   * concern. If enabled, the input to this filter must always be
   * re-executed on updates. Default is false. */
  itkSetMacro(ConsumeInput, bool);
  itkGetConstMacro(ConsumeInput, bool);

  /** Performs a merge of two segments in a SegmentTable according
   * to criteria specific to this algorithm.   */
  static void MergeSegments(SegmentTableTypePointer,
                            OneWayEquivalencyTableTypePointer,
                            const IdentifierType,
                            const IdentifierType);

  /** This method should not be used.  It will be removed in future versions
   * of this filter. */
  static void PruneMergeSegments(SegmentTableTypePointer,
                                 OneWayEquivalencyTableTypePointer,
                                 const IdentifierType,
                                 const IdentifierType,
                                 ScalarType);

  /** Standard itk::ProcessObject subclass method. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

protected:
  SegmentTreeGenerator();
  virtual ~SegmentTreeGenerator() ITK_OVERRIDE {}
  SegmentTreeGenerator(const Self &) {}
  void operator=(const Self &) {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generates an initial list of all potentential merges in
   * the segment table.   */
  void CompileMergeList(SegmentTableTypePointer, SegmentTreeTypePointer);

  /** Compiles a list of all the actual segment merges up to the specified
   * flood level, recomputing new potential merges as it goes.   */
  void ExtractMergeHierarchy(SegmentTableTypePointer, SegmentTreeTypePointer);

  void MergeEquivalencies();

  /** Methods required by the itk pipeline */
  virtual void GenerateOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

private:
  bool   m_Merge;
  double m_FloodLevel;
  bool   m_ConsumeInput;

  typedef itksys::hash_map< IdentifierType, bool,
                            itksys::hash< IdentifierType > >  HashMapType;

  OneWayEquivalencyTableType::Pointer m_MergedSegmentsTable;

  /** This value keeps track of the highest level this filter has been
   *  calculated.  m_FloodLevel can be manipulated anywhere below this
   *  level without re-executing the filter, preventing unnecessary
   *  updates. */
  double m_HighestCalculatedFloodLevel;
};
} // end namespace watershed
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedSegmentTreeGenerator.hxx"
#endif

#endif
