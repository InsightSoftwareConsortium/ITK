/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTreeGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedSegmentTreeGenerator_h
#define __itkWatershedSegmentTreeGenerator_h

#include "itkWatershedSegmentTable.h"
#include "itkWatershedSegmentTree.h"
#include "itkWatershedEquivalencyTable.h"
#include "itkWatershedOneWayEquivalencyTable.h"

#include <algorithm>
#include <utility>

namespace itk
{
namespace watershed
{
 
/**
 * \class WatershedSegmentTreeGenerator
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
 * itk::watershed::WatershedSegmentTree referred to as a ``merge tree'' in the
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
 * \ingroup WatershedSegmentation  */
template <class TScalarType>
class ITK_EXPORT SegmentTreeGenerator  : public ProcessObject
{
public:
  /**  Standard itk smart pointer declarations    */
  typedef SegmentTreeGenerator Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(SegmentTreeGenerator, ProcessObject);

  /** Convenient type definitions */
  typedef TScalarType ScalarType;
  typedef SegmentTable<ScalarType> SegmentTableType;
  typedef SegmentTree<ScalarType>  SegmentTreeType;
  typedef EquivalencyTable         EquivalencyTableType;
  typedef OneWayEquivalencyTable   OneWayEquivalencyTableType;
  typedef DataObject::Pointer      DataObjectPointer;

  /** Typedefs to avoid internal compiler error bug on Microsoft VC++ */
  typedef typename SegmentTableType::Pointer SegmentTableTypePointer;
  typedef typename OneWayEquivalencyTableType::Pointer
  OneWayEquivalencyTableTypePointer;
  typedef typename SegmentTreeType::Pointer SegmentTreeTypePointer;
  

  /** Get/Set the input table of segments to process */
  SegmentTableType * GetInputSegmentTable()
  { return static_cast<SegmentTableType *>
      (this->ProcessObject::GetInput(0)); }
  EquivalencyTableType * GetInputEquivalencyTable()
  { return static_cast<EquivalencyTableType *>
      (this->ProcessObject::GetInput(1)); }

  /** Get/Set input table of equivalencies to pre-merge before
   * running the tree generator algorithm.  Only useful for
   * streaming applications */
  void SetInputEquivalencyTable(EquivalencyTableType *eq)
  { this->ProcessObject::SetNthInput(1, eq); }
  void SetInputSegmentTable(SegmentTableType *st)
  { this->ProcessObject::SetNthInput(0, st); }

  /** Get/Set the output data */
  SegmentTreeType * GetOutputSegmentTree()
  { return static_cast<SegmentTreeType *>
      (this->ProcessObject::GetOutput(0)); }

  /** Standard non-threaded itk pipeline method */
  void GenerateData();

  /** Get/Set a boolean flag indicating whether or not to pre-merge the
    segments marked  as equivalent in the EquivalencyTable.  This is only
    useful for streaming  applications and is turned off by default.  (TRUE ==
    merge, FALSE == do not merge).       */
  itkSetMacro(Merge, bool);
  itkGetMacro(Merge, bool);

  /** Get/Set FloodLevel.  FloodLevel is specified as a percentage (0.0 - 1.0)
   of the maximum possible saliency value in the initial image from which the
   segment table was derived. A value of 0.0 calculates no merges.  A value of
   1.0 calculates all of the potential merges that can occur as the FloodLevel
   is increased to the  maximum saliency value.    */
  void SetFloodLevel(double);
  itkGetMacro(FloodLevel, double);

  /** Get/Set HighestCalculatedFloodLevel.  HighestCalculatedFloodLevel keeps
   * track of the highest level this filter has been asked to compute.  It is
   * used to prevent unneccessary re-execution of the filter. */
  itkSetMacro(HighestCalculatedFloodLevel, double);
  itkGetMacro(HighestCalculatedFloodLevel, double);

  /** Get/Set a flag that prevents the filter from copying its input segment
   * table before executing.  This can be enabled to conserve memory, especially 
   * in streaming applications where memory is a concern. If enabled, the input 
   * to this filter must always be re-executed on updates. Default is false.*/
  itkSetMacro(ConsumeInput, bool);
  itkGetMacro(ConsumeInput, bool);

  /** Performs a merge of two segments in a SegmentTable according
   * to criteria specific to this algorithm.   */
  static void MergeSegments(SegmentTableTypePointer,
                            OneWayEquivalencyTableTypePointer,
                            const unsigned long,
                            const unsigned long);

  /** This method should not be used.  It will be removed in future versions
   * of this filter. */
  static void PruneMergeSegments(SegmentTableTypePointer,
                                 OneWayEquivalencyTableTypePointer,
                                 const unsigned long,
                                 const unsigned long,
                                 ScalarType);

  /** Standard itk::ProcessObject subclass method. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);
  
protected:
  SegmentTreeGenerator();
  virtual ~SegmentTreeGenerator() {}
  SegmentTreeGenerator(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generates an initial list of all potentential merges in
   * the segment table.   */
  void CompileMergeList(SegmentTableTypePointer, SegmentTreeTypePointer);

  /** Compiles a list of all the actual segment merges up to the specified
   * flood level, recomputing new potential merges as it goes.   */
  void ExtractMergeHierarchy(SegmentTableTypePointer, SegmentTreeTypePointer);

  void MergeEquivalencies();

  /** Methods required by the itk pipeline */
  void GenerateOutputRequestedRegion(DataObject *output);
  void GenerateInputRequestedRegion();
  
private:
  bool m_Merge;
  double m_FloodLevel;
  bool m_ConsumeInput;
  OneWayEquivalencyTableType::Pointer m_MergedSegmentsTable;
  
  /** This value keeps track of the highest level this filter has been asked to 
   *  calculate.  m_FloodLevel can be manipulated anywhere below this level
   *  without re-executing the filter, preventing unneccesary updates. */
  double m_HighestCalculatedFloodLevel;
};
  
}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedSegmentTreeGenerator.txx"
#endif

#endif

