/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedBoundaryResolver.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedBoundaryResolver_h
#define __itkWatershedBoundaryResolver_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkWatershedBoundary.h"
#include "itkEquivalencyTable.h"
#include "itkWatershedSegmenter.h"

namespace itk
{
namespace watershed
{
/** \class BoundaryResolver
 *  This filter implements a piece of the streaming watershed
 *  segmentation algorithm.  It takes in pairs of itk::watershed::Boundary
 *  objects and connects the labeling of pixels across image chunk boundaries.
 *  Read the documentation found in itk::WatershedImageFilter and the other
 *  watershed segmentation component objects for more information.
 *
 * \par A note on terminology in itk watershed segmentation code
 * For streamed segmentation of images in the watershed framework, the
 * documentation refers to the complete data set at the ``image volume.''  The 
 * image volume is assumed to be partitioned into pieces referred to as image
 * chunks. Each chunk is processed in sequence through the pipeline.  The
 * ``face'' of an image chunk is an N-1 dimensional boundary region of an N
 * dimensional chunk (the planar faces of a cube, for example).
 
 * \par Input
 * The input to this filter is a pair of itk::watershed::Boundary pointers
 * (BoundaryA and BoundaryB).
 * The algorithm assumes that these Boundaries come from facing chunks in the
 * image volume.  The faces that align need to be specified in the parameters
 * of the filter.
 *
 * \par Output
 * This filter outputs a table of equivalencies among labels.  See
 * itk::EquivalencyTable for more information.
 * \ingroup WatershedSegmentation
 *
 * \par Parameters
 * The only parameters to set on this filter are the indicies of the faces that
 * align in the boundary object inputs.  See itk::Boundary for a description of
 * how boundary faces are indexed.
 * \sa itk::watershed::Boundary
 * \ingroup WatershedSegmentation
 */
template <class TPixelType, unsigned int TDimension>
class ITK_EXPORT BoundaryResolver : public ProcessObject
{
public:
  /** Set up smart pointer and object factory definitions.   */
  typedef BoundaryResolver Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(BoundaryResolver, ProcessObject);
  
  /** Expose the image dimension at run time. */
  itkStaticConstMacro(ImageDimension, unsigned int, TDimension);

  /** Some convenient typedefs.   */
  typedef TPixelType PixelType;
  typedef Boundary<PixelType, TDimension> BoundaryType;
  typedef EquivalencyTable EquivalencyTableType;
  typedef Segmenter<Image<TPixelType, TDimension> > SegmenterType;
  typedef DataObject::Pointer DataObjectPointer;
  
  /** Set/Get the first of two boundaries that are to be resolved.   */
  void SetBoundaryA(BoundaryType *bd)
  { this->ProcessObject::SetNthInput(0, bd); }
  typename BoundaryType::Pointer GetBoundaryA()
  { return static_cast<BoundaryType *>(this->GetInput(0));  }
  
  /** Set/Get the second of two boundaries that are to be resolved.  */
  void SetBoundaryB(BoundaryType *bd)
  { this->ProcessObject::SetNthInput(1, bd); }
  typename BoundaryType::Pointer GetBoundaryB()
  { return static_cast<BoundaryType *>(this->GetInput(1));  }
  
  /**  Set/Get the face of the boundary object that we are going to
       resolve. */
  itkSetMacro(Face, unsigned short);
  itkGetMacro(Face, unsigned short);
  
  /** This method sets/gets the equivalency table used to store equivalencies
   *  among segments that are generated from the boundary resolution
   *  algorithm.  */
  void SetEquivalencyTable(EquivalencyTableType::Pointer a)
  { this->ProcessObject::SetNthOutput(0, a.GetPointer()); }
  EquivalencyTableType::Pointer GetEquivalencyTable()
  { return static_cast<EquivalencyTableType *>
      (this->ProcessObject::GetOutput(0)); }

  /** Standard non-threaded pipeline method */
  void GenerateData();

  /** Standard itk::ProcessObject subclass method. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);  
protected:
  BoundaryResolver() : m_Face(0)
  {
    EquivalencyTable::Pointer eq
      = static_cast<EquivalencyTable*>(this->MakeOutput(0).GetPointer());
    this->SetNumberOfRequiredOutputs(1);
    this->ProcessObject::SetNthOutput(0, eq.GetPointer());
  }
  virtual ~BoundaryResolver() {}
  BoundaryResolver(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  unsigned short m_Face;
  void GenerateOutputRequestedRegion(DataObject *output);
};
  
}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedBoundaryResolver.txx"
#endif

#endif
