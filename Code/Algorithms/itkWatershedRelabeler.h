/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedRelabeler.h
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
#ifndef __itkWatershedRelabeler_h
#define __itkWatershedRelabeler_h

#include "itkWatershedEquivalencyTable.h"
#include "itkWatershedSegmentTree.h"
#include "itkWatershedSegmenter.h"
#include "itkImage.h"

namespace itk
{
namespace watershed
{

/** \class Relabeler
 *
 * This filter implements the final step in the watershed segmentation
 * algorithm.  It is responsible for relabeling a segmented image according to
 * a specified saliency level (flood level) in a merge tree.  (See
 * itk::WatershedImageFilter for information on terminology used in this
 * documentation.)
 *
 * \par Inputs
 * There are two inputs to this filter.  The first input is a labeled image of 
 * unsigned long integers, such as is produced by itk::watershed::Segmenter.
 * The second input is an itk::watershed::SegmentTree, which is the merge tree 
 * data structure produced by the itk::watershed::SegmentTreeGenerator
 * filter.  The merge tree represents the hierarchy of merges among adjacent
 * segments in the initial segmentation image.
 *
 * \par Output
 * The output of this filter is a relabeled image of unsigned long integers of 
 * dimension and size matching the input.
 *
 * \par Parameters
 * There is a single parameter FloodLevel for this filter.  FloodLevel is
 * given in percentage points (0.0 - 1.0) of the maximum saliency found in the 
 * merge tree.  A FloodLevel of 0.0 will produce an output in which no
 * segments are relabeled (merged).  A FloodLevel of 1.0 will produce an
 * output in which all the entries in the merge tree are used to relabel the
 * image.  FloodLevel controls which level in the segmentation hierarchy to
 * produce on the output.
 *
 * \ingroup WatershedSegmentation
 * \sa itk::WatershedImageFilter
 * \sa itk::watershed::EquivalencyTable
 * \sa itk::watershed::SegmentTree  */
template <class TScalarType, unsigned int TImageDimension>
class Relabeler
  : public ProcessObject
{
public:
  /** Define smart pointers for this object */
  typedef Relabeler Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(Relabeler, ProcessObject);

  /** Expose the ImageDimension template parameter at run time */
  enum {ImageDimension = TImageDimension};
  
  /** Some convenient typedefs */
  typedef TScalarType ScalarType;
  typedef Image<unsigned long, ImageDimension> ImageType;
  typedef SegmentTree<ScalarType> SegmentTreeType;
  typedef Segmenter<Image<ScalarType, ImageDimension> > SegmenterType;
  typedef DataObject::Pointer DataObjectPointer;

  /** Standard itk::ProcessObject subclass method. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);
  
  /** Set/Get the input image */
  void SetInputImage(ImageType *img)
    {
      this->ProcessObject::SetNthInput(0, img);
    }
  typename ImageType::Pointer GetInputImage()
    {
      return static_cast<ImageType *>
        (this->ProcessObject::GetInput(0).GetPointer());
    }

  /** Set/Get the output image */
  void SetOutputImage(ImageType * img)
    {
      this->ProcessObject::SetNthOutput(0,img);
    }
  typename ImageType::Pointer GetOutputImage()
    { return static_cast<ImageType *>
        (this->ProcessObject::GetOutput(0).GetPointer()); }

  /** Set/Get the input tree that defines segment merges */
  void SetInputSegmentTree(SegmentTreeType *et)
    {
      this->ProcessObject::SetNthInput(1, et);
    }
  SegmentTreeType::Pointer GetInputSegmentTree()
    {
      return static_cast<SegmentTreeType *>
        (this->ProcessObject::GetInput(1).GetPointer());
    }

  /** Standard non-threaded pipeline method */
  void GenerateData();

  /** Set/Get the percentage of the maximum saliency level
   * to merge to. */
  itkSetClampMacro(FloodLevel, double, 0.0, 1.0);
  itkGetMacro(FloodLevel, double);

  /** Standard ProcessObject method used in implementing mini-pipelines */
  void GraftOutput(ImageType *graft);
  void GraftNthOutput(unsigned int idx, ImageType *graft);
  
protected:
  Relabeler();
  virtual ~Relabeler() {}
  Relabeler(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  double m_FloodLevel;
  void GenerateOutputRequestedRegion(DataObject *output);
  void GenerateInputRequestedRegion();
};
  
}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedRelabeler.txx"
#endif

#endif

