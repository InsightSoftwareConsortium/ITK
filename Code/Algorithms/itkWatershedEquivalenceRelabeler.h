/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedEquivalenceRelabeler.h
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
#ifndef __itkWatershedEquivalenceRelabeler_h
#define __itkWatershedEquivalenceRelabeler_h

#include "itkWatershedEquivalencyTable.h"
#include "itkWatershedSegmenter.h"
#include "itkImage.h"

namespace itk
{
namespace watershed
{
/** \class EquivalenceRelabeler
 *
 * This class is part of the set of watershed segmentation component objects.
 * It is an image-to-image filter that relabels its input according to a set of
 * equivalencies defined in a table.  The filter is used in
 * itk::WatershedImageFilter, for example, to relabel a segmented image
 * at different hierarchies in the merge tree (see itk::WatershedImageFilter
 * for documentation on terminology).  It simply takes its input and changes
 * any values found in the equivalency table.
 *
 * \par Inputs
 * There are two inputs to this filter, an unsigned long itk::Image of
 * arbitrary dimension, and an itk::watershed::EquivalencyTable.  The input
 * image is the image to be relabeled and copied to the output, and the
 * EquivalencyTable identifies  how to relabel the values.
 *
 * \par Output
 * The output of this filter is the relabeled unsigned long itk::Image of same
 * dimension and size as the input.
 *
 * \ingroup WatershedSegmentation
 * \sa itk::WatershedImageFilter
 * \sa EquivalencyTable */
template <class TScalarType, unsigned int TImageDimension>
class EquivalenceRelabeler
  : public ProcessObject
{
public:
  /** Expose templated image dimension parameter at run time */
  enum {ImageDimension = TImageDimension};
  
  /**  Some convenient typedefs.   */
  typedef Image<unsigned long, ImageDimension> ImageType;
  typedef EquivalenceRelabeler Self;
  typedef ProcessObject Superclass;
  typedef TScalarType ScalarType;
  typedef EquivalencyTable EquivalencyTableType;
  typedef Segmenter<Image<ScalarType, ImageDimension> > SegmenterType;
  typedef DataObject::Pointer DataObjectPointer;
  
  /**  Define smart pointers for this object.   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(EquivalenceRelabeler, ProcessObject);

  /** Set/Get the image to relabel.   */
  void SetInputImage(ImageType *img)
    {      this->ProcessObject::SetNthInput(0, img);    }
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
  
  /** Set/Get the table to use in relabeling the input image.   */
  void SetEquivalencyTable(EquivalencyTableType *et)
    {
      this->ProcessObject::SetNthInput(1, et);
    }
  EquivalencyTableType::Pointer GetEquivalencyTable()
    {
      return static_cast<EquivalencyTableType *>
        (this->ProcessObject::GetInput(1).GetPointer());
    }

  /** Standard non-threaded pipeline method */
  void GenerateData();

  /** Standard itk::ProcessObject subclass method. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);

protected:
  EquivalenceRelabeler()
    {
      typename ImageType::Pointer img
        = static_cast<ImageType*>(this->MakeOutput(0).GetPointer());
      this->SetNumberOfRequiredOutputs(1);
      this->ProcessObject::SetNthOutput(0, img.GetPointer());
    }
  virtual ~EquivalenceRelabeler() {}
  EquivalenceRelabeler(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  void GenerateOutputRequestedRegion(DataObject *output);
  void GenerateInputRequestedRegion();
};
  
}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedEquivalenceRelabeler.txx"
#endif

#endif
