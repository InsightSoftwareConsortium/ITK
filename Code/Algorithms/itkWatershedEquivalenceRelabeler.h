/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedEquivalenceRelabeler.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedEquivalenceRelabeler_h
#define __itkWatershedEquivalenceRelabeler_h

#include "itkEquivalencyTable.h"
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
 * arbitrary dimension, and an itk::EquivalencyTable.  The input
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
class ITK_EXPORT EquivalenceRelabeler
  : public ProcessObject
{
public:
  /** Expose templated image dimension parameter at run time */
  itkStaticConstMacro(ImageDimension, unsigned int,TImageDimension);
  
  /**  Some convenient typedefs.   */
  typedef Image<unsigned long, TImageDimension> ImageType;
  typedef EquivalenceRelabeler Self;
  typedef ProcessObject Superclass;
  typedef TScalarType ScalarType;
  typedef EquivalencyTable EquivalencyTableType;
  typedef Segmenter<Image<ScalarType, TImageDimension> > SegmenterType;
  typedef DataObject::Pointer DataObjectPointer;
  
  /**  Define smart pointers for this object.   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(EquivalenceRelabeler, ProcessObject);

  /** Set/Get the image to relabel.   */
  void SetInputImage(ImageType *img)
  {      this->ProcessObject::SetNthInput(0, img);    }
  const ImageType * GetInputImage(void)
  {
    return static_cast<ImageType *>
      (this->ProcessObject::GetInput(0));
  }

  /** Set/Get the output image */
  void SetOutputImage(ImageType * img)
  {
    this->ProcessObject::SetNthOutput(0,img);
  }
  typename ImageType::Pointer GetOutputImage()
  { return static_cast<ImageType *>
      (this->ProcessObject::GetOutput(0)); }
  
  /** Set/Get the table to use in relabeling the input image.   */
  void SetEquivalencyTable(EquivalencyTableType *et)
  {
    this->ProcessObject::SetNthInput(1, et);
  }
  EquivalencyTableType::Pointer GetEquivalencyTable()
  {
    return static_cast<EquivalencyTableType *>
      (this->ProcessObject::GetInput(1));
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
