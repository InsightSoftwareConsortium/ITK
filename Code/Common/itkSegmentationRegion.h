/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationRegion.h
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
#ifndef _itkSegmentationRegion_h
#define _itkSegmentationRegion_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class SegmentationRegion
 * \brief Base class for SegmentationRegion object
 * 
 * itkSegmentationRegion is the base class for the SegmentationRegion
 * objects. It provides the basic function definitons that are inherent to a
 * SegmentationRegion objects.  A region object is defined by the label it
 * owns. We use integer labels to represent a region. No two regions can have
 * the same labels This object stores the region label. The user can get the
 * area, mean region intensity and a unique label associate with the region
 * through access functions provided publicly. The unique label parameter is
 * used at the end of a region growing class to associate a region with a new
 * label that characterises the region after several regions have been
 * merged.  This class provides an interface method called
 * ApplySegmentationRegion is defined as virtual to enable future extension 
 * of the region object.  
 * This object can be used where region in images needs to be defined.
 * Chlid classes can be derived from this class for tailoring them towards 
 * specific algorithmic needs.
 *
 * \ingroup RegionGrowingSegmentation 
 */
class ITK_EXPORT SegmentationRegion : public Object
{
public:
  /** Standard class typedefs. */
  typedef SegmentationRegion   Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SegmentationRegion,Object);

  /** Type definition for the input image. */
//  typedef typename TInputImage::Pointer   InputImageType;

  /** Type definition for the input image pixel type. */
//  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Type definition for an double vector. */
  typedef vnl_matrix<double> VecDblType;

  /** Define a virtual SegmentationRegion function that is meant to be
   * used in derived classes if some operation needs to be
   * performed on a region object. */
  virtual void ApplySegmentationRegion(){};

  /** Set the region with parameter values
   * defining the region. */
  itkSetMacro(RegionLabel, unsigned int);

  /** Get the label associated with the region.  */
  itkGetMacro(RegionLabel, unsigned int);

  /** Set/Get the area of the region. */
  itkSetMacro(RegionArea, unsigned int);
  itkGetMacro(RegionArea, unsigned int);

  /** Set/Get the area of the region. */
  itkSetMacro(UniqueLabel, unsigned int);
  itkGetMacro(UniqueLabel, unsigned int);

  /** Set/Get the mean pixel intensity in the region. */
  void SetMeanRegionIntensity(VecDblType averageRegionIntensity);
  VecDblType GetMeanRegionIntensity();

protected: 
  SegmentationRegion();
  ~SegmentationRegion();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SegmentationRegion(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  unsigned int    m_RegionLabel;
  unsigned int    m_RegionArea;
  unsigned int    m_UniqueLabel; 
  VecDblType      m_MeanVec; 

}; // class SegmentationRegion


} // namespace itk

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSegmentationRegion.txx"
#endif
*/



#endif




