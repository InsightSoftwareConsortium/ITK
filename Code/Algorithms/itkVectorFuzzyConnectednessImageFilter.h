/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorFuzzyConnectednessImageFilter.h
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
#ifndef __itkVectorFuzzyConnectednessImageFilter_h
#define __itkVectorFuzzyConnectednessImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkVector.h"
#include "itkMatrix.h"
#include <vector>
#include <list>

namespace itk{

/** \class VectorFuzzyConnectednessImageFilter
 * 
 * 
 */
template <class TInputImage, class TOutputImage>
class VectorFuzzyConnectednessImageFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef VectorFuzzyConnectednessImageFilter       Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorFuzzyConnectednessImageFilter,ImageToImageFilter);

  /** Extract the image and vector types from the template parameters. */
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename InputPixelType::VectorType InputVectorType;

  /** Extract the image and vector dimension from the template parameters. */
  enum {ImageDimension = TInputImage::ImageDimension };
  enum {VectorDimension = InputPixelType::VectorDimension};

  /** Vector and matrix related typedefs. */
  typedef   itk::Matrix<double,VectorDimension,VectorDimension>               MatrixType;
  typedef   itk::Vector<int,VectorDimension>                    VDVector;
  typedef   itk::Vector<int,ImageDimension>                     IDVector;

  /** Convenient typedefs. */
  typedef   TInputImage                           InputImageType;
  typedef   TOutputImage                          OutputImageType;
  typedef   Image <unsigned short,ImageDimension> UShortImage;
  typedef   typename TInputImage::IndexType       IndexType;
  typedef   typename TInputImage::SizeType        SizeType;
  typedef   typename TOutputImage::RegionType     RegionType;
  typedef   std::list<IndexType>                  ListType;
  typedef   std::vector<IDVector>                 OffsetType;
  typedef   std::vector<float>                    FloatType;
  
  /** Set/Get the object number be segmented in the input image. */
  itkSetMacro(Objects, int);
  itkGetMacro(Objects, int);

  /** Set/Get the selected object number to be segmented in the input image. */
  itkSetMacro(SelectedObject, int);
  itkGetMacro(SelectedObject, int);  

  /** Setting the covariance matrix for specified object: */
  void SetObjectsMatrix(const MatrixType object_max,const int object_num);

  /** Setting the seed points for specified object. */
  void SetObjectsSeed( const IndexType &seed, const int object_num);

  /** Setting the seed points for specified object. */
  void SetObjectsMean(const VDVector, const int object_num);

  /** Allocate the variate in terms of the number of Objects */
  void Initialization();

protected:
  VectorFuzzyConnectednessImageFilter();
  ~VectorFuzzyConnectednessImageFilter();

  /** Standard pipeline method. */
  void GenerateData();

private:
  SizeType                       m_Size;
  OffsetType                     *m_SpherePointsLoc;
  int                            *m_SpherePointsNum;

  double                         m_Mask[3][3];
  double                         m_MaskTotal;
  VDVector                       m_HomoMaxDiff;
  VDVector                       m_FeaturesThreshold;
  VDVector                       m_PowerValue;
  
  int                            m_Objects;
  int                            m_SelectedObject;

  MatrixType                     *m_ObjectsCovMatrix;
  VDVector                       *m_ObjectsMean;

  VDVector                       *m_ObjectsMaxDiff;
  FloatType                      *m_ObjectsMap;
  ListType                       *m_ObjectsSeed;

  std::vector<float>             m_HomogeneityMap;
  std::vector<float>             m_ScaleMap;
  std::vector<char>              m_ScaleArray;
  std::vector<double>            m_Material;

  typename InputImageType::Pointer   m_InputImage;
  typename InputImageType::Pointer   m_FilterImage;
  typename UShortImage::Pointer      m_ObjectFuzzyScene;
  typename UShortImage::Pointer      m_BackgroundFuzzyScene;
  typename OutputImageType::Pointer  m_SegmentObject; 

  std::vector<unsigned short>  m_Xaffinity;
  std::vector<unsigned short>  m_Yaffinity;
  std::vector<unsigned short>  m_Zaffinity;

  void ScalePrepare();
  void Compute_LookupTable();
  void Compute_Scale();
  void Compute_Filter();
  void Compute_Affinity();
  void FastTracking(int);
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorFuzzyConnectednessImageFilter.txx"
#endif

#endif
