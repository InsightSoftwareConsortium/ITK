/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkCastSpatialObjectFilter_h
#define itkCastSpatialObjectFilter_h

#include <list>

#include <itkSpatialObject.h>

#include <itkArrowSpatialObject.h>
#include <itkBlobSpatialObject.h>
#include <itkBoxSpatialObject.h>
#include <itkContourSpatialObject.h>
#include <itkEllipseSpatialObject.h>
#include <itkGaussianSpatialObject.h>
#include <itkGroupSpatialObject.h>
#include <itkImageMaskSpatialObject.h>
#include <itkImageSpatialObject.h>
#include <itkLandmarkSpatialObject.h>
#include <itkLineSpatialObject.h>
#include <itkPointBasedSpatialObject.h>
#include <itkPolygonSpatialObject.h>
#include <itkSurfaceSpatialObject.h>
#include <itkTubeSpatialObject.h>

namespace itk
{

/** \class CastSpatialObjectFilter
 * \brief This filter casts one spatialobject to another, when the class
 * hierarchy supports it (e.g., Tube to PointBased).
 * Particularly useful in Python where casting objects without public
 * contructors (e.g., objects managed by smartpointers) is problematic.
 * \ingroup ITKSpatialObjects
 */

template <unsigned int ObjectDimension>
class ITK_TEMPLATE_EXPORT CastSpatialObjectFilter : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CastSpatialObjectFilter);

  /** Standard class typedefs. */
  using Self = CastSpatialObjectFilter;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  itkTypeMacro(CastSpatialObjectFilter, Object);

  using InputSpatialObjectType = SpatialObject<ObjectDimension>;

  using InputChildrenListType = typename InputSpatialObjectType::ChildrenListType;

  itkSetObjectMacro(Input, InputSpatialObjectType);
  itkGetConstObjectMacro(Input, InputSpatialObjectType);

  template <class OutputSpatialObjectT>
  std::list<typename OutputSpatialObjectT::Pointer> *
  GetOutput() const
  {
    typedef OutputSpatialObjectT                       OutObjectType;
    typedef std::list<typename OutObjectType::Pointer> OutListType;

    auto * outputList = new OutListType;

    auto * obj = dynamic_cast<OutObjectType *>(m_Input.GetPointer());
    if (obj != nullptr)
    {
      typename OutObjectType::Pointer sObj = OutObjectType::New();
      sObj = obj;
      outputList->push_back(sObj);
    }
    InputChildrenListType * inputList = m_Input->GetChildren(9999);
    auto                    it = inputList->begin();
    while (it != inputList->end())
    {
      obj = dynamic_cast<OutObjectType *>(it->GetPointer());
      if (obj != nullptr)
      {
        typename OutObjectType::Pointer sObj = OutObjectType::New();
        sObj = obj;
        outputList->push_back(sObj);
      }
      ++it;
    }

    delete inputList;

    return outputList;
  }

  using ArrowPointer = typename itk::ArrowSpatialObject<ObjectDimension>::Pointer;
  std::list<ArrowPointer> *
  GetArrows() const
  {
    return this->GetOutput<itk::ArrowSpatialObject<ObjectDimension>>();
  }

  using BlobPointer = typename itk::BlobSpatialObject<ObjectDimension>::Pointer;
  std::list<BlobPointer> *
  GetBlobs() const
  {
    return this->GetOutput<itk::BlobSpatialObject<ObjectDimension>>();
  }

  using BoxPointer = typename itk::BoxSpatialObject<ObjectDimension>::Pointer;
  std::list<BoxPointer> *
  GetBoxes() const
  {
    return this->GetOutput<itk::BoxSpatialObject<ObjectDimension>>();
  }

  using ContourPointer = typename itk::ContourSpatialObject<ObjectDimension>::Pointer;
  std::list<ContourPointer> *
  GetContours() const
  {
    return this->GetOutput<itk::ContourSpatialObject<ObjectDimension>>();
  }

  using EllipsePointer = typename itk::EllipseSpatialObject<ObjectDimension>::Pointer;
  std::list<EllipsePointer> *
  GetEllipses() const
  {
    return this->GetOutput<itk::EllipseSpatialObject<ObjectDimension>>();
  }

  using GaussianPointer = typename itk::GaussianSpatialObject<ObjectDimension>::Pointer;
  std::list<GaussianPointer> *
  GetGaussians() const
  {
    return this->GetOutput<itk::GaussianSpatialObject<ObjectDimension>>();
  }

  using GroupPointer = typename itk::GroupSpatialObject<ObjectDimension>::Pointer;
  std::list<GroupPointer> *
  GetGroups() const
  {
    return this->GetOutput<itk::GroupSpatialObject<ObjectDimension>>();
  }

  using ImageMaskPointer = typename itk::ImageMaskSpatialObject<ObjectDimension>::Pointer;
  std::list<ImageMaskPointer> *
  GetImageMasks() const
  {
    return this->GetOutput<itk::ImageMaskSpatialObject<ObjectDimension>>();
  }

  using ImagePointer = typename itk::ImageSpatialObject<ObjectDimension>::Pointer;
  std::list<ImagePointer> *
  GetImages() const
  {
    return this->GetOutput<itk::ImageSpatialObject<ObjectDimension>>();
  }

  using LandmarkPointer = typename itk::LandmarkSpatialObject<ObjectDimension>::Pointer;
  std::list<LandmarkPointer> *
  GetLandmarks() const
  {
    return this->GetOutput<itk::LandmarkSpatialObject<ObjectDimension>>();
  }

  using LinePointer = typename itk::LineSpatialObject<ObjectDimension>::Pointer;
  std::list<LinePointer> *
  GetLines() const
  {
    return this->GetOutput<itk::LineSpatialObject<ObjectDimension>>();
  }

  using PointBasedPointer = typename itk::PointBasedSpatialObject<ObjectDimension>::Pointer;
  std::list<PointBasedPointer> *
  GetPointBased() const
  {
    return this->GetOutput<itk::PointBasedSpatialObject<ObjectDimension>>();
  }

  using PolygonPointer = typename itk::PolygonSpatialObject<ObjectDimension>::Pointer;
  std::list<PolygonPointer> *
  GetPolygons() const
  {
    return this->GetOutput<itk::PolygonSpatialObject<ObjectDimension>>();
  }

  using SpatialObjectPointer = typename itk::SpatialObject<ObjectDimension>::Pointer;
  std::list<SpatialObjectPointer> *
  GetSpatialObjects() const
  {
    return this->GetOutput<itk::SpatialObject<ObjectDimension>>();
  }

  using SurfacePointer = typename itk::SurfaceSpatialObject<ObjectDimension>::Pointer;
  std::list<SurfacePointer> *
  GetSurfaces() const
  {
    return this->GetOutput<itk::SurfaceSpatialObject<ObjectDimension>>();
  }

  using TubePointer = typename itk::TubeSpatialObject<ObjectDimension>::Pointer;
  std::list<TubePointer> *
  GetTubes() const
  {
    return this->GetOutput<itk::TubeSpatialObject<ObjectDimension>>();
  }

protected:
  CastSpatialObjectFilter();
  ~CastSpatialObjectFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;


private:
  typename InputSpatialObjectType::Pointer m_Input;

}; // End class CastSpatialObjectFilter

} // End namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCastSpatialObjectFilter.hxx"
#endif

#endif // End !defined( itkCastSpatialObjectFilter_h )
