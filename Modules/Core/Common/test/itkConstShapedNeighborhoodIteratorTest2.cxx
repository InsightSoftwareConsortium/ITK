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

#include "itkNeighborhoodIteratorTestCommon.hxx"
#include "itkConstShapedNeighborhoodIterator.h"

//
// this is kind of a duff test, in that it doesn't fail w/the old code
// at runtime, it won't compile at all.  But it does at least do
// coverage of the newly exposed methods.
template <typename ImageType>
class MyDerivedCSNI : public itk::ConstShapedNeighborhoodIterator<ImageType>
{
public:
  typedef typename itk::ConstShapedNeighborhoodIterator<ImageType> Superclass;
  typedef typename Superclass::SizeType                            SizeType;
  typedef typename Superclass::IndexType                           IndexType;
  typedef typename Superclass::RadiusType                          RadiusType;
  typedef typename Superclass::RegionType                          RegionType;

  void TestNewExposedProtectedMembers();
  MyDerivedCSNI(const SizeType & radius,
                const ImageType *ptr,
                const RegionType & region):
    Superclass (radius, const_cast< ImageType * >( ptr ), region)
    {
    }
};

template <typename ImageType>
void
MyDerivedCSNI<ImageType>
::TestNewExposedProtectedMembers()
{
  bool needToUseBoundaryCondition(this->GetNeedToUseBoundaryCondition());
  this->NeedToUseBoundaryConditionOn();
  this->NeedToUseBoundaryConditionOff();
  this->SetNeedToUseBoundaryCondition(needToUseBoundaryCondition);
}

int itkConstShapedNeighborhoodIteratorTest2(int, char* [] )
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  MyDerivedCSNI<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  // radius of the iterator
  MyDerivedCSNI<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  // region over which the iterator is defined
  MyDerivedCSNI<TestImageType>::RegionType reg;
  MyDerivedCSNI<TestImageType>::SizeType sz;
  MyDerivedCSNI<TestImageType>::IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;  idx[3] = 1;
  sz[0] = sz[1] = 10; sz[2] = 5; sz[3] = 1;
  reg.SetIndex(idx); reg.SetSize(sz);

  // initialize an iterator
  println("Creating ConstShapedNeighborhoodIterator");
  MyDerivedCSNI<TestImageType> it(radius, img, reg);
  it.Print(std::cout);
  it.TestNewExposedProtectedMembers();
  return 0;
}
