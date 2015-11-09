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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkLabelImageToStatisticsLabelMapFilter.h"
#include "itkConvertLabelMapFilter.h"
#include "itkTestingMacros.h"
#include "itkSimpleFilterWatcher.h"

int itkConvertLabelMapFilterTest2(int argc, char * argv[])
{
  if( argc != 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputLabelImage outputLabelImage";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;

  typedef unsigned char                         InputPixelType;
  typedef itk::Image< InputPixelType, dim >     InputImageType;

  typedef unsigned short                        OutputPixelType;
  typedef itk::Image< OutputPixelType, dim >    OutputImageType;

  typedef itk::ImageFileReader< InputImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //////////////////////////////////////////////////////////////////////////////////
  // test ShapeLabelMap conversion: unsigned char type -> unsigned short type
  //////////////////////////////////////////////////////////////////////////////////
  // convert label image to ShapeLabelMap
  typedef itk::LabelImageToShapeLabelMapFilter< InputImageType > L2SType;
  L2SType::Pointer l2s = L2SType::New();
  l2s->SetInput( reader->GetOutput() );
  l2s->Update();
  // convert ShapeLabelMap from unsigned char type to unsigned short type
  typedef L2SType::OutputImageType         InputShapeLabelMapType;
  typedef itk::LabelMap<itk::ShapeLabelObject<unsigned short, dim> >   OutputShapeLabelMapType;
  typedef itk::ConvertLabelMapFilter< InputShapeLabelMapType, OutputShapeLabelMapType > CastShapeType;
  CastShapeType::Pointer castShape = CastShapeType::New();
  castShape->SetInput( l2s->GetOutput() );
  castShape->Update();
  // verify ShapeLabelMap contains equivalent information after conversion
  bool shapeLabelMapIsKeptSame = true;
  if ( l2s->GetOutput()->GetNumberOfLabelObjects() != castShape->GetOutput()->GetNumberOfLabelObjects() ) // verify two label maps have the same number of label objects
    {
    shapeLabelMapIsKeptSame = false;
    }
  for( InputShapeLabelMapType::ConstIterator it( l2s->GetOutput() );
       ! it.IsAtEnd();
       ++it)
    {
    const InputShapeLabelMapType::LabelObjectType * labelObject = it.GetLabelObject();
    InputShapeLabelMapType::LabelType label = labelObject->GetLabel();

    if ( !castShape->GetOutput()->HasLabel( label ) ) // verify label first
      {
        shapeLabelMapIsKeptSame = false;
      }
    else
      {
        const OutputShapeLabelMapType::LabelObjectType* newLabelObject = castShape->GetOutput()->GetLabelObject( label ); // find output label object with the same label
        // verify voxels are the same
        // if number of pixels are the same, AND all pixels in inputLabelObject are also in outputLabelObject, then pixels are kept the same
        if ( labelObject->Size() != newLabelObject->Size() )
          {
            shapeLabelMapIsKeptSame = false;
          }
        for ( unsigned int pixelId = 0; pixelId < labelObject->Size(); ++pixelId )
          {
          if ( !newLabelObject->HasIndex( labelObject->GetIndex( pixelId ) ) )
            {
              shapeLabelMapIsKeptSame = false;
              break;
            }
          }
        // verify shape information
        if ( newLabelObject->GetBoundingBox() != labelObject->GetBoundingBox() \
             || newLabelObject->GetCentroid() != labelObject->GetCentroid() \
             || newLabelObject->GetElongation() != labelObject->GetElongation() \
             || newLabelObject->GetEquivalentEllipsoidDiameter() != labelObject->GetEquivalentEllipsoidDiameter() \
             || newLabelObject->GetEquivalentSphericalPerimeter() != labelObject->GetEquivalentSphericalPerimeter() \
             || newLabelObject->GetEquivalentSphericalRadius() != labelObject->GetEquivalentSphericalRadius() \
             || newLabelObject->GetFeretDiameter() != labelObject->GetFeretDiameter() \
             || newLabelObject->GetFlatness() != labelObject->GetFlatness() \
             || newLabelObject->GetNumberOfPixels() != labelObject->GetNumberOfPixels() \
             || newLabelObject->GetNumberOfPixelsOnBorder() != labelObject->GetNumberOfPixelsOnBorder() \
             || newLabelObject->GetPerimeter() != labelObject->GetPerimeter() \
             || newLabelObject->GetPerimeterOnBorder() != labelObject->GetPerimeterOnBorder() \
             || newLabelObject->GetPerimeterOnBorderRatio() != labelObject->GetPerimeterOnBorderRatio() \
             || newLabelObject->GetPhysicalSize() != labelObject->GetPhysicalSize() \
             || newLabelObject->GetPrincipalAxes() != labelObject->GetPrincipalAxes() \
             || newLabelObject->GetPrincipalMoments() != labelObject->GetPrincipalMoments() \
             || newLabelObject->GetRoundness() != labelObject->GetRoundness() )
          {
            shapeLabelMapIsKeptSame = false;
          }
      }

    // if one labelObject is not kept the same, then just exit program with failure
    if ( !shapeLabelMapIsKeptSame )
      {
      std::cerr << "shapeLabelMapIsKeptSame is false" << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cerr << "ShapeLabelMap test is finished and passed" << std::endl;

  //////////////////////////////////////////////////////////////////////////////////
  // test StatisticsLabelMap conversion: unsigned char type -> unsigned short type
  //////////////////////////////////////////////////////////////////////////////////
  // convert label image to StatisticsLabelMap
  typedef itk::LabelImageToStatisticsLabelMapFilter< InputImageType, InputImageType > L2StatType;
  L2StatType::Pointer l2stat = L2StatType::New();
  l2stat->SetInput( reader->GetOutput() );
  l2stat->SetFeatureImage( reader->GetOutput() ); // use input label image itself as the feature image
  l2stat->Update();
  // convert StatisticsLabelMap from unsigned char type to unsigned short type
  typedef L2StatType::OutputImageType              InputStatisticsLabelMapType;
  typedef itk::LabelMap<itk::StatisticsLabelObject<unsigned short, dim> >     OutputStatisticsLabelMapType;
  typedef itk::ConvertLabelMapFilter< InputStatisticsLabelMapType, OutputStatisticsLabelMapType > CastStatType;
  CastStatType::Pointer castStatistics = CastStatType::New();
  castStatistics->SetInput( l2stat->GetOutput() );
  castStatistics->Update();
  // verify StatisticsLabelMap contains equivalent information after conversion
  bool statisticsLabelMapIsKeptSame = true;
  if ( l2stat->GetOutput()->GetNumberOfLabelObjects() != castStatistics->GetOutput()->GetNumberOfLabelObjects() ) // verify two label maps have the same number of label objects
    {
    statisticsLabelMapIsKeptSame = false;
    }
  for( InputStatisticsLabelMapType::ConstIterator it( l2stat->GetOutput() );
       ! it.IsAtEnd();
       ++it)
    {
    const InputStatisticsLabelMapType::LabelObjectType * labelObject = it.GetLabelObject();
    InputStatisticsLabelMapType::LabelType label = labelObject->GetLabel();

    if ( !castStatistics->GetOutput()->HasLabel( label ) ) // verify label first
      {
        statisticsLabelMapIsKeptSame = false;
      }
    else
      {
        const OutputStatisticsLabelMapType::LabelObjectType* newLabelObject = castStatistics->GetOutput()->GetLabelObject( label ); // find output label object with the same label
        // verify voxels are the same
        // if number of pixels are the same, AND all pixels in inputLabelObject are also in outputLabelObject, then pixels are kept the same
        if ( labelObject->Size() != newLabelObject->Size() )
          {
            statisticsLabelMapIsKeptSame = false;
          }
        for ( unsigned int pixelId = 0; pixelId < labelObject->Size(); ++pixelId )
          {
          if ( !newLabelObject->HasIndex( labelObject->GetIndex( pixelId ) ) )
            {
              statisticsLabelMapIsKeptSame = false;
              break;
            }
          }
        // verify shape information inherited from ShapeLabelObject
        if ( newLabelObject->GetBoundingBox() != labelObject->GetBoundingBox() \
             || newLabelObject->GetCentroid() != labelObject->GetCentroid() \
             || newLabelObject->GetElongation() != labelObject->GetElongation() \
             || newLabelObject->GetEquivalentEllipsoidDiameter() != labelObject->GetEquivalentEllipsoidDiameter() \
             || newLabelObject->GetEquivalentSphericalPerimeter() != labelObject->GetEquivalentSphericalPerimeter() \
             || newLabelObject->GetEquivalentSphericalRadius() != labelObject->GetEquivalentSphericalRadius() \
             || newLabelObject->GetFeretDiameter() != labelObject->GetFeretDiameter() \
             || newLabelObject->GetFlatness() != labelObject->GetFlatness() \
             || newLabelObject->GetNumberOfPixels() != labelObject->GetNumberOfPixels() \
             || newLabelObject->GetNumberOfPixelsOnBorder() != labelObject->GetNumberOfPixelsOnBorder() \
             || newLabelObject->GetPerimeter() != labelObject->GetPerimeter() \
             || newLabelObject->GetPerimeterOnBorder() != labelObject->GetPerimeterOnBorder() \
             || newLabelObject->GetPerimeterOnBorderRatio() != labelObject->GetPerimeterOnBorderRatio() \
             || newLabelObject->GetPhysicalSize() != labelObject->GetPhysicalSize() \
             || newLabelObject->GetPrincipalAxes() != labelObject->GetPrincipalAxes() \
             || newLabelObject->GetPrincipalMoments() != labelObject->GetPrincipalMoments() \
             || newLabelObject->GetRoundness() != labelObject->GetRoundness() )
          {
            statisticsLabelMapIsKeptSame = false;
          }
        // verify statistics information
        if ( newLabelObject->GetCenterOfGravity() != labelObject->GetCenterOfGravity() \
             || newLabelObject->GetHistogram() != labelObject->GetHistogram() \
             || newLabelObject->GetKurtosis() != labelObject->GetKurtosis() \
             || newLabelObject->GetMaximum() != labelObject->GetMaximum() \
             || newLabelObject->GetMaximumIndex() != labelObject->GetMaximumIndex() \
             || newLabelObject->GetMean() != labelObject->GetMean() \
             || newLabelObject->GetMedian() != labelObject->GetMedian() \
             || newLabelObject->GetMinimum() != labelObject->GetMinimum() \
             || newLabelObject->GetMinimumIndex() != labelObject->GetMinimumIndex() \
             || newLabelObject->GetSkewness() != labelObject->GetSkewness() \
             || newLabelObject->GetStandardDeviation() != labelObject->GetStandardDeviation() \
             || newLabelObject->GetSum() != labelObject->GetSum() \
             || newLabelObject->GetVariance() != labelObject->GetVariance() \
             || newLabelObject->GetWeightedElongation() != labelObject->GetWeightedElongation() \
             || newLabelObject->GetWeightedFlatness() != labelObject->GetWeightedFlatness() \
             || newLabelObject->GetWeightedPrincipalAxes() != labelObject->GetWeightedPrincipalAxes() \
             || newLabelObject->GetWeightedPrincipalMoments() != labelObject->GetWeightedPrincipalMoments() )
          {
            statisticsLabelMapIsKeptSame = false;
          }
      }

    // if one labelObject is not kept the same, then just exit program with failure
    if ( !statisticsLabelMapIsKeptSame )
      {
      std::cerr << "statisticsLabelMapIsKeptSame is false" << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cerr << "statisticsLabelMap test is finished and passed" << std::endl;

  //////////////////////////////////////////////////////////////////////////////////
  // test LabelMap conversion: unsigned char type -> unsigned short type
  //////////////////////////////////////////////////////////////////////////////////
  // convert label image to label map
  typedef itk::LabelImageToLabelMapFilter< InputImageType > L2MType;
  L2MType::Pointer l2m = L2MType::New();
  l2m->SetInput( reader->GetOutput() );

  typedef itk::LabelObject< OutputPixelType, dim >      LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >              LabelMapType;

  // convert label map from unsigned char to unsigned short type
  typedef itk::ConvertLabelMapFilter< L2MType::OutputImageType, LabelMapType >     CastType;
  CastType::Pointer cast = CastType::New();
  cast->SetInput( l2m->GetOutput() );
  itk::SimpleFilterWatcher watcher(cast, "cast");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, OutputImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( cast->GetOutput() );

  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // for visual validation
  std::cout << " ============ original label map ============" << std::endl;
  l2m->GetOutput()->PrintLabelObjects();
  std::cout << " ============ casted label map ============" << std::endl;
  cast->GetOutput()->PrintLabelObjects();

  return EXIT_SUCCESS;
}
