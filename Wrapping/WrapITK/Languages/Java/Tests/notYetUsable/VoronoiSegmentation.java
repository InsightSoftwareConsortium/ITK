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

/**
 *  Example on the use of the VoronoiSegmentationImageFilter.
 *
 */

import InsightToolkit.*;

public class VoronoiSegmentation
{
  public static void main( String argv[] )
  {
    System.out.println("VoronoiSegmentationImageFilter Example");

    itkImageFileReaderUC2_Pointer readerInput =
      itkImageFileReaderUC2.itkImageFileReaderUC2_New();

    itkImageFileReaderUC2_Pointer readerPrior =
      itkImageFileReaderUC2.itkImageFileReaderUC2_New();

    readerInput.SetFileName( argv[0] );
    readerPrior.SetFileName( argv[1] );

    readerInput.Update();
    readerPrior.Update();

    itkVoronoiSegmentationImageFilterUC2UC2UC2_Pointer filter =
      itkVoronoiSegmentationImageFilterUC2UC2UC2.itkVoronoiSegmentationImageFilterUC2UC2UC2_New();

    filter.SetInput( readerInput.GetOutput() );
    filter.TakeAPrior( readerPrior.GetOutput() );

    filter.SetMeanPercentError( Double.parseDouble( argv[3] ) );
    filter.SetSTDPercentError(  Double.parseDouble( argv[4] ) );

    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    writer.SetInput( filter.GetOutput() );
    writer.SetFileName( argv[2] );
    writer.Update();
  }

}
