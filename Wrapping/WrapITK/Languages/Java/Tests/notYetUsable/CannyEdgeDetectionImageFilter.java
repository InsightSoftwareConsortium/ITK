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
 *  Example on the use of the CannyEdgeDetectionImageFilter
 *
 */

import InsightToolkit.*;


public class CannyEdgeDetectionImageFilter
{
  public static void main( String argv[] )
  {
    itkImageFileReaderF2_Pointer reader = itkImageFileReaderF2.itkImageFileReaderF2_New();
    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    itkCannyEdgeDetectionImageFilterF2F2_Pointer filter = itkCannyEdgeDetectionImageFilterF2F2.itkCannyEdgeDetectionImageFilterF2F2_New();

    itkRescaleIntensityImageFilterF2UC2_Pointer outputCast = itkRescaleIntensityImageFilterF2UC2.itkRescaleIntensityImageFilterF2UC2_New();

    filter.SetInput( reader.GetOutput() );
    outputCast.SetInput( filter.GetOutput() );
    writer.SetInput( outputCast.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    short outputMinimum = 0;
    short outputMaximum = 0;

    outputCast.SetOutputMinimum( outputMinimum );
    outputCast.SetOutputMaximum( outputMaximum );

    filter.SetVariance(  Float.parseFloat( argv[2] ) );
    filter.SetThreshold( Float.parseFloat( argv[3] ) );

    writer.Update();
  }

}
