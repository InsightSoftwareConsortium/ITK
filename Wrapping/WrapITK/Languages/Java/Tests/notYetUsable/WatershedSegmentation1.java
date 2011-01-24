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
 *  Example on the use of the WatershedImageFilter
 *
 */

import InsightToolkit.*;

public class WatershedSegmentation1
{
  public static void main( String argv[] )
  {
    System.out.println("WatershedSegmentation1 Example");

    itkImageFileReaderF2_Pointer reader = itkImageFileReaderF2.itkImageFileReaderF2_New();
    reader.SetFileName( argv[0] );

    itkGradientAnisotropicDiffusionImageFilterF2F2_Pointer diffusion =
      itkGradientAnisotropicDiffusionImageFilterF2F2.itkGradientAnisotropicDiffusionImageFilterF2F2_New();

    diffusion.SetInput( reader.GetOutput() );
    diffusion.SetTimeStep( 0.0625 );
    diffusion.SetConductanceParameter( 9.0 );
    diffusion.SetNumberOfIterations(  5 );

    itkGradientMagnitudeImageFilterF2F2_Pointer gradient =
      itkGradientMagnitudeImageFilterF2F2.itkGradientMagnitudeImageFilterF2F2_New();

    gradient.SetInput(diffusion.GetOutput());

    itkWatershedImageFilterF2_Pointer watershed =
      itkWatershedImageFilterF2.itkWatershedImageFilterF2_New();

    watershed.SetInput( gradient.GetOutput() );
    watershed.SetThreshold( 0.01 );
    watershed.SetLevel( 0.2 );

    itkImageFileWriterUL2_Pointer writer = itkImageFileWriterUL2.itkImageFileWriterUL2_New();
    writer.SetFileName( argv[1] );
    writer.SetInput( watershed.GetOutput()  );
    writer.Update();

  }

}
