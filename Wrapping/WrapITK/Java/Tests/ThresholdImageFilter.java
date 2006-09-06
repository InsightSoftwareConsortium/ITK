/**
 *  Example on the use of the ThresholdImageFilter
 *
 */

import InsightToolkit.*;

public class ThresholdImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("ThresholdImageFilter Example");

    itkImageFileReaderIUS2_Pointer reader = itkImageFileReaderIUS2.itkImageFileReaderIUS2_New();
    itkImageFileWriterIUS2_Pointer writer = itkImageFileWriterIUS2.itkImageFileWriterIUS2_New();

    itkThresholdImageFilterIUS2_Pointer filter = itkThresholdImageFilterIUS2.itkThresholdImageFilterIUS2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    filter.SetOutsideValue( Integer.parseInt( argv[2] )  );
    filter.ThresholdAbove(  Integer.parseInt( argv[3] )  );

    writer.Update();
  }

}


