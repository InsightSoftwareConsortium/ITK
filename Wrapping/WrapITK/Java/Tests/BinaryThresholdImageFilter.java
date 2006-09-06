/**
 *  Example on the use of the BinaryThresholdImageFilter
 *
 */

import InsightToolkit.*;

public class BinaryThresholdImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("BinaryThresholdImageFilter Example");

    itkImageFileReaderIUS2_Pointer reader = itkImageFileReaderIUS2.itkImageFileReaderIUS2_New();
    itkImageFileWriterIUS2_Pointer writer = itkImageFileWriterIUS2.itkImageFileWriterIUS2_New();

    itkBinaryThresholdImageFilterIUS2IUS2_Pointer filter = itkBinaryThresholdImageFilterIUS2IUS2.itkBinaryThresholdImageFilterIUS2IUS2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    filter.SetLowerThreshold(  Integer.parseInt( argv[2] )  );
    filter.SetUpperThreshold(  Integer.parseInt( argv[3] )  );

    filter.SetOutsideValue( Integer.parseInt( argv[4] )  );
    filter.SetInsideValue(  Integer.parseInt( argv[5] )  );

    writer.Update();
  }

}


