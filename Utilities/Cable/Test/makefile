CXX = /home/kingb2/Programs/MyGCC/inst/bin/g++
INSIGHT_INCLUDES = -I/home/kingb2/Insight/Code/Common \
		   -I/home/kingb2/Insight/Insight3DParty/vxl \
                   -I/home/kingb2/Insight


Vect3.cxx: Vect3.xml Vect3_config.xml
	../generateWrappers Vect3_config.xml -tcl

Vect3.xml: vect3Test.C vect3.h
	$(CXX) -fxml -o junk -c vect3Test.C >Vect3.xml; rm junk


itkDirectory_wrap.cxx: itkDirectory.xml itkDirectory_config.xml
	../generateWrappers itkDirectory_config.xml -tcl

itkDirectory.xml:
	$(CXX) -fxml $(INSIGHT_INCLUDES) -o junk -c /home/kingb2/Insight/Code/Common/itkDirectory.cxx > itkDirectory.xml ; rm junk

clean:
	rm -f Vect3.cxx Vect3.xml itkDirectory_out.cxx itkDirectory.xml

