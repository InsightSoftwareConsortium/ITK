CXX = /home/hoffman/Insight/GCC_Output/gcc-install/bin/xmlgen

INSIGHT_INCLUDES = -I/home/hoffman/Insight/Code/Common \
		   -I/home/hoffman/Insight/Code/Insight3DParty/vxl \
                   -I/home/hoffman/Insight-gcc \
                   -I/home/hoffman/Insight-gcc/Code/Insight3DParty/vxl \



Vect3.cxx: Vect3.xml Vect3_config.xml
	../generateWrappers Vect3_config.xml -tcl

Vect3.xml: vect3Test.C vect3.h
	$(CXX) -fxml -o junk -c vect3Test.C >Vect3.xml; rm junk


vnl_vector_double_wrap.cxx: vnl_vector+double-.xml vnl_vector+double-.xml
	../generateWrappers vnl_vector+double-config.xml -tcl

itkDirectory_wrap.cxx: itkDirectory.xml itkDirectory_config.xml
	../generateWrappers itkDirectory_config.xml -tcl

itkDirectory.xml:
	$(CXX) -fxml $(INSIGHT_INCLUDES) -o junk -S /home/hoffman/Insight/Code/Common/itkDirectory.cxx > itkDirectory.xml ; rm junk

vnl_vector+double-.xml:
	$(CXX) -fxml $(INSIGHT_INCLUDES) -o junk -S /home/hoffman/Insight/Code/Insight3DParty/vxl/vnl/Templates/vnl_vector+double-.cxx > vnl_vector+double-.xml ; rm junk

itkObject.xml:
	$(CXX) -fxml $(INSIGHT_INCLUDES) -o junk -c /home/hoffman/Insight/Code/Common/itkObject.cxx > itkObject.xml ; rm junk

clean:
	rm -f Vect3.cxx Vect3.xml itkDirectory_out.cxx itkDirectory.xml

