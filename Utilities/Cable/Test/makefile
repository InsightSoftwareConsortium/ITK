CXX = /home/kingb/Programs/MyGCC/inst/bin/g++
INSIGHT_HOME = /home/kingb/Insight/Insight
INSIGHT_BUILD = /home/kingb/Insight/Build
INSIGHT_INCLUDES = -I$(INSIGHT_HOME)/Code/Common \
		   -I$(INSIGHT_HOME)/Code/Insight3DParty/vxl \
                   -I$(INSIGHT_HOME) \
		   -I$(INSIGHT_BUILD)
WRAP_HOME = /home/kingb/Programs/WrapPrototype
WRAP_INCLUDES = -I$(WRAP_HOME)

Vect3.cxx: Vect3.xml Vect3_config.xml
	../generateWrappers Vect3_config.xml -tcl

Vect3.xml: vect3Test.C vect3.h
	$(CXX) -fxml -fsyntax-only vect3Test.C >Vect3.xml

vnl_vector_double_wrap.cxx: vnl_vector+double-.xml vnl_vector+double-.xml
	../generateWrappers vnl_vector+double-config.xml -tcl

Tcl/itkTest_wrap.cxx: itkTest.xml itkTest_config.xml
	../generateWrappers itkTest_config.xml -tcl

itkTest.xml:
	$(CXX) -fxml -fsyntax-only $(INSIGHT_INCLUDES) $(INSIGHT_HOME)/Code/Common/itkDirectory.cxx > itkTest.xml

vnl_vector+double-.xml:
	$(CXX) -fxml -fsyntax-only $(INSIGHT_INCLUDES) $(INSIGHT_HOME)/Code/Insight3DParty/vxl/vnl/Templates/vnl_vector+double-.cxx > vnl_vector+double-.xml

clean:
	rm -f Vect3.cxx Vect3.xml itkTest.xml

