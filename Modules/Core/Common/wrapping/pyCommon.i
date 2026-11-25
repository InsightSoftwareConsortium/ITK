%define DECL_PYTHON_FORCE_SNAKE_CASE_CLASS(class_name)

    %extend class_name {
        %pythoncode %{
            def __internal_call__(self):
                """Create an object, update with the inputs and
                attributes, and return the result.

                The syntax is the same as the one used in New().
                Update() is ran once the input are changed, and
                the current output.
                """
                self.Update()
                return self.GetOutput()
            %}
    }

%enddef
