API Reference: shum_string_conv
-------------------------------

Foreword: Fortran Vs. C String Representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This module deals with the differences in how character/string data is 
represented between the two languages.  Fortran ``CHARACTER`` variables are
fixed length whilst C ``char`` variables can be any length (with a terminating 
``NULL`` character signifying the end of the string).

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_string_conv_version``
''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_string_conv_version_mod``

    **Syntax**
        ``version = get_shum_string_conv_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_strlen``
'''''''''''''''''

This function returns the length of a C type string (in Fortran this is 
represented by a 1-dimensional array with the ``C_CHAR`` kind from 
``ISO_C_BINDING`` and comprised of single (``LEN=1``) ``CHARACTER`` values).  
The function calls through the the C intrinsic ``strlen`` directly, so it will
return the length up to the null-character (which terminates strings in C).

    **Available via module**
        ``f_shum_string_conv_mod``

    **Syntax**
        ``length = f_shum_stlen(c_string)``

    **Inputs**
        ``c_string (C_CHAR CHARACTER array)``
            The C type string object (see above for explanation).
    
    **Return Value**
        ``length (64-bit INTEGER)``
            The length of the input string object up to the null character.

    **Notes**
        The C string object provided as input should contain the null 
        character (if returned by a call to C code it should do) otherwise
        the returned length will be incorrect.

``f_shum_f2c_string``
'''''''''''''''''''''

This function converts between a Fortran type string (a ``CHARACTER`` variable)
and a C type string (see the description of ``f_shum_strlen`` for details).

    **Available via module**
        ``f_shum_string_conv_mod``

    **Syntax**
        ``c_string = f_shum_f2c_string(f_string)``

    **Inputs**
        ``f_string (CHARACTER)``
            Any normal Fortran string.

    **Return Value**
        ``c_string (C_CHAR CHARACTER array)``
            A C type string object (see above for explanation).

    **Notes**
        The returned C string will be terminated by the null character, and as
        a result will always be 1 element longer than the Fortran variable, to
        avoid the final character position being overwritten.


``f_shum_c2f_string``
'''''''''''''''''''''

This function converts between a C type string (see the description of 
``f_shum_strlen`` for details).

    **Available via module**
        ``f_shum_string_conv_mod``

    **Syntax**
        ``f_string = f_shum_c2f_string(c_obj [, c_string_len])``

    **Inputs**
        ``c_obj (C_CHAR CHARACTER array or C_PTR)``
            The C type string to convert; this can be either a C type string 
            object as discussed above, or a C pointer to a C string object.
        ``c_string_len (64-bit INTEGER)``
            This gives the length of ``c_obj`` (and ``f_string``).  If 
            ``c_obj`` is a C string object  (i.e. *not* a pointer) and 
            ``f_string`` is ``ALLOCATABLE`` this argument may be omitted; 
            ``f_string`` will be allocated to fit the contents of the C string 
            up to the null character.

    **Return Value**
        ``f_string (CHARACTER)``
            A Fortran character variable. If ``c_string_len`` is provided it
            must be that length, otherwise it must be ``ALLOCATABLE`` and will
            be allocated to the length required to hold the C string contents.   

C Functions
%%%%%%%%%%%

There are no C interfaces for the main functions in this library since they 
all relate to treatment of C like string objects in *Fortran*.  An interface
does exist for the function that returns the Shumlib version number, as this is
automatically added to all Shumlib libraries.

