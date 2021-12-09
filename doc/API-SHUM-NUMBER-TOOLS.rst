API Reference: shum_number_tools
--------------------------------

There are certain special classes of number which may be encountered when
working with floating point arithmatic. These are "NaN" (not a number),
"Inf" (infinities), and "denormal" (too small to be represented) numbers.

The intrisinc IEEE arithmetic module provides some functions to query these
classes of number, but unfortunatly compiler support accross different
architectures is incomplete.

Even when the IEEE functions are availible, they sometimes do not work on
accelerator architectures. For example, there are known examples of systems
which support ``ieee_is_nan`` correctly on a host CPU, but this does not
function correctly when offloaded to a GPU.

This library therefore provides abstract tests which are portable across
systems even when the IEEE arithmetic module is not availible, and which
also work on accerators.

All of these tests work with both positive and negative variants of inputs.
That is to say if, for example, ``f_shum_is_inf(number)`` is ``.TRUE.``,
then ``f_shum_is_inf(-number)`` is also ``.TRUE.``.

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_number_tools_version``
'''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_number_tools_version_mod``

    **Syntax**
        ``version = get_shum_number_tools_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_is_denormal``
''''''''''''''''''''''

This function tests a given floating-point number to determine if it is denormal.  

    **Available via module**
        ``f_shum_is_denormal_mod``

    **Syntax**
        ``denormal = f_shum_is_denormal(number)``

    **Inputs**
        ``number (REAL)``
            The number of to be checked.

    **Return Value**
        ``denormal (LOGICAL)``
            Returned ``.TRUE.`` if ``number`` is denormal; ``.FALSE.`` otherwise

``f_shum_has_denormal``
'''''''''''''''''''''''

This function tests a given floating-point array to determine if any element of
it is denormal.  

    **Available via module**
        ``f_shum_is_denormal_mod``

    **Syntax**
        ``denormal = f_shum_has_denormal(number_array)``

    **Inputs**
        ``number_array (REAL)``
            The array to be checked.

    **Return Value**
        ``denormal (LOGICAL)``
            Returned ``.TRUE.`` if any element of ``number_array`` is denormal;
            ``.FALSE.`` otherwise

    **Notes**
        ``number_array`` may be any dimension up-to and including 5D.

``f_shum_is_inf``
'''''''''''''''''

This function tests a given floating-point number to determine if it is an 
infinity.  

    **Available via module**
        ``f_shum_is_inf_mod``

    **Syntax**
        ``infinity = f_shum_is_inf(number)``

    **Inputs**
        ``number (REAL)``
            The number of to be checked.

    **Return Value**
        ``infinity (LOGICAL)``
            Returned ``.TRUE.`` if ``number`` is an infinity; ``.FALSE.`` otherwise

``f_shum_has_inf``
''''''''''''''''''

This function tests a given floating-point array to determine if any element of
it is an infinity.  

    **Available via module**
        ``f_shum_has_inf_mod``

    **Syntax**
        ``infinity = f_shum_has_inf(number_array)``

    **Inputs**
        ``number_array (REAL)``
            The array to be checked.

    **Return Value**
        ``infinity (LOGICAL)``
            Returned ``.TRUE.`` if any element of ``number_array`` is an infinity;
            ``.FALSE.`` otherwise

    **Notes**
        ``number_array`` may be any dimension up-to and including 5D.

``f_shum_is_nan``
'''''''''''''''''

This function tests a given floating-point number to determine if it is a NaN.  

    **Available via module**
        ``f_shum_is_nan_mod``

    **Syntax**
        ``nan = f_shum_is_nan(number)``

    **Inputs**
        ``number (REAL)``
            The number of to be checked.

    **Return Value**
        ``nan (LOGICAL)``
            Returned ``.TRUE.`` if ``number`` is a NaN; ``.FALSE.`` otherwise

``f_shum_has_nan``
''''''''''''''''''

This function tests a given floating-point array to determine if any element of
it is a NaN.  

    **Available via module**
        ``f_shum_has_nan_mod``

    **Syntax**
        ``nan = f_shum_has_nan(number_array)``

    **Inputs**
        ``number_array (REAL)``
            The array to be checked.

    **Return Value**
        ``nan (LOGICAL)``
            Returned ``.TRUE.`` if any element of ``number_array`` is a NaN;
            ``.FALSE.`` otherwise

    **Notes**
        ``number_array`` may be any dimension up-to and including 5D.

C Functions
%%%%%%%%%%%

``get_shum_number_tools_version``
'''''''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        ``c_shum_number_tools_version.h``

    **Syntax**
        ``version = get_shum_number_tools_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

