#!/usr/bin/python3

from escdf.template import EscdfTemplate
from escdf.specs import EscdfSpecs


#
# Fortran interface
#

# Fortran interface default structure
f03_interface_default = """\
interface
    subroutine escdf_@%group%@_@%action%@_@%name%@(@%params_list%@)
        use iso_c_binding
        implicit none
        @%params_desc%@
    end subroutine escdf_@%group%@_@%action%@_@%name%@
end interface"""


class EscdfFortranInterface(object):

    def __init__(self, specs, template=None):
        """Generate a Fortran interface for a variable from a dictionary"""

        # Init
        self.specs = specs
        if ( template ):
            self.template = EscdfTemplate(template)
        else:
            self.template = EscdfTemplate(f03_interface_default)

        # Hard-coded specs -> Fortran type conversion (for now) 
        self.f03_type = {
            "bool":"logical",
            "char":"character(len=*)",
            "double":"double precision",
            "float":"real",
            "int":"integer",
            "long_int":"integer",
            "short_int":"integer",
            "unsigned_long":"integer",
            "unsigned_int":"integer",
            "unsigned_short":"integer"}

        # Hard-coded argument intents (for now)
        self.f03_intent = {"read":"inout", "write":"in"}

        # Check consistency of specs
        required_fields = ["action", "group", "name", "object", "type"]
        errs = [item for item in required_fields if item not in specs]
        if ( len(errs) > 0 ):
            raise KeyError("missing required fields: %s" % errs)

        # Generate Fortran source code
        f03_specs = {
            "name":specs["name"],
            "group":specs["group"],
            "action":specs["action"]}
        params_list = [self.specs["group"], self.specs["name"]]
        f03_specs["params_list"] = ", ".join(params_list)
        f03_specs["params_desc"] = "type(c_ptr), intent(%s) :: %s\n" % \
            (self.f03_intent[specs["action"]], specs["group"])
        if ( specs["object"] == "scalar" ):
            param_intent = "intent(%s)" % self.f03_intent[specs["action"]]
        else:
            param_intent = "pointer"
        f03_specs["params_desc"] += "%s, %s :: %s" % \
            (self.f03_type[specs["type"]], param_intent, specs["name"])
        f03_text = self.template.substitute(f03_specs)
        self.f03_text = self.template.wrap_fortran(f03_text)


    def __str__(self):

        return self.f03_text


                    ########################################

#
# Fortran module
#

# Fortran module default structure
f03_mod_default = """\
module fescdf_@%group%@

    use iso_c_binding

    implicit none

    ! TODO: ensure line continuations are < 39
    public :: &
        fescdf_@%group%@_t, &
        fescdf_@%group%@_new, &
        fescdf_@%group%@_read_metadata, &
        fescdf_@%group%@_write_metadata, &
        fescdf_@%group%@_free, &
        fescdf_@%group%@_set_file
    @%public_objects%@

    private :: &
        c_to_f_string, &
        c_to_f_string_ptr, &
        f_to_c_string

    type :: fescdf_@%group%@_t
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type fescdf_@%group%@_t

    interface
        subroutine escdf_@%group%@_new(@%group%@), bind(c)
            import
            type(c_ptr) :: @%group%@
        end subroutine escdf_@%group%@_new
    end interface
    interface
        subroutine escdf_@%group%@_read_metadata(@%group%@), bind(c)
            import
            type(c_ptr) :: @%group%@
        end subroutine escdf_@%group%@_read_metadata
    end interface
    interface
        subroutine escdf_@%group%@_write_metadata(@%group%@), bind(c)
            import
            type(c_ptr) :: @%group%@
        end subroutine escdf_@%group%@_write_metadata
    end interface
    interface
        subroutine escdf_@%group%@_free(@%group%@), bind(c)
            import
            type(c_ptr) :: @%group%@
        end subroutine escdf_@%group%@_free
    end interface

    @%interfaces%@

contains

    subroutine fescdf_@%group%@_new(@%group%@, path, mode)
        type(fescdf_@%group%@_t), intent(inout) :: @%group%@
        character(kind=c_char, len=*), intent(in) :: path
        character(kind=c_char, len=*), intent(in) :: mode

        call escdf_@%group%@_new(@%group%@%ptr, path, mode)

    end subroutine fescdf_@%group%@_new

    subroutine fescdf_@%group%@_read_metadata(@%group%@)
        type(fescdf_@%group%@_t), intent(inout) :: @%group%@

        call escdf_@%group%@_read_metadata(@%group%@%ptr)

    end subroutine fescdf_@%group%@_read_metadata

    subroutine fescdf_@%group%@_write_metadata(@%group%@)
        type(fescdf_@%group%@_t), intent(in) :: @%group%@

        call escdf_@%group%@_write_metadata(@%group%@%ptr)

    end subroutine fescdf_@%group%@_write_metadata

    subroutine fescdf_@%group%@_free(@%group%@)
        type(fescdf_@%group%@_t), intent(inout) :: @%group%@

        call escdf_@%group%@_free(@%group%@%ptr)
        @%group%@%ptr = C_NULL_PTR

    end subroutine fescdf_@%group%@_free

    @%wrappers%@

                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Helper functions to convert between C and Fortran strings
    ! Based on the routines by Joseph M. Krahn

    subroutine c_to_f_string(c_string, f_string)

        character(kind=c_char, len=1), intent(in) :: c_string(*)
        character(len=*), intent(out) :: f_string

        integer :: i

        i = 1
        do while(c_string(i) /= C_NULL_CHAR .and. i <= len(f_string))
            f_string(i:i) = c_string(i)
            i = i + 1
        end do
        if (i < len(f_string)) f_string(i:) = ' '

    end subroutine c_to_f_string

    subroutine c_to_f_string_ptr(c_string, f_string)

        type(c_ptr), intent(in) :: c_string
        character(len=*), intent(out) :: f_string

        character(len=1, kind=c_char), pointer :: p_chars(:)
        integer :: i

        if (.not. c_associated(c_string)) then
            f_string = ' '
        else
            call c_f_pointer(c_string, p_chars, [huge(0)])
            i = 1
            do while(p_chars(i) /= C_NULL_CHAR .and. i <= len(f_string))
              f_string(i:i) = p_chars(i)
              i = i + 1
            end do
            if (i < len(f_string)) f_string(i:) = ' '
        end if

    end subroutine c_to_f_string_ptr

    function f_to_c_string(f_string) result(c_string)

        character(len=*), intent(in) :: f_string
        character(kind=c_char, len=1) :: c_string(len_trim(f_string)+1)
            
        integer :: i, strlen

        strlen = len_trim(f_string)

        forall (i=1:strlen)
            c_string(i) = f_string(i:i)
        end forall
        c_string(strlen+1) = C_NULL_CHAR

    end function f_to_c_string

end module fescdf_@%group%@
"""


class EscdfFortranModule(object):

    def __init__(self, group, yaml_doc, template=None):

        # Init
        self.group = group
        self.specs = EscdfSpecs(self.group, yaml_doc)
        if ( template ):
            self.template = EscdfTemplate(template)
        else:
            self.template = EscdfTemplate(f03_mod_default)

        # Build Fortran interfaces
        f03_interfaces = []
        for elem in self.specs.get_elements():
            spec = self.specs.get_spec(elem)
            for action in ["read", "write"]:
                spec["action"] = action
                f03_interfaces.append("%s" % EscdfFortranInterface(spec))

        # Build Fortran wrappers
        f03_wrappers = []

        # Substitute patterns
        self.patterns = {}
        self.patterns["group"] = self.group
        self.patterns["public_objects"] = "public :: &\n    " + \
            ", &\n    ".join(self.specs.get_all_functions())
        self.patterns["interfaces"] = "\n".join(f03_interfaces)
        self.f03_module = self.template.substitute(self.patterns)


    def __str__(self):

        return self.f03_module

