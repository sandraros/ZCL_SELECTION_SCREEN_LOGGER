*&---------------------------------------------------------------------*
*& Report z_selection_screen_logger_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_selection_screen_logger_demo.
TYPES:
  BEGIN OF ts_data_for_select_options,
    land1 TYPE t005-land1,
  END OF ts_data_for_select_options.
DATA data_for_select_options TYPE ts_data_for_select_options.

PARAMETERS p_land1 TYPE t005-land1 DEFAULT 'FR'.
SELECT-OPTIONS s_land1 FOR data_for_select_options-land1 DEFAULT 'F*' SIGN I OPTION CP.
PARAMETERS p_nodisp TYPE t005-land1 NO-DISPLAY.

START-OF-SELECTION.
  DATA(selection_screen_logger) = NEW zcl_selection_screen_logger( program_name = sy-repid ).
  DATA(xx) = selection_screen_logger->get_parameter_values_as_text( ).
  LOOP AT xx REFERENCE INTO DATA(line).
    WRITE / line->*.
  ENDLOOP.
  ASSERT 1 = 1. " Debug helper to set a break-point
