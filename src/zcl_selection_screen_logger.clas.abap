CLASS zcl_selection_screen_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING program_name TYPE syrepid.

    METHODS get_parameter_values_as_text
      RETURNING VALUE(parameter_values_as_text) TYPE string_table.

    METHODS get_parameter_value_as_text
      IMPORTING parameter_name                 TYPE rsscr_name
      RETURNING VALUE(parameter_value_as_text) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES tt_rsscr TYPE standard TABLE OF rsscr WITH default key.
    types ty_text_pool type STANDARD TABLE OF textpool with DEFAULT KEY.

    DATA program_name TYPE syrepid.
    DATA table_sscr  TYPE tt_rsscr.
    DATA log TYPE string_table.
    DATA text_pool TYPE ty_text_pool.

" Constants from the include RSDBCOM2
CONSTANTS: BIT_SSCR_F1_DBSE    TYPE I VALUE 1,  " Datenbankspezifisch
           BIT_SSCR_F1_OBLI    TYPE I VALUE 2,  " OBLIGATORY
           BIT_SSCR_F1_NODI    TYPE I VALUE 3,  " NO-DISPLAY
           BIT_SSCR_F1_LOWC    TYPE I VALUE 4,  " LOWER CASE
           BIT_SSCR_F1_CBOX    TYPE I VALUE 5,  " AS CHECKBOX
           BIT_SSCR_F1_NOIN    TYPE I VALUE 5,  " NO INTERVALS
           BIT_SSCR_F1_NOEX    TYPE I VALUE 6,  " NO-EXTENSION
           BIT_SSCR_F1_IXST    TYPE I VALUE 6,  " AS INDEX STRUCTURE
           BIT_SSCR_F1_RADI    TYPE I VALUE 7,  " RADIOBUTTON
           BIT_SSCR_F1_REDB    TYPE I VALUE 7,  " AS DATABASE SELECTION
           BIT_SSCR_F1_SUBS    TYPE I VALUE 7,  " SUBSCREENBEREICH
           BIT_SSCR_F1_PARM    TYPE I VALUE 8,  " Parameter

           BIT_SSCR_F2_VRLO    TYPE I VALUE 1,  " VALUE-REQUEST FOR LOW
           BIT_SSCR_F2_VRHI    TYPE I VALUE 2,  " VALUE-REQUEST FOR HIGH
           BIT_SSCR_F2_VCHK    TYPE I VALUE 2,  " VALUE CHECK
           BIT_SSCR_F2_HRLO    TYPE I VALUE 3,  " HELP-REQUEST FOR LOW
           BIT_SSCR_F2_HRHI    TYPE I VALUE 4,  " HELP-REQUEST FOR HIGH
           BIT_SSCR_F2_HR      TYPE I VALUE 5,  " AS LISTBOX
           BIT_SSCR_F2_DYN     TYPE I VALUE 6,  " HELP-REQUEST FOR both
           BIT_SSCR_F2_REFR    TYPE I VALUE 7,  " Dynamisches Bezugsfeld
           BIT_SSCR_F2_SIGN    TYPE I VALUE 8.  " Nur Referenz
    METHODS get_select_options_line_as_txt      " DDIC: Vorzeichen
      IMPORTING
        range_line                TYPE any
      RETURNING
        value(SELECT_OPTIONS_LINe_AS_TEXT) TYPE string_table.
ENDCLASS.



CLASS zcl_selection_screen_logger IMPLEMENTATION.
  METHOD constructor.
    me->program_name = program_name.

    LOAD REPORT program_name PART 'SSCR' INTO table_sscr.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text  = 'Load of program &1 not found'(004)
          msgv1 = EXACT #( program_name ).
    ENDIF.

    READ TEXTPOOL program_name INTO text_pool.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text  = 'Text pool of program &1 not found'(007)
          msgv1 = EXACT #( program_name ).
    ENDIF.
  ENDMETHOD.

  METHOD get_parameter_value_as_text.
    DATA line_of_sscr_load           TYPE REF TO rsscr.
    DATA variable_for_dirty_assign   TYPE string.
    DATA select_options_line_as_text TYPE string.

    FIELD-SYMBOLS <parameter_value>        TYPE data.
    FIELD-SYMBOLS <select_options_value>   TYPE ANY TABLE.
    FIELD-SYMBOLS <line_of_select_options> TYPE any.

    READ TABLE table_sscr WITH KEY name = parameter_name REFERENCE INTO line_of_sscr_load.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING text  = 'Parameter &1 not found'(003)
                  msgv1 = CONV #( parameter_name ).
    ENDIF.

    CASE line_of_sscr_load->kind.
      WHEN 'P'.
        variable_for_dirty_assign = |({ program_name }){ parameter_name }|.
        ASSIGN (variable_for_dirty_assign) TO <parameter_value>.
        INSERT |{ <parameter_value> }| INTO TABLE parameter_value_as_text.
      WHEN 'S'.
        variable_for_dirty_assign = |({ program_name }){ parameter_name }[]|.
        ASSIGN (variable_for_dirty_assign) TO <select_options_value>.
        DATA(number_of_include_lines) = 0.
        TRY.
            LOOP AT <select_options_value> ASSIGNING <line_of_select_options>
                 WHERE (`SIGN = 'I'`).
              INSERT LINES OF get_select_options_line_as_txt( range_line = <line_of_select_options> ) INTO TABLE parameter_value_as_text.
              number_of_include_lines = number_of_include_lines + 1.
            ENDLOOP.
            IF lines( <select_options_value> ) > number_of_include_lines.
              IF number_of_include_lines = 0.
                INSERT EXACT #( 'All except:'(001) ) INTO TABLE parameter_value_as_text.
              ELSE.
                INSERT EXACT #( 'Except:'(002) ) INTO TABLE parameter_value_as_text.
              ENDIF.
              LOOP AT <select_options_value> ASSIGNING <line_of_select_options>
                   WHERE (`SIGN = 'E'`).
                INSERT LINES OF get_select_options_line_as_txt( range_line = <line_of_select_options> ) INTO TABLE parameter_value_as_text.
              ENDLOOP.
            ENDIF.
          CATCH zcx_selection_screen_logger INTO DATA(error).
            RAISE EXCEPTION TYPE zcx_selection_screen_logger
              EXPORTING previous = error
                        text     = 'Error for parameter &1'(005)
                        msgv1    = EXACT #( parameter_name ).
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD get_parameter_values_as_text.
    DATA table_sscr_display TYPE tt_rsscr.
    DATA line_of_sscr_load  TYPE REF TO rsscr.

    LOOP AT table_sscr REFERENCE INTO line_of_sscr_load.
      GET BIT bit_sscr_f1_nodi OF line_of_sscr_load->flag1 INTO DATA(bit).
      IF bit = 1.
        CONTINUE.
      ENDIF.
      INSERT line_of_sscr_load->* INTO TABLE table_sscr_display.
    ENDLOOP.

    LOOP AT table_sscr_display REFERENCE INTO line_of_sscr_load.
      INSERT LINES OF get_parameter_value_as_text( line_of_sscr_load->name ) INTO TABLE parameter_values_as_text.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_select_options_line_as_txt.
    DATA log_line TYPE string.

    ASSIGN COMPONENT 2 OF STRUCTURE range_line TO FIELD-SYMBOL(<option>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text = 'Parameter RANGE_LINE misses the 2nd component (OPTION)'.
    ENDIF.

    ASSIGN COMPONENT 3 OF STRUCTURE range_line TO FIELD-SYMBOL(<low>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text = 'Parameter RANGE_LINE misses the 3rd component (LOW)'.
    ENDIF.

    ASSIGN COMPONENT 4 OF STRUCTURE range_line TO FIELD-SYMBOL(<high>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text = 'Parameter RANGE_LINE misses the 4th component (HIGH)'.
    ENDIF.

    CASE <option>.
      WHEN 'BT'.
        log_line = |Between { <low> } and { <high> }|.
      WHEN 'CP'.
        log_line = |Like { <low> }|.
      WHEN 'EQ'.
        log_line = |{ <low> }|.
        IF    log_line IS INITIAL
           OR log_line CA '*+#'.
          log_line = |=  { <low> }|.
        ELSE.
          log_line = |   { <low> }|.
        ENDIF.
      WHEN 'GE'.
        log_line = |>= { <low> }|.
      WHEN 'GT'.
        log_line = |>  { <low> }|.
      WHEN 'LE'.
        log_line = |<= { <low> }|.
      WHEN 'LT'.
        log_line = |<  { <low> }|.
      WHEN 'NB'.
        log_line = |Not between { <low> } and { <high> }|.
      WHEN 'NE'.
        log_line = |<> { <low> }|.
      WHEN 'NP'.
        log_line = |Not like { <low> }|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_selection_screen_logger
          EXPORTING
            text  = 'Option &1 is invalid'(006)
            msgv1 = <option>.
    ENDCASE.
    INSERT log_line INTO TABLE select_options_line_as_text.
  ENDMETHOD.
ENDCLASS.
