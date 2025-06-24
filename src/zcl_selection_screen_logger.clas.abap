CLASS zcl_selection_screen_logger DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_parameter_value,
        parameter_name   TYPE rsscr_name,
        parameter_label  TYPE c LENGTH 40,
        t_values_as_text TYPE string_table,
      END OF ts_parameter_value.
    TYPES tt_parameter_value TYPE STANDARD TABLE OF ts_parameter_value WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING program_name TYPE syrepid.

    METHODS get_parameter_values_as_table
      RETURNING VALUE(parameter_values) TYPE tt_parameter_value.

    METHODS get_parameter_values_as_text
      RETURNING VALUE(parameter_values_as_text) TYPE string_table.

    METHODS get_parameter_value_as_struct
      IMPORTING parameter_name                 TYPE rsscr_name
      RETURNING VALUE(parameter_value_as_text) TYPE ts_parameter_value.

  PRIVATE SECTION.
    TYPES tt_rsscr     TYPE STANDARD TABLE OF rsscr WITH DEFAULT KEY.
    TYPES ty_text_pool TYPE STANDARD TABLE OF textpool WITH DEFAULT KEY.

    DATA program_name TYPE syrepid.
    DATA table_sscr   TYPE tt_rsscr.
    DATA log          TYPE string_table.
    DATA text_pool    TYPE ty_text_pool.

    " Constants copied from the include RSDBCOM2
    CONSTANTS bit_sscr_f1_dbse TYPE i VALUE 1. " Datenbankspezifisch
    CONSTANTS bit_sscr_f1_obli TYPE i VALUE 2. " OBLIGATORY
    CONSTANTS bit_sscr_f1_nodi TYPE i VALUE 3. " NO-DISPLAY
    CONSTANTS bit_sscr_f1_lowc TYPE i VALUE 4. " LOWER CASE
    CONSTANTS bit_sscr_f1_cbox TYPE i VALUE 5. " AS CHECKBOX
    CONSTANTS bit_sscr_f1_noin TYPE i VALUE 5. " NO INTERVALS
    CONSTANTS bit_sscr_f1_noex TYPE i VALUE 6. " NO-EXTENSION
    CONSTANTS bit_sscr_f1_ixst TYPE i VALUE 6. " AS INDEX STRUCTURE
    CONSTANTS bit_sscr_f1_radi TYPE i VALUE 7. " RADIOBUTTON
    CONSTANTS bit_sscr_f1_redb TYPE i VALUE 7. " AS DATABASE SELECTION
    CONSTANTS bit_sscr_f1_subs TYPE i VALUE 7. " SUBSCREENBEREICH
    CONSTANTS bit_sscr_f1_parm TYPE i VALUE 8. " Parameter

    CONSTANTS bit_sscr_f2_vrlo TYPE i VALUE 1. " VALUE-REQUEST FOR LOW
    CONSTANTS bit_sscr_f2_vrhi TYPE i VALUE 2. " VALUE-REQUEST FOR HIGH
    CONSTANTS bit_sscr_f2_vchk TYPE i VALUE 2. " VALUE CHECK
    CONSTANTS bit_sscr_f2_hrlo TYPE i VALUE 3. " HELP-REQUEST FOR LOW
    CONSTANTS bit_sscr_f2_hrhi TYPE i VALUE 4. " HELP-REQUEST FOR HIGH
    CONSTANTS bit_sscr_f2_hr   TYPE i VALUE 5. " AS LISTBOX
    CONSTANTS bit_sscr_f2_dyn  TYPE i VALUE 6. " HELP-REQUEST FOR both
    CONSTANTS bit_sscr_f2_refr TYPE i VALUE 7. " Dynamisches Bezugsfeld
    CONSTANTS bit_sscr_f2_sign TYPE i VALUE 8. " Nur Referenz

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

  METHOD get_parameter_value_as_struct.
    DATA line_of_sscr_load           TYPE REF TO rsscr.
    DATA variable_for_dirty_assign   TYPE string.
    DATA select_options_line_as_text TYPE string.
    DATA line_of_text_pool           TYPE REF TO textpool.

    FIELD-SYMBOLS <parameter_value>        TYPE data.
    FIELD-SYMBOLS <select_options_value>   TYPE ANY TABLE.
    FIELD-SYMBOLS <line_of_select_options> TYPE any.
    FIELD-SYMBOLS <param_or_sel_opt_value> TYPE any.
    FIELD-SYMBOLS <basis_field>            TYPE data.

    parameter_value_as_text-parameter_name = parameter_name.

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
      WHEN 'S'.
        variable_for_dirty_assign = |({ program_name }){ parameter_name }[]|.
        ASSIGN (variable_for_dirty_assign) TO <select_options_value>.
    ENDCASE.

    READ TABLE text_pool WITH KEY id = 'S' key = parameter_name REFERENCE INTO line_of_text_pool.
    IF sy-subrc = 0.
      IF line_of_text_pool->entry = 'D       .'.
        ASSIGN (variable_for_dirty_assign) TO <param_or_sel_opt_value>.
        CASE line_of_sscr_load->kind.
          WHEN 'P'.
            ASSIGN <param_or_sel_opt_value> TO <basis_field>.
          WHEN 'S'.
            ASSIGN COMPONENT 3 OF STRUCTURE <param_or_sel_opt_value> TO <basis_field>.
        ENDCASE.
        DATA(typedescr) = cl_abap_typedescr=>describe_by_data( <basis_field> ).
        CAST cl_abap_elemdescr( typedescr )->get_ddic_field( EXPORTING  p_langu      = sy-langu
                                                             RECEIVING  p_flddescr   = DATA(dfies)
                                                             EXCEPTIONS not_found    = 1
                                                                        no_ddic_type = 2
                                                                        OTHERS       = 3 ).
        IF sy-subrc = 0.
          parameter_value_as_text-parameter_label = dfies-scrtext_l.
        ELSE.
          parameter_value_as_text-parameter_label = parameter_name.
        ENDIF.
      ELSE.
        parameter_value_as_text-parameter_label = substring( val = line_of_text_pool->entry
                                                             off = 8 ).
      ENDIF.
    ELSE.
      parameter_value_as_text-parameter_label = parameter_name.
    ENDIF.

    CASE line_of_sscr_load->kind.
      WHEN 'P'.
        INSERT |{ <parameter_value> }| INTO TABLE parameter_value_as_text-t_values_as_text.
      WHEN 'S'.
        DATA(number_of_include_lines) = 0.
        TRY.
            LOOP AT <select_options_value> ASSIGNING <line_of_select_options>
                 WHERE (`SIGN = 'I'`).
              INSERT LINES OF get_select_options_line_as_txt( range_line = <line_of_select_options> ) INTO TABLE parameter_value_as_text-t_values_as_text.
              number_of_include_lines = number_of_include_lines + 1.
            ENDLOOP.
            IF lines( <select_options_value> ) > number_of_include_lines.
              IF number_of_include_lines = 0.
                INSERT EXACT #( 'All except:'(001) ) INTO TABLE parameter_value_as_text-t_values_as_text.
              ELSE.
                INSERT EXACT #( 'Except:'(002) ) INTO TABLE parameter_value_as_text-t_values_as_text.
              ENDIF.
              LOOP AT <select_options_value> ASSIGNING <line_of_select_options>
                   WHERE (`SIGN = 'E'`).
                INSERT LINES OF get_select_options_line_as_txt( range_line = <line_of_select_options> ) INTO TABLE parameter_value_as_text-t_values_as_text.
              ENDLOOP.
            ENDIF.
          CATCH zcx_selection_screen_logger INTO DATA(error).
            RAISE EXCEPTION TYPE zcx_selection_screen_logger
              EXPORTING previous = error
                        text     = 'Error for parameter &1'(005)
                        msgv1    = EXACT #( parameter_name ).
        ENDTRY.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.

  METHOD get_parameter_values_as_table.
    DATA table_sscr_display TYPE tt_rsscr.
    DATA line_of_sscr_load  TYPE REF TO rsscr.

    " Get all Parameters and Select-Options except those defined as NO-DISPLAY.
    LOOP AT table_sscr REFERENCE INTO line_of_sscr_load
         WHERE    kind = 'P'
               OR kind = 'S'.
      GET BIT bit_sscr_f1_nodi OF line_of_sscr_load->flag1 INTO DATA(bit).
      IF bit = 1.
        CONTINUE.
      ENDIF.
      INSERT line_of_sscr_load->* INTO TABLE table_sscr_display.
    ENDLOOP.

    LOOP AT table_sscr_display REFERENCE INTO line_of_sscr_load.
      INSERT get_parameter_value_as_struct( line_of_sscr_load->name ) INTO TABLE parameter_values.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_parameter_values_as_text.
    TYPES:
      BEGIN OF ts_output,
        parameter_label TYPE c LENGTH 40,
        colon           TYPE c LENGTH 1,
        space           TYPE c LENGTH 1,
        parameter_value TYPE c length 1000,
      END OF ts_output.

    DATA parameter_value_as_text   TYPE string.
    DATA table_of_parameter_values TYPE zcl_selection_screen_logger=>tt_parameter_value.
    DATA parameter_value           TYPE REF TO zcl_selection_screen_logger=>ts_parameter_value.
    DATA parameter_line            TYPE ts_output.
    DATA value_as_text             TYPE REF TO string.

    table_of_parameter_values = get_parameter_values_as_table( ).
    LOOP AT table_of_parameter_values REFERENCE INTO parameter_value.
      parameter_line-parameter_label = parameter_value->parameter_label.
      parameter_line-colon           = ':'.

      IF parameter_value->t_values_as_text IS INITIAL.
        INSERT parameter_value_as_text
               INTO TABLE parameter_values_as_text.
      ELSE.
        parameter_line-parameter_value = parameter_value->t_values_as_text[ 1 ].
        parameter_value_as_text = parameter_line.
        INSERT parameter_value_as_text
               INTO TABLE parameter_values_as_text.

        LOOP AT parameter_value->t_values_as_text REFERENCE INTO value_as_text
             FROM 2.
          CLEAR parameter_line.
          parameter_line-parameter_value = value_as_text->*.
          parameter_value_as_text = parameter_line.
          INSERT parameter_value_as_text
                 INTO TABLE parameter_values_as_text.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_select_options_line_as_txt.
    DATA log_line TYPE string.

    ASSIGN COMPONENT 2 OF STRUCTURE range_line TO FIELD-SYMBOL(<option>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text  = 'Parameter RANGE_LINE misses the component number &1 (&2)'(008)
          msgv1 = '2'
          msgv2 = 'OPTION' ##NO_TEXT.
    ENDIF.

    ASSIGN COMPONENT 3 OF STRUCTURE range_line TO FIELD-SYMBOL(<low>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text  = 'Parameter RANGE_LINE misses the component number &1 (&2)'(008)
          msgv1 = '3'
          msgv2 = 'LOW' ##NO_TEXT.
    ENDIF.

    ASSIGN COMPONENT 4 OF STRUCTURE range_line TO FIELD-SYMBOL(<high>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_selection_screen_logger
        EXPORTING
          text  = 'Parameter RANGE_LINE misses the component number &1 (&2)'(008)
          msgv1 = '4'
          msgv2 = 'HIGH' ##NO_TEXT.
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
