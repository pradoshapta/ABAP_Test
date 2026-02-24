*&---------------------------------------------------------------------*
*& Report  ZSELECT_REMEDIATION
*&---------------------------------------------------------------------*
*& Purpose: Remediates SELECT * statements to use explicit fields
*&          This tool analyzes ABAP programs and suggests/implements
*&          remediation by replacing SELECT * with specific fields
*&---------------------------------------------------------------------*

REPORT zselect_remediation.

*----------------------------------------------------------------------
* Type Definitions
*----------------------------------------------------------------------
TYPES: BEGIN OF ty_program_analysis,
         program_name   TYPE program,
         line_number    TYPE i,
         statement      TYPE string,
         table_name     TYPE tabname,
         suggestion     TYPE string,
       END OF ty_program_analysis,
       
       BEGIN OF ty_remediation,
         old_select     TYPE string,
         new_select     TYPE string,
         program_name   TYPE program,
         status         TYPE string,
       END OF ty_remediation.

*----------------------------------------------------------------------
* Global Data
*----------------------------------------------------------------------
DATA: gt_programs        TYPE TABLE OF program,
      gt_analysis        TYPE TABLE OF ty_program_analysis,
      gt_remediation     TYPE TABLE OF ty_remediation,
      gv_program_name    TYPE program,
      gv_apply_changes   TYPE c VALUE space,
      go_prog_reader     TYPE REF TO object.

*----------------------------------------------------------------------
* Selection Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_prog TYPE program MATCHCODE OBJECT 'DYNP',
            p_pkg  TYPE devclass,
            p_all  TYPE c AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_analyze TYPE c AS CHECKBOX DEFAULT 'X',
            p_remedy  TYPE c AS CHECKBOX DEFAULT space,
            p_show    TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------
* Initialization
*----------------------------------------------------------------------
INITIALIZATION.
  TEXT-001 = 'Selection Parameters'.
  TEXT-002 = 'Processing Options'.

*----------------------------------------------------------------------
* Start of Selection
*----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM collect_programs.
  
  IF gt_programs IS NOT INITIAL.
    PERFORM analyze_programs.
    
    IF p_show = 'X'.
      PERFORM display_analysis.
    ENDIF.
    
    IF p_remedy = 'X' AND gt_analysis IS NOT INITIAL.
      PERFORM apply_remediation.
    ENDIF.
  ELSE.
    MESSAGE 'No programs found for selection' TYPE 'I'.
  ENDIF.

*----------------------------------------------------------------------
*& Form COLLECT_PROGRAMS
*----------------------------------------------------------------------
FORM collect_programs.
  
  IF p_all = 'X'.
    "Collect all programs from package
    SELECT DISTINCT obj_name INTO TABLE gt_programs
      FROM tadir
      WHERE object = 'PROG'
        AND devclass = p_pkg
        AND object_name LIKE '%Z%'.
  ELSE.
    "Single program selection
    APPEND p_prog TO gt_programs.
  ENDIF.
  
  SORT gt_programs.
  DELETE DUPLICATES FROM gt_programs.
  
ENDFORM. "collect_programs

*----------------------------------------------------------------------
*& Form ANALYZE_PROGRAMS
*----------------------------------------------------------------------
FORM analyze_programs.
  
  DATA: lt_source      TYPE TABLE OF abap_source_line,
        lv_line        TYPE i,
        lv_statement   TYPE string,
        lv_found       TYPE c,
        lv_table_name  TYPE tabname,
        ls_analysis    TYPE ty_program_analysis,
        lv_index       TYPE i,
        lv_select_pos  TYPE i,
        lv_from_pos    TYPE i,
        lv_where_pos   TYPE i.
  
  LOOP AT gt_programs INTO gv_program_name.
    
    "Read program source
    READ REPORT gv_program_name INTO lt_source.
    
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    
    lv_line = 0.
    
    LOOP AT lt_source INTO ls_analysis-statement.
      lv_line = lv_line + 1.
      ls_analysis-program_name = gv_program_name.
      ls_analysis-line_number = lv_line.
      
      "Check for SELECT *
      IF ls_analysis-statement CS 'SELECT *' OR
         ls_analysis-statement CS 'SELECT  *'.
        
        lv_found = 'X'.
        
        "Extract table name
        PERFORM extract_table_name
          USING ls_analysis-statement
          CHANGING lv_table_name.
        
        ls_analysis-table_name = lv_table_name.
        
        "Generate suggestion
        PERFORM generate_suggestion
          USING lv_table_name ls_analysis-statement
          CHANGING ls_analysis-suggestion.
        
        APPEND ls_analysis TO gt_analysis.
        
      ENDIF.
      
    ENDLOOP.
    
  ENDLOOP.
  
ENDFORM. "analyze_programs

*----------------------------------------------------------------------
*& Form EXTRACT_TABLE_NAME
*----------------------------------------------------------------------
FORM extract_table_name
  USING p_statement TYPE string
  CHANGING p_table_name TYPE tabname.
  
  DATA: lv_from_pos TYPE i,
        lv_where_pos TYPE i,
        lv_length   TYPE i,
        lv_temp     TYPE string.
  
  lv_from_pos = find( haystack = p_statement
                      needle   = 'FROM'
                      case     = abap_false ).
  
  IF lv_from_pos GT 0.
    lv_temp = p_statement+lv_from_pos+4.
    lv_where_pos = find( haystack = lv_temp
                         needle   = 'WHERE'
                         case     = abap_false ).
    
    IF lv_where_pos GT 0.
      lv_temp = lv_temp(lv_where_pos - 1).
    ENDIF.
    
    p_table_name = lv_temp.
    SHIFT p_table_name LEFT DELETING LEADING SPACES.
    
  ENDIF.
  
ENDFORM. "extract_table_name

*----------------------------------------------------------------------
*& Form GENERATE_SUGGESTION
*----------------------------------------------------------------------
FORM generate_suggestion
  USING p_table_name TYPE tabname
        p_statement TYPE string
  CHANGING p_suggestion TYPE string.
  
  DATA: lt_fields   TYPE TABLE OF dfies,
        ls_field    TYPE dfies,
        lv_fields   TYPE string,
        lv_first    TYPE c VALUE 'X'.
  
  "Get table fields from dictionary
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname         = p_table_name
      fieldname       = '*'
    TABLES
      dfies           = lt_fields
    EXCEPTIONS
      table_has_no_fields = 1
      table_not_found = 2
      OTHERS          = 3.
  
  IF sy-subrc = 0 AND lt_fields IS NOT INITIAL.
    
    LOOP AT lt_fields INTO ls_field.
      
      "Skip internal fields
      IF ls_field-fieldname CA '$_#'.
        CONTINUE.
      ENDIF.
      
      IF lv_first = 'X'.
        lv_fields = ls_field-fieldname.
        lv_first = space.
      ELSE.
        CONCATENATE lv_fields ',' ls_field-fieldname INTO lv_fields.
      ENDIF.
      
    ENDLOOP.
    
    CONCATENATE 'SELECT ' lv_fields ' FROM ' p_table_name
      INTO p_suggestion.
    
  ELSE.
    CONCATENATE 'SELECT * FROM ' p_table_name ' (Manual review needed)'
      INTO p_suggestion.
  ENDIF.
  
ENDFORM. "generate_suggestion

*----------------------------------------------------------------------
*& Form DISPLAY_ANALYSIS
*----------------------------------------------------------------------
FORM display_analysis.
  
  DATA: ls_analysis TYPE ty_program_analysis.
  
  WRITE: / 'SELECT * Remediation Analysis Report', / .
  WRITE: / sy-uline.
  WRITE: / 'Program'.
  WRITE: sy-vline.
  WRITE: 'Line'.
  WRITE: sy-vline.
  WRITE: 'Table'.
  WRITE: sy-vline.
  WRITE: 'Suggestion'.
  WRITE: / sy-uline.
  
  LOOP AT gt_analysis INTO ls_analysis.
    
    WRITE: / ls_analysis-program_name.
    WRITE: sy-vline NO-GAP.
    WRITE: ls_analysis-line_number.
    WRITE: sy-vline NO-GAP.
    WRITE: ls_analysis-table_name.
    WRITE: sy-vline NO-GAP.
    WRITE: ls_analysis-suggestion.
    
  ENDLOOP.
  
  WRITE: / sy-uline.
  SKIP 1.
  WRITE: 'Total SELECT * occurrences found: ', lines( gt_analysis).
  
ENDFORM. "display_analysis

*----------------------------------------------------------------------
*& Form APPLY_REMEDIATION
*----------------------------------------------------------------------
FORM apply_remediation.
  
  DATA: lv_response TYPE c.
  
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Apply Remediation'
      text_question         = 'Apply remediation to all programs?'
      text_button_1         = 'Yes'
      text_button_2         = 'No'
    IMPORTING
      answer                = lv_response.
  
  IF lv_response = '1'.
    
    MESSAGE 'Remediation logic would apply SELECT * fixes here' TYPE 'I'.
    
    " In production, you would:
    " 1. Create a request/task
    " 2. For each program in gt_analysis:
    "    - Generate new source code with explicit field lists
    "    - Create transport request
    "    - Activate the program
    
  ENDIF.
  
ENDFORM. "apply_remediation